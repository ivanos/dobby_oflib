%%%=============================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd
%%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%% @doc <Suite purpose>
%%% @end
%%%=============================================================================
-module(dofl_with_server_SUITE).
-copyright("2015, Erlang Solutions Ltd.").

%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Callbacks
%%%=============================================================================


suite() ->
    [{timetrap,{minutes,10}}].

init_per_suite(Config) ->
    application:ensure_all_started(dobby),
    case is_dobby_server_running() of
        false ->
            ct:pal(Reason = "Dobby server is not running"),
            {skip, Reason};
        true ->
            Config
    end.

all() ->
    [should_publish_net_flow].

%%%=============================================================================
%%% Testcases
%%%=============================================================================

should_publish_net_flow(_Config) ->
    %% GIVEN
    FlowPath = dofl_test_utils:flow_path(),
    SrcEP = <<"EP1">>,
    DstEP = <<"EP2">>,
    FlowPathIds = dofl_test_utils:flow_path_to_identifiers(FlowPath),
    %% It would be better to have transitions expressed via nodes' types
    AllowedTransitions = [{ep_to_nf, of_path_starts_at},
                          {of_path_starts_at, of_path_ends_at},
                          {of_path_starts_at, of_path_forwards_to},
                          {of_path_forwards_to, of_path_forwards_to},
                          {of_path_forwards_to, of_path_ends_at},
                          {of_path_ends_at, ep_to_nf}],

    %% WHEN
    NetFlowId = dobby_oflib:publish_new_flow(SrcEP, DstEP, FlowPath),
    Expected = lists:flatten([SrcEP, NetFlowId, FlowPathIds, NetFlowId, DstEP]),

    %% THEN
    Fun = mk_net_flow_with_flow_path_fun(DstEP, AllowedTransitions),
    Actual = dby:search(Fun, [], SrcEP, [depth, {max_depth, 10}]),
    ?assertEqual(Expected, Actual).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

is_dobby_server_running() ->
    proplists:is_defined(dobby, application:which_applications()).

mk_net_flow_with_flow_path_fun(DstEndpoint, AllowedTransitions) ->
    fun(Identifier, IdMetadata, LinkMetadata, Acc = [Prev | _])
          when length(Acc) >= 2 ->
            T = transition(Prev, LinkMetadata),
            case is_transition_allowed(T, AllowedTransitions) of
                false ->
                    {skip, Acc};
                true when Identifier == DstEndpoint ->
                    Result = [{Identifier, IdMetadata, LinkMetadata} | Acc],
                    {stop, filter_out_metadata(Result)};
                true ->
                    {continue, [{Identifier, IdMetadata, LinkMetadata} | Acc]}
            end;
       (Identifier, IdMetadata, LinkMetadata, Acc) ->
            {continue, [{Identifier, IdMetadata, LinkMetadata} | Acc]}
    end.

transition(PrevData, LinkMetadata) ->
    {_PrevIdentifier, _PrevIdMetadata, PrevLinkMetadata} = PrevData,
    {maps:get(type, PrevLinkMetadata), maps:get(type, LinkMetadata)}.

is_transition_allowed(Transition, AllowedTransitions) ->
    lists:member(Transition, AllowedTransitions).

filter_out_metadata(SearchResult) ->
    lists:map(fun({Identifier, _, _}) ->
                      Identifier
              end, SearchResult).

trace_dby_publish() ->
    {module, M} = code:ensure_loaded(M = dby),
    ct:pal("Matched traces: ~p~n",
           [recon_trace:calls({dby, publish, '_'}, 10, [{pid, all}])]).
