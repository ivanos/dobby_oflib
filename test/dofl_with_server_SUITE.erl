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

-define(SRC_EP, <<"SRC">>).
-define(DST_EP, <<"DST">>).
-define(PUBLISHER_ID, <<"PUBLISHER">>).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================


suite() ->
    [{timetrap,{minutes,10}}].

init_per_suite(Config) ->
    mock_flow_table_identifiers(),
    start_applications(),
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
    FlowPathIds = dofl_test_utils:flow_path_to_identifiers(FlowPath),
    publish_endpoints(),

    %% WHEN
    {ok, NetFlowId} = dobby_oflib:publish_new_flow(?PUBLISHER_ID, ?SRC_EP, ?DST_EP, FlowPath),
    Expected = lists:flatten(
                 [?SRC_EP, NetFlowId, FlowPathIds, NetFlowId, ?DST_EP]),

    %% %% THEN
    Fun = mk_net_flow_with_flow_path_fun(?DST_EP),
    Actual = dby:search(Fun, [], ?SRC_EP, [depth, {max_depth, 10}, {loop, link}]),
    ?assertEqual(Expected, Actual).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

start_applications() ->
    application:ensure_all_started(dobby),
    application:ensure_all_started(dobby_oflib).

is_dobby_server_running() ->
    proplists:is_defined(dobby, application:which_applications()).

mock_flow_table_identifiers() ->
    ok = meck:expect(dofl_identifier, flow_table,
                     fun(Dpid, _FlowMod = {_, _, Opts}) ->
                             TableNo = proplists:get_value(table_id, Opts),
                             TableNoBin = integer_to_binary(TableNo),
                             <<Dpid/binary, ":", TableNoBin/binary>>
                     end).

publish_endpoints() ->
    [dby:publish(
       ?PUBLISHER_ID, {EP, [{<<"type">>, <<"endpoint">>}]}, [persistent])
     || EP <- [?SRC_EP, ?DST_EP]].

mk_net_flow_with_flow_path_fun(DstEndpoint) ->
    fun(Identifier, _IdMetadataInfo, [], _) ->
            {continue, [allowed_transitions(init, []), [Identifier]]};
       (Identifier, IdMetadataInfo, [{_, PrevIdMetadataInfo, _} | _], Acc) ->
            [AllowedT, IdentifiersAcc] = Acc,
            T = transition(PrevIdMetadataInfo, IdMetadataInfo),
            case is_transition_allowed(T, AllowedT) of
                false ->
                    {skip, Acc};
                true when Identifier == DstEndpoint ->
                    {stop, [Identifier | IdentifiersAcc]};
                true ->
                    NewAllowedT = allowed_transitions(T, AllowedT),
                    {continue, [NewAllowedT, [Identifier | IdentifiersAcc]]}
            end
    end.

transition(PrevIdMetadataInfo, IdMetadataInfo) ->
    [PrevT, T] =  [begin
                       TypeMap = maps:get(<<"type">>, MetadataInfo),
                       maps:get(<<"value">>, TypeMap)
                   end || MetadataInfo <- [PrevIdMetadataInfo, IdMetadataInfo]],
    {binary_to_atom(PrevT, utf8), binary_to_atom(T, utf8)}.

is_transition_allowed(Transition, AllowedTransitions) ->
    lists:member(Transition, AllowedTransitions).

allowed_transitions({endpoint, of_net_flow}, _CurrentAllowedT) ->
    [{of_net_flow, of_flow_mod},
     {of_flow_mod, of_flow_mod},
     {of_flow_mod, of_net_flow}];
allowed_transitions({of_flow_mod, net_flow}, _CurrentAllowedT) ->
    [{net_flow, endpoint}];
allowed_transitions(init, _CurrentAllowedT) ->
    [{endpoint, of_net_flow}];
allowed_transitions(_, CurrentAllowedT) ->
    CurrentAllowedT.

trace_dby_publish() ->
    {module, M} = code:ensure_loaded(M = dby),
    ct:pal("Matched traces: ~p~n",
           [recon_trace:calls({dby, publish, '_'}, 10, [{pid, all}])]).
