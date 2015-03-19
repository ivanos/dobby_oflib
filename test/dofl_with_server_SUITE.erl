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
    start_applications(),
    case is_dobby_server_running() of
        false ->
            ct:pal(Reason = "Dobby server is not running"),
            {skip, Reason};
        true ->
            Config
    end.

init_per_testcase(Case, Config) when
      Case =:= should_find_flow_table_identifers;
      Case =:= should_return_path_with_flow_mods ->
    TopFilename = ?config(data_dir, Config) ++ "daisychain-3.json",
    dby_bulk:import(json0, TopFilename),
    daisychain_identifiers() ++  Config;
init_per_testcase(_, Config) ->
    mock_flow_table_identifiers(),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(),
    Config.

all() ->
    [should_return_path_with_flow_mods].
    %% [should_publish_net_flow,
    %%  should_find_flow_table_identifers,
%%  should_publish_flow_path].

daisychain_identifiers() ->
    [{endpoints, {<<"EP1">>, <<"EP2">>}},
     {of_switches, [<<"00:00:00:00:00:01:00:01">>,
                    <<"00:00:00:00:00:01:00:02">>]}].

%%%=============================================================================
%%% Testcases
%%%=============================================================================

should_publish_net_flow(_Config) ->
    %% GIVEN
    FlowPath = dofl_test_utils:flow_path(),
    publish_endpoints(),

    %% WHEN
    {ok, NetFlowId} = dobby_oflib:publish_new_flow(?PUBLISHER_ID, ?SRC_EP, ?DST_EP, FlowPath),
    Expected = [?SRC_EP, NetFlowId, ?DST_EP],

    %% %% THEN
    Fun = mk_net_flow_fun(?DST_EP),
    Actual = dby:search(Fun, [], ?SRC_EP, [depth, {max_depth, 10}, {loop, link}]),
    ?assertEqual(Expected, Actual).

should_publish_flow_path(_Config) ->
    %% GIVEN
    FlowPath = dofl_test_utils:flow_path(),
    FlowPathIds = dofl_test_utils:flow_path_to_identifiers(FlowPath),
    publish_endpoints(),

    %% WHEN
    {ok, NetFlowId} = dobby_oflib:publish_new_flow(?PUBLISHER_ID, ?SRC_EP, ?DST_EP, FlowPath),
    Expected = lists:flatten([NetFlowId, FlowPathIds, NetFlowId]),

    %% %% THEN
    Fun = mk_flow_path_fun(NetFlowId),
    Actual = dby:search(Fun, [], NetFlowId, [breadth, {max_depth, 10}, {loop, link}]),
    ?assertEqual(Expected, Actual).

should_find_flow_table_identifers(_Config) ->
    %% GIVEN
    Dpid = <<"00:00:00:00:00:01:00:01">>,
    FlowMod = {_Matches = [], _Actions = [], [{table_id, 0}]},

    %% WHEN
    Id = dofl_identifier:flow_table(Dpid, FlowMod),

    %% THEN
    ?assertEqual(<<"OFS1-table-0">>, Id).

should_return_path_with_flow_mods(Config) ->
    %% GIVEN
    {Ep1, Ep2} = ?config(endpoints, Config),
    FlowPath = dofl_test_utils:flow_path(),
    publish_net_flow(Ep1, Ep2, FlowPath),

    %% WHEN
    {ok, G} = dobby_oflib:get_path(Ep1, Ep2),

    %% THEN
    assert_digraph_has_flow_mods(G, FlowPath, Config).

%%%=============================================================================
%%% Assertions
%%%=============================================================================

assert_digraph_has_flow_mods(Digraph, FlowPath, Config) ->
    SwitchesIds = ?config(of_switches, Config),
    [begin
         {_OFVersion, FlowMods} = proplists:get_value(SwId, FlowPath),
         EachFlowModConnectedFun =
             fun(FlowMod) ->
                     {_Matches, _Instructions, Opts} = FlowMod,
                     FlowModId = proplists:get_value(cookie, Opts),
                     assert_path_exists(Digraph, SwId, FlowModId)
             end,
         lists:foreach(EachFlowModConnectedFun, FlowMods)
     end || SwId <- SwitchesIds].

assert_path_exists(Digraph, Src, Dst) ->
    ?assert(false /= digraph:get_path(Digraph, Src, Dst)).

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

publish_net_flow(Src, Dst, FlowPath) ->
    dobby_oflib:publish_new_flow(?PUBLISHER_ID, Src, Dst, FlowPath).

mk_net_flow_fun(DstEndpoint) ->
    fun(Identifier, _IdMetadataInfo, [], _) ->
            {continue, {net_flow_next_trasitions(init), [Identifier]}};
       (Identifier, IdMetadataInfo, [PrevPathElement | _], Acc) ->
            {NextTs, IdAcc} = Acc,
            T = transition(PrevPathElement, IdMetadataInfo),
            case transition_allowed(T, NextTs) of
                true when Identifier =:= DstEndpoint ->
                    {stop, lists:reverse([Identifier | IdAcc])};
                true ->
                    {continue, {net_flow_next_trasitions(IdMetadataInfo),
                                [Identifier | IdAcc]}};
                false ->
                    {skip, Acc}
            end
    end.

mk_flow_path_fun(NetFlowId) ->
    fun(Identifier, _IdMetadataInfo, [], _) ->
            {continue, {flow_path_next_trasitions(init), [Identifier]}};
       (Identifier, IdMetadataInfo, [PrevPathElement | _], Acc) ->
            {NextTs, IdAcc} = Acc,
            T = transition(PrevPathElement, IdMetadataInfo),
            case transition_allowed(T, NextTs) of
                true when Identifier =:= NetFlowId ->
                    {stop, lists:reverse([Identifier | IdAcc])};
                true ->
                    {continue, {flow_path_next_trasitions(IdMetadataInfo),
                                [Identifier | IdAcc]}};
                false ->
                    {skip, Acc}
            end
    end.

net_flow_next_trasitions(init) ->
    [{ep_to_nf, of_net_flow}];
net_flow_next_trasitions(#{<<"type">> := IdType}) ->
    net_flow_next_trasitions(binary_to_atom(maps:get(value, IdType), utf8));
net_flow_next_trasitions(of_net_flow) ->
    [{ep_to_nf, endpoint}].

flow_path_next_trasitions(init) ->
    [{LinkT, of_flow_mod} || LinkT <- [of_path_starts_at, of_path_ends_at]];
flow_path_next_trasitions(#{<<"type">> := IdType}) ->
    flow_path_next_trasitions(binary_to_atom(maps:get(value, IdType), utf8));
flow_path_next_trasitions(of_flow_mod) ->
    [{of_path_forwards_to, of_flow_mod} |
     [{LinkT, of_net_flow} || LinkT <- [of_path_starts_at, of_path_ends_at]]].

transition_allowed(T, AllowedTs) ->
    lists:member(T, AllowedTs).

transition({_, _, #{<<"type">> := LinkType}}, #{<<"type">> := IdType}) ->
    F = fun(T) -> binary_to_atom(maps:get(value, T), utf8) end,
    {F(LinkType), F(IdType)};
transition(_, _) ->
    unknown.

trace_dby_publish() ->
    {module, M} = code:ensure_loaded(M = dby),
    ct:pal("Matched traces: ~p~n",
           [recon_trace:calls({dby, publish, '_'}, 20, [{pid, all}])]).
