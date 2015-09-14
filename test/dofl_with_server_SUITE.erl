%%%=============================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd
%%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%% @doc <Suite purpose>
%%% @end
%%%=============================================================================
-module(dofl_with_server_SUITE).
-copyright("2015, Erlang Solutions Ltd.").

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PUBLISHER_ID, <<"PUBLISHER">>).
-define(FM_COOKIE(X), <<0,0,0,0,0,0,0,X>>).
-define(OF_VERSION, 4).
-define(TYPE(V), #{<<"type">> := #{value := V}}).
-define(FLOW_MOD_MD, [<<"type">>, <<"of_version">>, <<"dpid">>]).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================


suite() ->
    [{timetrap,{minutes,10}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(dobby_oflib),
    lager:set_loglevel(lager_console_backend, none),
    Config1 = add_topo_filename(Config),
    add_topo_identifiers(Config1).

end_per_suite(_Config) ->
    application:stop(dobby),
    application:stop(dobby_oflib).

all() ->
    [it_publishes_flow_mod,
     it_publishes_net_flow,
     it_finds_flow_table_identifers,
     it_returns_path_with_flow_mods].


%%%=============================================================================
%%% Testcases
%%%=============================================================================

it_publishes_flow_mod(Config) ->
    %% GIVEN
    setup_dobby(Config),
    [DpId, _] = ?config(datapath_ids, Config),
    DpFlowMod = dp_flow_mod(DpId, ?FM_COOKIE(10)),

    %% WHEN
    {ok, PublishedFmId} = dobby_oflib:publish_dp_flow_mod(?PUBLISHER_ID,
                                                          DpFlowMod),

    %% THEN
    assert_flow_mod_identifier(DpId, PublishedFmId).

it_publishes_net_flow(Config) ->
    %% GIVEN
    setup_dobby(Config),
    [Ep1, Ep2] = ?config(endpoints, Config),
    FlowModsIds = publish_flow_mods(?PUBLISHER_ID, Config),

    %% WHEN
    {ok, NetFlowId} =
        dobby_oflib:publish_net_flow(?PUBLISHER_ID, Ep1, Ep2, FlowModsIds),

    %% THEN
    assert_net_flow_identifier(Ep1, Ep2, NetFlowId),
    assert_flow_path(NetFlowId, FlowModsIds).

it_finds_flow_table_identifers(Config) ->
    %% GIVEN
    [Dpid | _] = ?config(datapath_ids, Config),
    DpidToFt = ?config(dpid_to_flow_table, Config),
    {FtId, FtNo} = maps:get(Dpid, DpidToFt),

    %% WHEN
    Id = dofl_identifier:flow_table(Dpid, flow_mod(FtNo)),

    %% THEN
    ?assertEqual(FtId, Id).

it_returns_path_with_flow_mods(Config) ->
    %% GIVEN
    setup_dobby(Config),
    [Ep1, Ep2] = ?config(endpoints, Config),
    FlowModsIds = publish_flow_mods(?PUBLISHER_ID, Config),

    %% WHEN
    {ok, G} = dobby_oflib:get_path(Ep1, Ep2),

    %% THEN
    assert_digraph_has_flow_mods(G, FlowModsIds, Config).

%%%=============================================================================
%%% Assertions
%%%=============================================================================

assert_digraph_has_flow_mods(Digraph, FlowModsIds, Config) ->
    SwitchesIds = ?config(datapath_ids, Config),
    [begin
         EachFlowModConnectedFun =
             fun(FlowModId) ->
                     assert_path_exists(Digraph, SwId, FlowModId)
             end,
         lists:foreach(EachFlowModConnectedFun, FlowModsIds)
     end || SwId <- SwitchesIds].

assert_path_exists(Digraph, Src, Dst) ->
    ?assert(false /= digraph:get_path(Digraph, Src, Dst)).

assert_flow_mod_identifier(DpId, PublishedFmId) ->
    {SearchedFmId, SearchedFmMd} = dby:search(mk_flow_mod_search_fun(),
                                              [], DpId,
                                              [breadth, {max_depth, 2}]),
    ?assertEqual(PublishedFmId, SearchedFmId),
    [?assertMatch({ok, _}, maps:find(K, SearchedFmMd)) || K <- ?FLOW_MOD_MD].

assert_net_flow_identifier(Src, Dst, NetFlowId) ->
    Expected = [Src, NetFlowId, Dst],
    Actual = dby:search(mk_net_flow_fun(Dst), [], Src,
                        [depth, {max_depth, 10}, {loop, link}]),
    ?assertEqual(Expected, Actual).

assert_flow_path(NetFlowId, FlowModsIds) ->
    Expected = lists:flatten([NetFlowId, FlowModsIds, NetFlowId]),
    Actual = dby:search(mk_flow_path_fun(NetFlowId), [], NetFlowId,
                        [depth, {max_depth, 10}, {loop, link}]),
    ?assertEqual(Expected, Actual).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

setup_dobby(Config) ->
    application:stop(dobby),
    application:load(erl_mnesia),
    application:set_env(erl_mnesia, options, [persistent]),
    {ok, _} = application:ensure_all_started(dobby),
    ok = dby_bulk:import(json0, ?config(add_topo_filename, Config)).

add_topo_identifiers(Config) ->
    [{endpoints, [<<"EP1">>, <<"EP2">>]},
     {datapath_ids, [<<"00:00:00:00:00:01:00:01">>,
                     <<"00:00:00:00:00:01:00:02">>]},
     {dpid_to_flow_table,
      #{<<"00:00:00:00:00:01:00:01">> => {<<"OFS1-table-0">>,0}}} | Config].

add_topo_filename(Config) ->
    [{add_topo_filename, ?config(data_dir, Config) ++ "daisychain-3.json"}
     | Config].

publish_endpoints(PubId, Src, Dst) ->
    [dby:publish(PubId, {EP, [{<<"type">>, <<"endpoint">>}]}, [persistent])
     || EP <- [Src, Dst]].

dp_flow_mod(Dpid, FmCookie) ->
    {Dpid, ?OF_VERSION, dofl_test_utils:flow_mod(FmCookie)}.

publish_flow_mods(PubId, Config) ->
    [DpId1, DpId2] = ?config(datapath_ids, Config),
    DpFlowMods = [dp_flow_mod(DpId1, ?FM_COOKIE(10)),
                  dp_flow_mod(DpId2, ?FM_COOKIE(20))],
    [begin
         {ok, FmId} = dobby_oflib:publish_dp_flow_mod(PubId, DpFm),
         FmId
     end || DpFm <- DpFlowMods].

publish_net_flow(PubId, Src, Dst, FlowModsIds) ->
    {ok, NfId} = dobby_oflib:publish_net_flow(PubId, Src, Dst, FlowModsIds),
    NfId.

flow_mod(FtNo) ->
    {_Matches = [], _Actions = [], [{table_id, FtNo}]}.

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

mk_flow_mod_search_fun() ->
    fun(_, _, [], Acc) ->
            {continue, Acc};
       (_, ?TYPE(<<"of_flow_table">>), _, Acc) ->
            {continue, Acc};
       (FlowModId, ?TYPE(<<"of_flow_mod">>) = IdMetadataInfo, _,  _Acc) ->
            {stop, {FlowModId, IdMetadataInfo}};
       (_, _, _, Acc) ->
            {skip, Acc}
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
