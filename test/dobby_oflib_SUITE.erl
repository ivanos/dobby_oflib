%%%=============================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd
%%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%% @doc <Suite purpose>
%%% @end
%%%=============================================================================
-module(dobby_oflib_SUITE).
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
    mock_dobby(),
    Config.

end_per_suite(_Config) ->
    unmock_dobby(),
    ok.

all() ->
    [should_publish_net_flow].

%%%=============================================================================
%%% Testcases
%%%=============================================================================

should_publish_net_flow(_Config) ->
    %% GIVEN
    FlowPath = dofl_test_utils:flow_path(),
    SrcEP = <<"Src">>,
    DstEP = <<"Dst">>,

    %% WHEN
    NetFlowId = dobby_oflib:publish_new_flow(SrcEP, DstEP, FlowPath),

    %% THEN
    assert_net_flow_published(SrcEP, DstEP, NetFlowId),
    assert_flow_path_published(NetFlowId, FlowPath).

%%%=============================================================================
%%% Assertions
%%%=============================================================================

assert_net_flow_published(SrcEP, DstEP, NetFlowId) ->
    ?assert(meck:called(dby, publish, [SrcEP, {NetFlowId, #{type => of_net_flow}},
                                       #{type => ep_to_nf, src => SrcEP},
                                       [persistent]])),
    ?assert(meck:called(dby, publish, [NetFlowId,
                                       DstEP,
                                       #{type => ep_to_nf, src => NetFlowId},
                                       [persistent]])).


assert_flow_path_published(NetFlowId, FlowPath) ->
    FlatFlowPath = flatten_flow_path(FlowPath),
    assert_flow_path_published(NetFlowId, FlatFlowPath,
                               _PrevIdentifier = NetFlowId).

assert_flow_path_published(NetFlowId, [FlowMod | T], NetFlowId) ->
    LinkMd = link_metadata(of_path_starts_at, {NetFlowId, NetFlowId}),
    FmNode = {FmId,_FmMD} = flow_mod_identifier(FlowMod),
    ?assert(meck:called(dby, publish, [NetFlowId, FmNode, LinkMd, [persistent]])),
    assert_flow_path_published(NetFlowId, T, FmId);
assert_flow_path_published(NetFlowId, [FlowMod | T], LastId) ->
    LinkMd = link_metadata(of_path_forwards_to, {NetFlowId, LastId}),
    FmNode = {FmId,_FmMD} = flow_mod_identifier(FlowMod),
    ?assert(meck:called(dby, publish, [LastId, FmNode, LinkMd, [persistent]])),
    assert_flow_path_published(NetFlowId, T, FmId);
assert_flow_path_published(NetFlowId, [], LastId) ->
    LinkMd = link_metadata(of_path_ends_at, {NetFlowId, LastId}),
    ?assert(meck:called(dby, publish, [LastId, NetFlowId, LinkMd, [persistent]])).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

mock_dobby() ->
    ok = meck:expect(dby, publish, 4, _Ret = ok).

unmock_dobby() ->
    ok = meck:unload(dby).

flow_mod_identifier({Dpid, OFVersion, FlowMod}) ->
    {_Matches, _Instructions, Opts} = FlowMod,
    Cookie = proplists:get_value(cookie, Opts),
    {Cookie, #{type => of_flow_mod, dpid => Dpid, of_version => OFVersion}}.

link_metadata(Type, {NetFlowId, Src}) ->
    #{type => Type, src => Src, net_flow_ids => [NetFlowId]}.

flatten_flow_path(FlowPath0) ->
    Fun = fun({Dpid, {OFVersion, FlowMods}}) ->
                  [{Dpid, OFVersion, FlowMod} || FlowMod <- FlowMods]
          end,
    FlowPath1 = lists:map(Fun, FlowPath0),
    lists:flatten(FlowPath1).




