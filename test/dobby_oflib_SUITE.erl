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

-define(SRC_EP, <<"Src">>).
-define(DST_EP, <<"Dst">>).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================


suite() ->
    [{timetrap,{minutes,10}}].

init_per_suite(Config) ->
    mock_dobby(),
    mock_flow_table_identifiers(),
    Config.

end_per_suite(_Config) ->
    unmock_dobby(),
    ok.

all() ->
    [should_publish_net_flow,
    should_publish_flow_path].

%%%=============================================================================
%%% Testcases
%%%=============================================================================

should_publish_net_flow(_Config) ->
    %% GIVEN
    FlowPath = dofl_test_utils:flow_path(),

    %% WHEN
    NetFlowId = dobby_oflib:publish_new_flow(?SRC_EP, ?DST_EP, FlowPath),

    %% THEN
    assert_net_flow_published(?SRC_EP, ?DST_EP, NetFlowId).

should_publish_flow_path(_Config) ->
    %% GIVEN
    FlowPath0 = dofl_test_utils:flow_path(),
    FlowPath1 = reconstruct_flow_path(FlowPath0),

    %% WHEN
    NetFlowId = dobby_oflib:publish_new_flow(?SRC_EP, ?DST_EP, FlowPath0),

    %% THEN
    assert_flow_path_published(NetFlowId, FlowPath1).

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
    assert_flow_path_published(NetFlowId, FlowPath, _PrevId = NetFlowId).

assert_flow_path_published(NetFlowId, [FlowMod | T], NetFlowId) ->
    LinkMd = link_metadata(of_path_starts_at, {NetFlowId, NetFlowId}),
    LinkMd2 = link_metadata(of_resource, bidirectional),
    FmNode = {FmId,_FmMD} = flow_mod_identifier(FlowMod),
    FtId = flow_table_identifier(FlowMod),
    ?assert(meck:called(dby, publish, [NetFlowId, FmNode, LinkMd, [persistent]])),
    ?assert(meck:called(dby, publish, [FmId, FtId, LinkMd2, [persistent]])),
    assert_flow_path_published(NetFlowId, T, FmId);
assert_flow_path_published(NetFlowId, [FlowMod | T], LastId) ->
    LinkMd = link_metadata(of_path_forwards_to, {NetFlowId, LastId}),
    LinkMd2 = link_metadata(of_resource, bidirectional),
    FmNode = {FmId,_FmMD} = flow_mod_identifier(FlowMod),
    FtId = flow_table_identifier(FlowMod),
    ?assert(meck:called(dby, publish, [LastId, FmNode, LinkMd, [persistent]])),
    ?assert(meck:called(dby, publish, [FmId, FtId, LinkMd2, [persistent]])),
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

mock_flow_table_identifiers() ->
    ok = meck:expect(dofl_identifier, flow_table,
                     fun(Dpid, _FlowMod = {_, _, Opts}) ->
                             TableNo = proplists:get_value(table_id, Opts),
                             TableNoBin = integer_to_binary(TableNo),
                             <<Dpid/binary, ":", TableNoBin/binary>>
                     end).

flow_mod_identifier({Dpid, OFVersion, _FlowTableId, FlowMod}) ->
    {_Matches, _Instructions, Opts} = FlowMod,
    Cookie = proplists:get_value(cookie, Opts),
    {Cookie, #{type => of_flow_mod, dpid => Dpid, of_version => OFVersion}}.

flow_table_identifier({Dpid, _OFVersion, TableNo, _FlowMod}) ->
    TableNoBin = integer_to_binary(TableNo),
    <<Dpid/binary, ":", TableNoBin/binary>>.

link_metadata(Type, {NetFlowId, Src}) ->
    #{type => Type, src => Src, net_flow_ids => [NetFlowId]};
link_metadata(Type, bidirectional) ->
    #{type => Type}.


reconstruct_flow_path(FlowPath0) ->
    Fun = fun({Dpid, {OFVersion, FlowMods}}) ->
                  [reconstruct_flow_mod(Dpid, OFVersion, FM) || FM <- FlowMods]
          end,
    FlowPath1 = lists:map(Fun, FlowPath0),
    lists:flatten(FlowPath1).

reconstruct_flow_mod(Dpid, OFVersion, FlowMod) ->
    TableNo = proplists:get_value(table_id, _Opts = element(3, FlowMod)),
    {Dpid, OFVersion, TableNo, FlowMod}.

trace_dby_publish() ->
    {module, M} = code:ensure_loaded(M = dby),
    ct:pal("Matched traces: ~p~n",
           [recon_trace:calls({dby, publish, '_'}, 10, [{pid, all}])]).
