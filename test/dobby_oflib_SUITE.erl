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
-define(PUBLISHER_ID, <<"Publisher">>).

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
    unmock_flow_table_identifiers(),
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
    {ok, NetFlowId} = dobby_oflib:publish_new_flow(?PUBLISHER_ID,
                                                   ?SRC_EP,
                                                   ?DST_EP,
                                                   FlowPath),

    %% THEN
    assert_net_flow_published(?PUBLISHER_ID, ?SRC_EP, ?DST_EP, NetFlowId).

should_publish_flow_path(_Config) ->
    %% GIVEN
    FlowPath0 = dofl_test_utils:flow_path(),
    FlowPath1 = reconstruct_flow_path(FlowPath0),

    %% WHEN
    {ok, NetFlowId} = dobby_oflib:publish_new_flow(?PUBLISHER_ID,
                                                   ?SRC_EP,
                                                   ?DST_EP,
                                                   FlowPath0),

    %% THEN
    assert_flow_path_published(?PUBLISHER_ID, NetFlowId, FlowPath1).

%%%=============================================================================
%%% Assertions
%%%=============================================================================

assert_net_flow_published(PublisherId, SrcEP, DstEP, NetFlowId) ->
    [assert_dby_publish_called([PublisherId, Src, Dst,
                                link_metadata({ep_to_nf, Src})])
     || {Src, Dst} <- [{SrcEP, net_flow_identifier(NetFlowId)},
                       {NetFlowId, DstEP}]].

assert_dby_publish_called(Args) ->
    ?assert(meck:called(dby, publish, Args ++ [[persistent]])).

assert_flow_path_published(PublisherId, NetFlowId, FlowPath) ->
    assert_flow_path_published(PublisherId, NetFlowId, FlowPath,
                               _PrevId = NetFlowId).

assert_flow_path_published(PublisherId, NetFlowId, [FlowMod | T], NetFlowId) ->
    OFPathArgs = [NetFlowId,
                  {FmId, _FmMd} = flow_mod_identifier(FlowMod),
                  link_metadata({of_path_starts_at, NetFlowId, NetFlowId})],
    OFResourceArgs = [FmId,
                      flow_table_identifier(FlowMod),
                      link_metadata(of_resource)],
    [assert_dby_publish_called([PublisherId | Args])
     || Args <- [OFPathArgs, OFResourceArgs]],
    assert_flow_path_published(PublisherId, NetFlowId, T, FmId);
assert_flow_path_published(PublisherId, NetFlowId, [FlowMod | T], LastId) ->
    OFPathArgs = [LastId,
                  {FmId,_FmMd} = flow_mod_identifier(FlowMod),
                  link_metadata({of_path_forwards_to, LastId, NetFlowId})],
    OFResourceArgs = [FmId,
                      flow_table_identifier(FlowMod),
                      link_metadata(of_resource)],
    [assert_dby_publish_called([PublisherId | Args])
     || Args <- [OFPathArgs, OFResourceArgs]],
    assert_flow_path_published(PublisherId, NetFlowId, T, FmId);
assert_flow_path_published(PublisherId, NetFlowId, [], LastId) ->
    OFPathArgs = [LastId,
                  NetFlowId,
                  link_metadata({of_path_ends_at, LastId, NetFlowId})],
    assert_dby_publish_called([PublisherId | OFPathArgs]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

mock_dobby() ->
    ok = meck:expect(dby, publish, 5, _Ret = ok).

unmock_dobby() ->
    ok = meck:unload(dby).

mock_flow_table_identifiers() ->
    ok = meck:expect(dofl_identifier, flow_table,
                     fun(Dpid, _FlowMod = {_, _, Opts}) ->
                             TableNo = proplists:get_value(table_id, Opts),
                             TableNoBin = integer_to_binary(TableNo),
                             <<Dpid/binary, ":", TableNoBin/binary>>
                     end).

unmock_flow_table_identifiers() ->
    ok = meck:unload(dofl_identifier).

flow_mod_identifier({Dpid, OFVersion, _FlowTableId, FlowMod}) ->
    {_Matches, _Instructions, Opts} = FlowMod,
    Cookie = proplists:get_value(cookie, Opts),
    {Cookie, [{<<"type">>, <<"of_flow_mod">>},
              {<<"dpid">>, Dpid},
              {<<"of_version">>, OFVersion}]}.

flow_table_identifier({Dpid, _OFVersion, TableNo, _FlowMod}) ->
    TableNoBin = integer_to_binary(TableNo),
    <<Dpid/binary, ":", TableNoBin/binary>>.

net_flow_identifier(NetFlowId) ->
    {NetFlowId, [{<<"type">>, <<"of_net_flow">>}]}.

link_metadata({Type, Src}) ->
    [{<<"type">>, atom_to_binary(Type, utf8)},
     {<<"src">>, Src}];
link_metadata({Type, Src, NetFlowId}) ->
    [{<<"type">>, atom_to_binary(Type, utf8)},
     {<<"src">>, Src},
     {<<"net_flow_ids">>, [NetFlowId]}];
link_metadata(Type) ->
    [{<<"type">>, atom_to_binary(Type, utf8)}].

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
