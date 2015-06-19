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
-define(DPID, <<"00:00:00:00:00:01:00:01">>).
-define(OF_VERSION, 4).
-define(FM_COOKIE, <<0,0,0,0,0,0,0,10>>).
-define(FM_COOKIE2, <<0,0,0,0,0,0,0,12>>).

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
    unmock_flow_table_identifiers(),
    unmock_dobby(),
    ok.

all() ->
    [it_publishes_net_flow,
     it_publishes_flow_mod].

%%%=============================================================================
%%% Testcases
%%%=============================================================================

it_publishes_net_flow(_Config) ->
    %% GIVEN
    FlowModsIds = [?FM_COOKIE, ?FM_COOKIE2],

    %% WHEN
    {ok, NetFlowId} = dobby_oflib:publish_net_flow(?PUBLISHER_ID,
                                                   ?SRC_EP,
                                                   ?DST_EP,
                                                   FlowModsIds),

    %% THEN
    assert_net_flow_published(?PUBLISHER_ID, ?SRC_EP, ?DST_EP, NetFlowId),
    assert_flow_path_published(?PUBLISHER_ID, NetFlowId, FlowModsIds).

it_publishes_flow_mod(_Config) ->
    %% GIVEN
    DpFlowMod = {?DPID, ?OF_VERSION, dofl_test_utils:flow_mod(?FM_COOKIE)},

    %% WHEN
    {ok, FlowModId} = dobby_oflib:publish_dp_flow_mod(?PUBLISHER_ID, DpFlowMod),

    %% THEN
    assert_flow_mod_published(?PUBLISHER_ID, FlowModId, DpFlowMod).

%%%=============================================================================
%%% Assertions
%%%=============================================================================

assert_flow_mod_published(PublisherId, PublishedFMId, DpFlowMod) ->
    OFResourceArgs = [flow_table_identifier(DpFlowMod),
                      {ExpectedFMId, _} = flow_mod_identifier(DpFlowMod),
                      link_metadata(of_resource)],
    ?assertEqual(ExpectedFMId, PublishedFMId),
    assert_dby_publish_called([PublisherId | OFResourceArgs]).

assert_net_flow_published(PublisherId, SrcEP, DstEP, NetFlowId) ->
    [assert_dby_publish_called([PublisherId, Src, Dst,
                                link_metadata({ep_to_nf, Src})])
     || {Src, Dst} <- [{SrcEP, net_flow_identifier(NetFlowId)},
                       {NetFlowId, DstEP}]].

assert_flow_path_published(PubId, NetFlowId, FlowModsIds) ->
    Fun = fun(FmId, PrevId) ->
                  LinkType = flow_path_link_type(PrevId, NetFlowId),
                  LinkMd = link_metadata({LinkType, PrevId, NetFlowId}),
                  assert_dby_publish_called([PubId, PrevId, FmId, LinkMd]),
                  FmId
          end,
    LastFmId = lists:foldl(Fun, NetFlowId, FlowModsIds),
    LinkMd = link_metadata({of_path_ends_at, LastFmId, NetFlowId}),
    assert_dby_publish_called([PubId, LastFmId, NetFlowId, LinkMd]).

assert_dby_publish_called(Args) ->
    ?assert(meck:called(dby, publish, Args ++ [[persistent]])
            %% If the function wasn't called with the expected
            %% arguments, ensure that the test output contains the
            %% arguments and all calls to functions in dby.
            orelse {Args, meck:history(dby)}).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

mock_dobby() ->
    ok = meck:expect(dby, publish, 5, _Ret = ok).

unmock_dobby() ->
    ok = meck:unload(dby).

mock_flow_table_identifiers() ->
    ok = meck:expect(dofl_identifier, flow_table,
                     fun(Dpid, FlowMod) ->
                             flow_table_identifier({Dpid, 4, FlowMod})
                     end).

unmock_flow_table_identifiers() ->
    ok = meck:unload(dofl_identifier).

flow_mod_identifier({Dpid, OFVersion, FlowMod}) ->
    {_Matches, _Instructions, Opts} = FlowMod,
    Cookie = proplists:get_value(cookie, Opts),
    JSONFriendlyCookie = iolist_to_binary(io_lib:format("~w", [Cookie])),
    {JSONFriendlyCookie,
     [{<<"type">>, <<"of_flow_mod">>},
      {<<"dpid">>, Dpid},
      {<<"of_version">>, OFVersion}
      %% We could check the tail of the list, but that would amount to
      %% duplicating the code in dofl_identifier in this test.
      | '_']}.

flow_table_identifier({Dpid, _OFVersion, FlowMod}) ->
    TableNo = proplists:get_value(table_id, _Opts = element(3, FlowMod)),
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

trace_dby_publish() ->
    {module, M} = code:ensure_loaded(M = dby),
    ct:pal("Matched traces: ~p~n",
           [recon_trace:calls({dby, publish, '_'}, 10, [{pid, all}])]).

flow_path_link_type(PrevId, NetFlowId) ->
    case PrevId of
        NetFlowId ->
            of_path_starts_at;
        _ ->
            of_path_forwards_to
    end.
