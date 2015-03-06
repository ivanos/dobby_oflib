%%%=============================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd
%%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%% @doc <Suite purpose>
%%% @end
%%%=============================================================================
-module(dofl_identifier_SUITE).
-copyright("2015, Erlang Solutions Ltd.").

%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TABLE_IDENTIFIER, <<"TableId">>).
-define(DPID, <<"00:00:00:00:00:01:00:01">>).

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
    [should_query_flow_table_id].

%%%=============================================================================
%%% Testcases
%%%=============================================================================

should_query_flow_table_id(_Config) ->
    %% GIVEN
    FlowMod = dofl_test_utils:flow_mod(),
    TableNo = dofl_test_utils:flow_mod_to_table_no(FlowMod),

    %% WHEN
    dofl_identifier:flow_table(?DPID, FlowMod),

    %% THEN
    assert_dobby_search_fun_correct(?DPID, TableNo).

%%%=============================================================================
%%% Assertions
%%%=============================================================================

assert_dobby_search_fun_correct(Dpid, TableNo) ->
    SearchFun = meck:capture(first, dby, search, 4, _ArgNo = 1),
    Path = switch_to_flow_table_path(Dpid),
    SearchResult = SearchFun(_FlowTableId = ?TABLE_IDENTIFIER,
                             flow_table_metadata_info(TableNo),
                             Path,
                             []),
    ?assertEqual({stop, ?TABLE_IDENTIFIER}, SearchResult).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

mock_dobby() ->
    ok = meck:expect(dby, search, 4, _Ret = ?TABLE_IDENTIFIER).
unmock_dobby() ->
    ok = meck:unload(dby).

switch_to_flow_table_path(Dpid) ->
    [{_SwIdentifier = Dpid,
      _SwIdMetadataInfo = metadata([{type, of_switch}]),
      _SwToFlowTableMetadataInfo =  metadata([{type, of_resource}])}].

flow_table_metadata_info(TableNo) ->
    metadata([{type, of_flow_table}, {table_no, TableNo}]).

metadata(Proplist) ->
    lists:foldl(fun({K, V}, AccMap)  ->
                        IM = inner_metadata(V),
                        maps:put(atom_to_binary(K, utf8), IM, AccMap)
                end, #{}, Proplist).

inner_metadata(V) when is_atom(V) ->
    inner_metadata(atom_to_binary(V, utf8));
inner_metadata(V) ->
    #{value => V, publisher_id => <<"ID">>, timestamp => <<"TSTM">>}.
