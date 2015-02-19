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

%%%=============================================================================
%%% Callbacks
%%%=============================================================================


suite() ->
  [{timetrap,{minutes,10}}].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_group(_GroupName, Config) ->
  Config.

end_per_group(_GroupName, _Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

groups() ->
  [].

all() ->
  [should_publish_net_flow].

%%%=============================================================================
%%% Testcases
%%%=============================================================================

should_publish_net_flow(_Config) ->
    %% GIVEN
    FlowPath =
        [{"00:00:00:00:0p0:01:00:01",
          [{[{in_port,1}],[{apply_actions,[{output,2,no_buffer}]}],[]},
           {[{in_port,2}], [{apply_actions,[{output,1,no_buffer}]}], []}]
         },
         {"00:00:00:00:00:01:00:02",
          [{[{in_port,2}],[{apply_actions,[{output,1,no_buffer}]}],[]},
           {[{in_port,1}],[{apply_actions,[{output,2,no_buffer}]}],[]}]
         }],
    SrcEP = <<"Src">>,
    DstEP = <<"Dst">>,

    %% WHEN
    NetFlowId = dobby_ofclient:publish_new_flow(SrcEP, DstEP, FlowPath),

    %% THEN
    meck:called(dby, publish, [SrcEP, DstEP, ])
