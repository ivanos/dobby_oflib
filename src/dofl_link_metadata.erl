%%%=============================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd
%%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%% @doc <Module purpose>
%%% @end
%%%=============================================================================
-module(dofl_link_metadata).
-copyright("2015, Erlang Solutions Ltd.").

%% API
-export([endpoint_with_net_flow/1,
         net_flow_with_flow_mod/2,
         between_flow_mods/2,
         flow_mod_with_flow_table/0]).


-include_lib("dobby_clib/include/dobby.hrl").
-include("dobby_oflib.hrl").

%%%=============================================================================
%%% External functions
%%%=============================================================================

-spec endpoint_with_net_flow(dby_identifier()) -> map().

endpoint_with_net_flow(Src) ->
    #{type => ep_to_nf, src => Src}.


-spec net_flow_with_flow_mod(dby_identifier(), dby_identifier()) -> map().

net_flow_with_flow_mod(Src, NetFlowId) when Src =:= NetFlowId ->
    #{type => of_path_starts_at, src => Src, net_flow_ids => [NetFlowId]};
net_flow_with_flow_mod(Src, NetFlowId) ->
    #{type => of_path_ends_at, src => Src, net_flow_ids => [NetFlowId]}.


-spec between_flow_mods(dby_identifier(), dby_identifier()) -> map().

between_flow_mods(Src, NetFlowId) ->
    #{type => of_path_forwards_to, src => Src, net_flow_ids => [NetFlowId]}.

-spec flow_mod_with_flow_table() -> map().

flow_mod_with_flow_table() ->
    #{type => of_resource}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
