%%%=============================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd
%%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%% @doc API Dobby Client Library for OpenFlow
%%% @end
%%%=============================================================================
-module(dobby_oflib).
-copyright("2015, Erlang Solutions Ltd.").

%% API
-export([get_path/2,
         publish_new_flow/3]).

-include_lib("dobby_clib/include/dobby.hrl").
-include("dobby_oflib.hrl").

%%%=============================================================================
%%% External functions
%%%=============================================================================

-spec get_path(SrcEndpoint :: binary(), DstEndpoint :: binary()) ->
                      {ok, Path :: digraph:graph()} | {error, Reason :: term()}.

get_path(SrcEndpoint, DstEndpoint) ->
    {ok, digraph:new()}.

%% @doc Publish a Net Flow between two endpoints to Dobby.
%%
%% Publishes a NetFlow identifier between `SrcEndpoint' and `DstEndpoint'
%% that are assumed to be present in Dobby database. It also publishes
%% `FlowPath' that is set of links and identifiers representing OpenFlow
%% entities that provide logical connectivity between the two endpoints.
%% The first and the last identifiers of the `FlowPath' hang off
%% the Net Flow Identifier and their connections to it indicate the beginning
%% an the end of the `FlowPath'.
%%
%% The function returns Net Flow Identifier: `NetFlowId' that can be
%% used for referencing published Net Flow.
-spec publish_new_flow(identifier(), identifier(), flow_path()) ->
                              Result when
      Result :: {ok, NetFlowId :: endpoint()}
              | {error, Reason :: term()}.

publish_new_flow(SrcEndpoint, DstEndpoint, FlowPath) ->
    NfId = publish_net_flow_identifier(SrcEndpoint, DstEndpoint),
    publish_flow_path(NfId, FlowPath),
    lager:info("Published NetFlow: ~p between endpoints src: ~p dst: ~p ~n",
               [NfId, SrcEndpoint, DstEndpoint]),
    NfId.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

publish_net_flow_identifier(Src, Dst) ->
    %% TODO: In transaction
    NfNode = {NfId, _NfMetadata} = dofl_identifier:net_flow(Src, Dst),
    publish(Src, NfNode,
            dofl_link_metadata:endpoint_with_net_flow(Src)),
    publish(NfId, Dst,
            dofl_link_metadata:endpoint_with_net_flow(NfId)),
    NfId.

publish_flow_path(NetFlowId, FlowPath0) ->
    FlowPath1 = reconstruct_flow_path(FlowPath0),
    publish_flow_path(NetFlowId, FlowPath1, NetFlowId).

publish_flow_path(NetFlowId, [ExtendedFlowMod | T], LastId)
  when LastId =:= NetFlowId ->
    publish(NetFlowId,
            {Id, _Md} = flow_mod_identifier(ExtendedFlowMod),
            dofl_link_metadata:net_flow_with_flow_mod(NetFlowId, NetFlowId)),
    publish(Id,
            flow_table_identifier(ExtendedFlowMod),
            dofl_link_metadata:flow_mod_with_flow_table()),
    publish_flow_path(NetFlowId, T, Id);
publish_flow_path(NetFlowId, [ExtendedFlowMod | T], LastId) ->
    publish(LastId,
            {Id, _Md} = flow_mod_identifier(ExtendedFlowMod),
            dofl_link_metadata:between_flow_mods(LastId, NetFlowId)),
    publish(Id,
            flow_table_identifier(ExtendedFlowMod),
            dofl_link_metadata:flow_mod_with_flow_table()),
    publish_flow_path(NetFlowId, T, Id);
publish_flow_path(NetFlowId, [], LastId) ->
    publish(LastId, NetFlowId,
            dofl_link_metadata:net_flow_with_flow_mod(LastId, NetFlowId)).

flow_mod_identifier({Dpid, OFVersion, FlowMod}) ->
    dofl_identifier:flow_mod(Dpid, OFVersion, FlowMod).

flow_table_identifier({Dpid, _OFVersion, FlowMod}) ->
    dofl_identifier:flow_table(Dpid, FlowMod).

publish(Src, Dst, LinkMetadata) ->
    dby:publish(Src, Dst, LinkMetadata, [persistent]).

reconstruct_flow_path(FlowPath0) ->
    Fun = fun({Dpid, {OFVersion, FlowMods}}) ->
                  [{Dpid, OFVersion, FM} || FM <- FlowMods]
          end,
    FlowPath1 = lists:map(Fun, FlowPath0),
    lists:flatten(FlowPath1).


