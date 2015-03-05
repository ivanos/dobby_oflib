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
    case dby:search(
           vertices_edges_search_fun(SrcEndpoint, DstEndpoint),
           not_found,
           SrcEndpoint,
           [breadth, {max_depth, 100}]) of
        Path = [_|_] ->
            Digraph = digraph:new(),
            insert_path(Path, Digraph),
            {ok, Digraph};
        not_found ->
            {error, no_path}
    end.

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
-spec publish_new_flow(dby_identifier(), dby_identifier(), flow_path()) ->
                              Result when
      Result :: {ok, NetFlowId :: dby_endpoint()}
              | {error, Reason :: term()}.

publish_new_flow(SrcEndpoint, DstEndpoint, FlowPath) ->
    NfId = publish_net_flow_identifier(SrcEndpoint, DstEndpoint),
    publish_flow_path(NfId, FlowPath),
    lager:info("Published NetFlow: ~p between endpoints src: ~p dst: ~p ~n",
               [NfId, SrcEndpoint, DstEndpoint]),
    {ok, NfId}.

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

%% This function returns a function to be passed to dby:search.
vertices_edges_search_fun(Ep1, Ep2) ->
    fun(Id, #{<<"type">> := <<"endpoint">>}, [], Acc) when Id =:= Ep1 ->
            %% This is the starting point.
            {continue, Acc};
       (Id,
        #{<<"type">> := <<"endpoint">>} = NodeMetadata,
        [{_, #{<<"type">> := <<"of_port">>}, #{<<"type">> := <<"connected_to">>}} | _] = Path,
        Acc) ->
            %% We've found an endpoint.  That's only interesting if
            %% it's the endpoint we're looking for.
            if Id =:= Ep2 ->
                    %% If so, stop and return the path.
                    {stop, lists:reverse(Path, [{Id, NodeMetadata, #{}}])};
               true ->
                    %% Otherwise, keep going.
                    {skip, Acc}
            end;
       (_Id,
        #{<<"type">> := Type2},
        [{_, #{<<"type">> := Type1}, #{<<"type">> := EdgeType}} | _],
        Acc) ->
            case valid_edge(Type1, EdgeType, Type2) of
                true ->
                    {continue, Acc};
                false ->
                    {skip, Acc}
            end
    end.

-spec valid_edge(Node1Type :: binary(),
                 EdgeType :: binary(),
                 Node2Type :: binary()) -> boolean().
%% @doc
%% Return true if we should follow an edge of type `EdgeType' from a
%% node of type `Node1Type' to a node of type `Node2Type'.
valid_edge(<<"endpoint">>, <<"connected_to">>, <<"of_port">>) ->
    true;
valid_edge(<<"of_port">>, <<"port_of">>, <<"of_switch">>) ->
    true;
valid_edge(<<"of_switch">>, <<"port_of">>, <<"of_port">>) ->
    true;
valid_edge(<<"of_port">>, <<"connected_to">>, <<"of_port">>) ->
    true;
valid_edge(_, _, _) ->
    false.

insert_path([{NodeId, NodeMetadata, EdgeMetadata} | Rest],
            Digraph) ->
    %% Insert the first node as a vertex.
    digraph:add_vertex(Digraph, NodeId, NodeMetadata),
    %% Then recurse through the rest of the list, adding edges as we
    %% go along.
    insert_path(Rest, NodeId, EdgeMetadata, Digraph).

insert_path([], _, _, _) ->
    ok;
insert_path([{NodeId, NodeMetadata, NextEdgeMetadata} | Rest],
            PreviousNodeId, EdgeMetadata, Digraph) ->
    digraph:add_vertex(Digraph, NodeId, NodeMetadata),
    %% We want bidirectional edges:
    digraph:add_edge(Digraph, PreviousNodeId, NodeId, EdgeMetadata),
    digraph:add_edge(Digraph, NodeId, PreviousNodeId, EdgeMetadata),
    insert_path(Rest, NodeId, NextEdgeMetadata, Digraph).
