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
         publish_dp_flow_mod/2,
         publish_net_flow/4]).

-include_lib("dobby_clib/include/dobby.hrl").
-include("dobby_oflib.hrl").

-define(MD_VALUE(V), #{value := V}).
-define(TYPE(V), #{<<"type">> := #{value := V}}).

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
           [breadth, {max_depth, 100}, {loop, link}]) of
        Path = [_|_] ->
            Digraph = digraph:new(),
            insert_path(Path, Digraph),
            add_flow_tables_and_mods(Path, Digraph),
            {ok, Digraph};
        not_found ->
            {error, no_path}
    end.

%% @doc Publish a NetFlow between two endpoints to Dobby.
%%
%% Publishes a NetFlow identifier between `SrcEndpoint' and `DstEndpoint'
%% that are assumed to be present in Dobby database. It also connects
%% the FlowMods Identifiers to form a path that a packet would traverse
%% from the `SrcEndpoint' to the `DstEndpoint'. The first and the last
%% FlowMod identifiers are connected to the NetFlow Identifier.
%%
%% The function returns NetFlow identifier `NetFlowId' that can be
%% used for referencing published Net Flow.
-spec publish_net_flow(publisher_id(), dby_identifier(), dby_identifier(),
                       [dby_identifier()]) -> Result when
      Result :: {ok, NetFlowId :: dby_endpoint()}
              | {error, Reason :: term()}.

publish_net_flow(PublisherId, SrcEndpoint, DstEndpoint, FlowModsIds) ->
    NfId = publish_net_flow_identifier(PublisherId, SrcEndpoint, DstEndpoint),
    publish_flow_path(PublisherId, NfId, FlowModsIds),
    lager:info("Published NetFlow: ~p between endpoints src: ~p dst: ~p ~n",
               [NfId, SrcEndpoint, DstEndpoint]),
    {ok, NfId}.


%% @doc Publish a FlowMod for a Datapath.
%%
%% Publishes a FlowMod identifier for a given Datapath. The FlowMod is
%% not directly connected to the Datapath identifier bot to the appropriate
%% FlowTable identifier. The FlowMod identifier is also marked with appropriate
%% OpenFlow version.
%%
%% The function returns the FlowMod identifier that can be later used
%% for publishing a NetFlow.
-spec publish_dp_flow_mod(dby_identifier(), datapath_flow_mod()) -> Result when
      Result :: {ok, DpFlowModId :: dby_identifier()}
              | {error, Reason :: term()}.

publish_dp_flow_mod(PublisherId, {DatapathId, _, {_, _, Opts} = FlowMod} = DatapathFlowMod) ->
    FtId =
        case dofl_identifier:flow_table(DatapathId, FlowMod) of
            [] ->
                %% No flow table node.  Let's create one.
                %% Table id defaults to 0, to match of_msg_lib
                TableNo = proplists:get_value(table_id, Opts, 0),
                TableId = <<DatapathId/binary, "-table-", (integer_to_binary(TableNo))/binary>>,
                publish(PublisherId,
                        DatapathId,
                        {TableId, [{type, of_flow_table}, {table_no, TableNo}]},
                        [{type, of_resource}]),
                TableId;
            Id ->
                Id
        end,
    publish(PublisherId,
            FtId,
            {FmId, _} = _FmIdWithMd = flow_mod_identifier(DatapathFlowMod),
            dofl_link_metadata:flow_mod_with_flow_table()),
    {ok, FmId}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

publish_net_flow_identifier(PublisherId, Src, Dst) ->
    NfNode = {NfId, _NfMetadata} = dofl_identifier:net_flow(Src, Dst),
    publish(PublisherId, Src, NfNode,
            dofl_link_metadata:endpoint_with_net_flow(Src)),
    publish(PublisherId, NfId, Dst,
            dofl_link_metadata:endpoint_with_net_flow(NfId)),
    NfId.

publish_flow_path(PublisherId, NetFlowId, FlowModsIds) ->
    Fun = fun(FmId, PrevId) ->
                  LinkMd = of_path_link_md(PrevId, FmId, NetFlowId),
                  publish(PublisherId, PrevId, FmId, LinkMd),
                  FmId
          end,
    LastFmId = lists:foldl(Fun, NetFlowId, FlowModsIds),
    LinkMd = of_path_link_md(LastFmId, NetFlowId, NetFlowId),
    publish(PublisherId, LastFmId, NetFlowId, LinkMd).

of_path_link_md(Src, Dst, NetFlowId)
  when Src =:= NetFlowId; Dst =:= NetFlowId ->
    dofl_link_metadata:net_flow_with_flow_mod(Src, NetFlowId);
of_path_link_md(Src, _, NetFlowId) ->
    dofl_link_metadata:between_flow_mods(Src, NetFlowId).

flow_mod_identifier({Dpid, OFVersion, FlowMod}) ->
    dofl_identifier:flow_mod(Dpid, OFVersion, FlowMod).

publish(PublisherId, Src, Dst, LinkMetadata) ->
    dofl_publish:do(PublisherId, Src, Dst, LinkMetadata).

%% This function returns a function to be passed to dby:search.
vertices_edges_search_fun(Ep1, Ep2) ->
    fun(Id, #{<<"type">> := ?MD_VALUE(_Endpoint)}, [], Acc) when Id =:= Ep1 ->
            %% This is the starting point.
            {continue, Acc};
       (Id,
        #{<<"type">> := Type2} = NodeMetadata,
        [{_, #{<<"type">> := Type1}, #{<<"type">> := EdgeType}} | _] = Path,
        Acc) ->
            case valid_edge(Type1, EdgeType, Type2) of
                true when Id =:= Ep2 ->
                    %% We found the endpoint we're looking for.
                    %% Stop and return the path.
                    {stop, lists:reverse(Path, [{Id, NodeMetadata, #{}}])};
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
valid_edge(#{value := NodeType1}, #{value := EdgeType}, #{value := NodeType2}) ->
    valid_edge(NodeType1, EdgeType, NodeType2);
valid_edge(_Endpoint, <<"connected_to">>, <<"of_port">>) ->
    true;
valid_edge(<<"of_port">>, <<"connected_to">>, _Endpoint) ->
    true;
%% XXX: accepting "bound_to" as synonym for "connected_to"
valid_edge(_Endpoint, <<"bound_to">>, <<"of_port">>) ->
    true;
valid_edge(<<"of_port">>, <<"bound_to">>, _Endpoint) ->
    true;
%% Accept both 'port_of' and 'part_of' for now; use 'part_of'
%% exclusively at some point in the future.
valid_edge(<<"of_port">>, <<"port_of">>, <<"of_switch">>) ->
    true;
valid_edge(<<"of_switch">>, <<"port_of">>, <<"of_port">>) ->
    true;
valid_edge(<<"of_port">>, <<"part_of">>, <<"of_switch">>) ->
    true;
valid_edge(<<"of_switch">>, <<"part_of">>, <<"of_port">>) ->
    true;
valid_edge(<<"of_port">>, <<"connected_to">>, <<"of_port">>) ->
    true;
%% XXX: accepting "bound_to" as synonym for "connected_to"
valid_edge(<<"of_port">>, <<"bound_to">>, <<"of_port">>) ->
    true;
%% For Leviathan:
valid_edge(<<"container">>, <<"bound_to">>, <<"endpoint">>) ->
    true;
valid_edge(<<"endpoint">>, <<"veth_peer">>, <<"endpoint">>) ->
    true;
valid_edge(<<"endpoint">>, <<"bound_to">>, <<"bridge">>) ->
    true;
valid_edge(<<"bridge">>, <<"bound_to">>, <<"of_port">>) ->
    true;
valid_edge(<<"endpoint">>, <<"bound_to">>, <<"container">>) ->
    true;
valid_edge(<<"bridge">>, <<"bound_to">>, <<"endpoint">>) ->
    true;
valid_edge(<<"of_port">>, <<"bound_to">>, <<"bridge">>) ->
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

add_flow_tables_and_mods(Path, Digraph) ->
    SwitchesIds = switches_ids(Path, []),
    ModsAndTables = collect_flow_tables_and_mods(SwitchesIds, []),
    extend_digraph(ModsAndTables, Digraph).


switches_ids([{NodeId, #{<<"type">> := ValueMap}, _} | Rest], Acc) ->
    case maps:get(value, ValueMap) of
        <<"of_switch">> ->
            switches_ids(Rest, [NodeId | Acc]);
        _ ->
            switches_ids(Rest, Acc)
    end;
switches_ids([], Acc) ->
    Acc.

collect_flow_tables_and_mods([SwId | Rest], Acc) ->
    SearchFun = gather_flow_table_with_flow_mods_fun(SwId),
    FtsAndFms = dby:search(SearchFun, [], SwId, [breadth, {max_depth, 2}]),
    collect_flow_tables_and_mods(Rest, [lists:reverse(FtsAndFms) | Acc]);
collect_flow_tables_and_mods([], Acc) ->
    lists:flatten(Acc).

gather_flow_table_with_flow_mods_fun(SwitchId) ->
    fun(Id, _, [], Acc) when Id =:= SwitchId ->
            {continue, Acc};
       (FtId, ?TYPE(<<"of_flow_table">>) = IdMd, [{_, _, LinkMd}], Acc) ->
            {continue, [{SwitchId, LinkMd, FtId, IdMd} | Acc]};
       (FmId, ?TYPE(<<"of_flow_mod">>) = IdMd, [{FtId, _, LinkMd} | _], Acc) ->
            {continue, [{FtId, LinkMd, FmId, IdMd} | Acc]};
       (_, _, _, Acc) ->
            {skip, Acc}
    end.

extend_digraph([{ExistingNodeId, LinkMd, NodeId, NodeMd} | Rest], Digraph) ->
    digraph:add_vertex(Digraph, NodeId, NodeMd),
    digraph:add_edge(Digraph, ExistingNodeId, NodeId, LinkMd),
    digraph:add_edge(Digraph, NodeId, ExistingNodeId, LinkMd),
    extend_digraph(Rest, Digraph);
extend_digraph([], _Digraph) ->
    ok.
