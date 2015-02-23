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

%% -define(MYMAC, "MY").

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type openflow_path() ::
        [#{DatapathId :: binary() =>
                         list({OFVersion :: 4 | 5, [flow_mod()]})
          }].

-type flow_mod() :: {Matches :: [term()],
                     Instructions :: [term()],
                     Opts :: [term()]}.

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
%% `OpenFlowPath' that is set of links and identifiers representing OpenFlow
%% entities that provide logical connectivity between the two endpoints.
%% The first and the last identifiers of the `OpenFlowPath' hang off
%% the Net Flow Identifier and their connections to it indicate the beginning
%% an the end of the `OpenFlowPath'.
%%
%% The function returns Net Flow Identifier: `NetFlowId' that can be
%% used for referencing published Net Flow.
-spec publish_new_flow(identifier(), identifier(), openflow_path()) ->
                              Result when
      Result :: {ok, NetFlowId :: endpoint()}
              | {error, Reason :: term()}.

publish_new_flow(SrcEndpoint, DstEndpoint, OpenFlowPath) ->
    NfId = publish_net_flow_identifer(SrcEndpoint, DstEndpoint),
    publish_openflow_path(NfId, OpenFlowPath),
    lager:info("Published NetFlow: ~p between endpoints src: ~p dst: ~p ~n",
               [NfId, SrcEndpoint, DstEndpoint]),
    NfId.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

publish_net_flow_identifer(Src, Dst) ->
    %% TODO: In transaction
    NfId = net_flow_identifier(Src, Dst),
    publish(Src, NfId, link_metadata(ep_to_nf, Src)),
    publish(NfId, Dst, link_metadata(ep_to_nf, NfId)),
    NfId.

publish_openflow_path(NetFlowId, OpenFlowPath0) ->
    FlowPath1 = flatten_openflow_path(OpenFlowPath0),
    publish_openflow_path(NetFlowId, FlowPath1, NetFlowId).

publish_openflow_path(NetFlowId, [ExtendedFlowMod | T], LastId)
  when LastId =:= NetFlowId ->
    publish(NetFlowId,
            {Id, _Md} = flow_mod_identifier(ExtendedFlowMod),
            link_metadata(of_path_starts_at, {NetFlowId, NetFlowId})),
    publish_openflow_path(NetFlowId, T, Id);
publish_openflow_path(NetFlowId, [ExtendedFlowMod | T], LastId) ->
    publish(LastId,
            {Id, _Md} = flow_mod_identifier(ExtendedFlowMod),
            link_metadata(of_path_forwards_to, {NetFlowId, LastId})),
    publish_openflow_path(NetFlowId, T, Id);
publish_openflow_path(NetFlowId, [], LastId) ->
    publish(LastId, NetFlowId,
            link_metadata(of_path_ends_at, {NetFlowId, LastId})).

net_flow_identifier(Src, Dst) ->
    <<"NF:", Src/binary, ":", Dst/binary>>.

flow_mod_identifier({Dpid, OFVersion, FlowMod}) ->
    {_Matches, _Instructions, Opts} = FlowMod,
    Cookie = proplists:get_value(cookie, Opts),
    {Cookie, #{dpid => Dpid, of_version => OFVersion}}.

link_metadata(Type = ep_to_nf, SrcIdentifier) ->
    #{type => Type, src => SrcIdentifier};
link_metadata(Type, {NetFlowId, Src})
  when Type =:= of_path_starts_at;
       Type =:= of_path_ends_at;
       Type =:= of_path_forwards_to ->
    #{type => Type, src => Src, net_flow_ids => [NetFlowId]}.

publish(Src, Dst, LinkMetadata) ->
    dby:publish(Src, Dst, LinkMetadata, [persistent]).

flatten_openflow_path(FlowPath0) ->
    Fun = fun({Dpid, {OFVersion, FlowMods}}) ->
                  [{Dpid, OFVersion, FlowMod} || FlowMod <- FlowMods]
          end,
    FlowPath1 = lists:map(Fun, FlowPath0),
    lists:flatten(FlowPath1).


