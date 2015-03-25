%%%=============================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd
%%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%% @doc <Module purpose>
%%% @end
%%%=============================================================================
-module(dofl_identifier).
-copyright("2015, Erlang Solutions Ltd.").

-export([]).

%% API
-export([net_flow/2,
         flow_mod/3,
         flow_table/2]).

-include_lib("dobby_clib/include/dobby.hrl").
-include("dobby_oflib.hrl").

%%%=============================================================================
%%% External functions
%%%=============================================================================

-spec net_flow(dby_identifier(), dby_identifier()) ->
                      {dby_identifier(), [tuple()]}.

net_flow(Src, Dst) ->
    {<<"NF:", Src/binary, ":", Dst/binary>>, [{type, of_net_flow}]}.

-spec flow_mod(dby_identifier(), of_version(), flow_mod()) ->
                      {dby_identifier(), [tuple()]}.

flow_mod(Dpid, OFVersion, FlowMod) ->
    {_Matches, _Instructions, Opts} = FlowMod,
    Cookie = proplists:get_value(cookie, Opts),
    {Cookie, [{type, of_flow_mod}, {dpid, Dpid}, {of_version, OFVersion}]}.


-spec flow_table(dby_identifier(), flow_mod()) ->
                        dby_identifier() | {error, Reason :: term()}.

flow_table(DatapahtId, {_Matches, _Actions, Opts}) ->
    TableNo = proplists:get_value(table_id, Opts),
    TableIdFun =
        fun(Dpid, _, [], _) when Dpid =:= DatapahtId ->
                {continue, []};
           (Identifier, IdMetadataInfo, _, _) ->
                case table_found(IdMetadataInfo, TableNo) of
                    true ->
                        {stop, Identifier};
                    _ ->
                        {skip, []}
                end
        end,
    dby:search(TableIdFun, [], DatapahtId, [breadth, {max_depth, 1}]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

table_found(IdMetadataInfo, TableNo) ->
    case get_metadata_value(type, IdMetadataInfo) of
        of_flow_table ->
            TableNo =:= get_metadata_value(table_no, IdMetadataInfo);
        _ ->
            false
    end.

get_metadata_value(Key, Metadatainfo) ->
    KeyMap = maps:get(atom_to_binary(Key, utf8), Metadatainfo, undefined),
    case KeyMap =/= undefined andalso maps:get(value, KeyMap) of
        false ->
            undefined;
        V when is_binary(V) ->
            binary_to_atom(V, utf8);
        V ->
            V
    end.
