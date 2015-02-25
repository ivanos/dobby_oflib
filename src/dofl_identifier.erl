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

-spec net_flow(Src :: identifier(), Dst :: identifier()) -> NetFlow :: endpoint().

net_flow(Src, Dst) ->
    {<<"NF:", Src/binary, ":", Dst/binary>>, #{type => of_net_flow}}.

-spec flow_mod(Dpid :: binary(), OFVersion :: of_version(), FlowMod :: flow_mod())
              -> FlowModId :: endpoint().

flow_mod(Dpid, OFVersion, FlowMod) ->
    {_Matches, _Instructions, Opts} = FlowMod,
    Cookie = proplists:get_value(cookie, Opts),
    {Cookie, #{type => of_flow_mod, dpid => Dpid, of_version => OFVersion}}.


-spec flow_table(DatapathId :: binary(), FlowMod :: flow_mod())
                -> FlowTableId :: idenfier().

flow_table(DatapahtId, {_Matches, _Actions, Opts}) ->
    TableNo = proplists:get_value(table_id, Opts),
    TableIdFun =
        fun(Dpid, _, undefined, []) ->
                {continue, Dpid};
           (Identifier, IdMetadata, _LinkMetadata, Dpid) ->
                case [maps:get(P, IdMetadata) || P <- [type, table_no]] of
                    [of_flow_table, TableNo] ->
                        {stop, Identifier};
                    _ ->
                        {skip, Dpid}
                end
        end,
    dby:search(TableIdFun, [], DatapahtId, [breadth,{max_depth, 1}]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
