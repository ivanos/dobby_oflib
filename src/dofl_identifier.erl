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
    {Matches, Instructions, Opts} = FlowMod,
    Cookie = proplists:get_value(cookie, Opts),
    %% The identifier must be a valid JSON string, so no raw cookie
    %% binary there.
    %% TODO: come up with better identifier for flow mods
    {iolist_to_binary(io_lib:format("~w", [Cookie])),
     [{type, of_flow_mod}, {dpid, Dpid}, {of_version, OFVersion},
      {matches, encode_matches(Matches)},
      {instructions, encode_instructions(Instructions)}]}.

encode_matches(Matches) ->
    lists:map(
      fun(#{<<"match">> := _, <<"value">> := _} = AlreadyEncoded) ->
              AlreadyEncoded;
         ({Field, Value}) ->
              #{<<"match">> => atom_to_binary(Field, utf8),
                <<"value">> => Value}
      end, Matches).

encode_instructions(Instructions) ->
    lists:map(
      fun({apply_actions, Actions}) ->
              #{<<"instruction">> => <<"apply_actions">>,
                <<"actions">> =>
                    lists:map(
                      fun({output, Port, no_buffer}) ->
                              #{<<"action">> => <<"output">>,
                                <<"port">> => Port,
                                <<"buffer">> => <<"no_buffer">>}
                      end, Actions)
               }
      end, Instructions).

-spec flow_table(dby_identifier(), flow_mod()) ->
                        dby_identifier() | {error, Reason :: term()}.

flow_table(DatapahtId, {_Matches, _Actions, Opts}) ->
    %% Table id defaults to 0, to match of_msg_lib
    TableNo = proplists:get_value(table_id, Opts, 0),
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
