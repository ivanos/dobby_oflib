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
         flow_table/2,
         encode_matches/1,
         decode_matches/1]).

-include_lib("dobby_clib/include/dobby.hrl").
-include("dobby_oflib.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
    Priority = proplists:get_value(priority, Opts, 16#ffff),
    %% The identifier must be a valid JSON string, so no raw cookie
    %% binary there.
    %% TODO: come up with better identifier for flow mods
    {iolist_to_binary(io_lib:format("~w", [Cookie])),
     [{type, of_flow_mod}, {dpid, Dpid}, {of_version, OFVersion},
      {priority, Priority},
      {matches, encode_matches(Matches)},
      {instructions, encode_instructions(Instructions)}]}.

%% @doc Convert from of_msg_lib style matches to something that can be
%% turned into JSON.
encode_matches(Matches) ->
    lists:map(
      fun(#{<<"match">> := _, <<"value">> := _} = AlreadyEncoded) ->
              AlreadyEncoded;
         ({Field, Value}) when is_binary(Value) ->
              %% Special case for binaries: convert them to lists, so
              %% they can be encoded as JSON.
              #{<<"match">> => atom_to_binary(Field, utf8),
                <<"type">> => <<"binary">>,
                <<"value">> => binary_to_list(Value)};
         ({Field, Value}) ->
              #{<<"match">> => atom_to_binary(Field, utf8),
                <<"value">> => Value};
         ({Field, Value, Mask}) when is_binary(Value), is_binary(Mask) ->
              %% Same here: use lists for binaries.
              %% If there's a mask, it's always a binary.
              #{<<"match">> => atom_to_binary(Field, utf8),
                <<"value">> => binary_to_list(Value),
                <<"mask">> => binary_to_list(Mask)}
      end, Matches).

%% @doc Convert matches stored in Dobby to the format expected by
%% of_msg_lib.
decode_matches(Matches) ->
    lists:map(
      fun(#{<<"match">> := Field, <<"value">> := Value,
            <<"mask">> := Mask}) ->
              {binary_to_atom(Field, utf8),
               list_to_binary(Value),
               list_to_binary(Mask)};
         (#{<<"match">> := Field, <<"value">> := Value,
            <<"type">> := <<"binary">>}) ->
              {binary_to_atom(Field, utf8),
               list_to_binary(Value)};
         (#{<<"match">> := Field, <<"value">> := Value}) ->
              {binary_to_atom(Field, utf8), Value}
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

%%%=============================================================================
%%% Test cases
%%%=============================================================================

-ifdef(TEST).
%% Ensure that encode_matches and decode_matches don't lose anything
%% on a roundtrip.
encode_decode_matches_test_() ->
    TestCases =
        [{"In port", [{in_port, 42}]}
        ,{"IPv4 source", [{ipv4_src, <<1,2,3,4>>}]}
        ,{"IPv4 source with mask", [{ipv4_src, <<1,2,3,4>>, <<255,255,255,0>>}]}],
    [{Label, ?_assertEqual(TestCase, decode_matches(encode_matches(TestCase)))}
     || {Label, TestCase} <- TestCases].
-endif.
