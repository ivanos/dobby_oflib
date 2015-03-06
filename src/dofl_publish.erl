%%%=============================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd
%%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%% @doc Module encapsulating publishing to the dobby database
%%% @end
%%%=============================================================================
-module(dofl_publish).
-copyright("2015, Erlang Solutions Ltd.").

%% API
-export([do/4]).

-include_lib("dobby_clib/include/dobby.hrl").
-include("dobby_oflib.hrl").

%%%=============================================================================
%%% External functions
%%%=============================================================================

do(PublisherId, Src, Dst, LinkMetadata) ->
    do_publish(PublisherId,
               binarize(Src),
               binarize(Dst),
               binarize_metadata(LinkMetadata)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

binarize(Identifier) when is_binary(Identifier) ->
    Identifier;
binarize({Identifier, Metadata}) when is_binary(Identifier) ->
    {Identifier, binarize_metadata(Metadata)}.

binarize_metadata(Metadata) when is_list(Metadata)->
    [{binarize_term(K), binarize_term(V)} || {K, V} <- Metadata].

binarize_term(T) when is_atom(T) ->
    atom_to_binary(T, utf8);
binarize_term(T)
  when is_binary(T);
       is_number(T);
       is_function(T);
       is_list(T);
       T =:= delete;
       T =:= nochange ->
    T.

do_publish(PublisherId, Src, Dst, LinkMetadata) ->
    dby:publish(PublisherId, Src, Dst, LinkMetadata, [persistent]).
