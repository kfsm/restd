-module(restd_codec).
-compile({parse_transform, category}).

-export([
   encode_form/1,
   decode_form/1
]).

%%
%%
encode_form(Form) ->
   [identity ||
      maps:to_list(Form),
      lists:map(fun to_pair/1, _),
      scalar:s(lists:join(<<$&>>, _))
   ].

to_pair(Pair) ->
   scalar:s(
      lists:join(<<$=>>, 
         [uri:escape(X) || X <- erlang:tuple_to_list(Pair)]
      )
   ).


decode_form(Form) ->
   [identity ||
      binary:split(scalar:s(Form), <<$&>>, [trim, global]),
      lists:map(fun as_pair/1, _),
      maps:from_list(_)
   ].

as_pair(Pair) ->
   erlang:list_to_tuple(
      [uri:unescape(X) || X <- binary:split(Pair, <<$=>>)]
   ).
