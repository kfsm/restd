-module(echo_api_env).

-export([
	allowed_methods/0,
	content_provided/0, 
   content_accepted/0,
   'GET'/2
]).

%%
allowed_methods() ->
   ['GET'].

%%
content_provided() ->
   [{application, json}].

%%
content_accepted() ->
   [].

%%
'GET'(_, {_Url, _Heads, Env}) ->
	{ok, 
		jsx:encode([
			{env,   [env(X) || X <- Env]}
		])
	}.

env({Key, Val})
 when is_tuple(Val) ->
 	{Key, scalar:s(io_lib:format("~120p", [Val]))};

env({Key, Val}) ->
	{Key, scalar:s(Val)}.

