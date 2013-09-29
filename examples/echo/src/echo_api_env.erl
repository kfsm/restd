-module(echo_api_env).

-export([
	resource/0,
	allowed_methods/0,
	content_provided/0, 
   content_accepted/0,
   'GET'/4
]).

%%
resource() ->
	"*://*/env/:name".

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
'GET'(_, _, _Heads, Env) ->
	{ok, 
		jsx:encode([
			{env,   [env(X) || X <- Env]}
		])
	}.

env({Key, Val})
 when is_tuple(Val) ->
 	{Key, scalar:s(io_lib:format("~p", [Val]))};

env({Key, Val}) ->
	{Key, scalar:s(Val)}.

