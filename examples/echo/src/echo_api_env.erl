-module(echo_api_env).

-export([
	allowed_methods/1,
	content_provided/1, 
   content_accepted/1,
   'GET'/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [].

%%
'GET'(_, {_Url, _Heads, Env}, _) ->
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

