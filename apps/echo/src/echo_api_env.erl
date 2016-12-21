-module(echo_api_env).

-export([
	allowed_methods/1,
	content_provided/1, 
   'GET'/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}].


%%
'GET'(_Type, _Msg, {_Url, _Heads, Env}) ->
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

