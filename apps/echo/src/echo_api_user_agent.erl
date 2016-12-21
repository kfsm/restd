-module(echo_api_user_agent).

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
'GET'(_Type, _Msg, {_, Heads, _Env}) ->
	{_, UA} = lists:keyfind('User-Agent', 1, Heads),
	{ok, 
		jsx:encode([
			{'user-agent', UA}
		])
	}.