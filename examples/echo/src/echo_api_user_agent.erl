-module(echo_api_user_agent).

-export([
	resource/0,
	allowed_methods/0,
	content_provided/0, 
   content_accepted/0,
   'GET'/4
]).

%%
resource() ->
	"*://*/user-agent".

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
'GET'(_, _, Heads, _Env) ->
	{_, UA} = lists:keyfind('User-Agent', 1, Heads),
	{ok, 
		jsx:encode([
			{'user-agent', UA}
		])
	}.