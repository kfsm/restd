-module(echo_api_ip).

-export([
	allowed_methods/0,
	content_provided/0, 
   content_accepted/0,
   'GET'/4
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
'GET'(_, _, _Heads, Env) ->
	{_, IP}  = lists:keyfind(peer, 1, Env),
	{ok, 
		jsx:encode([
			{origin, list_to_binary(inet_parse:ntoa(IP))}
		])
	}.