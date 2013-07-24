-module(echo_api_ip).

-export([
	resource/0,
	allowed_methods/0,
	content_provided/0, 
   content_accepted/0,
   'GET'/3
]).

%%
resource() ->
	{ip}.

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
'GET'(_, _, Heads) ->
	{_, Env} = lists:keyfind(env,  1, Heads),
	{_, IP}  = lists:keyfind(peer, 1, Env),
	{ok, 
		jsx:encode([
			{origin, list_to_binary(inet_parse:ntoa(IP))}
		])
	}.