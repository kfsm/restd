-module(echo_api_ip).

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
   [{application, json}, {text, plain}].

%%
content_accepted() ->
   [].

%%
'GET'({text, plain}, {_Url, _Heads, Env}) ->
   {_, IP}  = lists:keyfind(peer, 1, Env),
   {ok, scalar:s(inet_parse:ntoa(IP))};

'GET'(_, {_Url, _Heads, Env}) ->
	{_, IP}  = lists:keyfind(peer, 1, Env),
	{ok, 
		jsx:encode([
			{origin, scalar:s(inet_parse:ntoa(IP))}
		])
	}.