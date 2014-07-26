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
   {_, Peer}  = lists:keyfind(peer, 1, Env),
   {ok, uri:host(Peer)};

'GET'(_, {_Url, _Heads, Env}) ->
	{_, Peer}  = lists:keyfind(peer, 1, Env),
	{ok, 
		jsx:encode([
			{origin, uri:host(Peer)}
		])
	}.