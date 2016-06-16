-module(echo_api_ip).

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
   [{application, json}, {text, plain}].

%%
'GET'({text, plain}, {_Url, _Heads, Env}, _) ->
   {_, Peer}  = lists:keyfind(peer, 1, Env),
   {ok, uri:host(Peer)};

'GET'(_, {_Url, _Heads, Env}, _) ->
	{_, Peer}  = lists:keyfind(peer, 1, Env),
	{ok, 
		jsx:encode([
			{origin, uri:host(Peer)}
		])
	}.