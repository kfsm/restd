-module(echo_api_get).

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
'GET'(_Type, _Msg, {Url, Heads, Env}) ->
	{_, Peer}  = lists:keyfind(peer, 1, Env),
	H = [header(X) || X <- Heads],
	{ok, 
		jsx:encode([
			{headers, H}, 
			{origin,  uri:host(Peer)}, 
			{url,     uri:s(Url)}
		])
	}.

header({'Content-Type', {Type, SubType}}) ->
	{'Content-Type', <<(scalar:s(Type))/binary, $/, (scalar:s(SubType))/binary>>};
header({'Accept', Val}) ->
	L = [<<(scalar:s(Type))/binary, $/, (scalar:s(SubType))/binary>> || {Type, SubType} <- Val],
	X = iolist_to_binary([hd(L)] ++ [[$,, X] || X <- tl(L)]),
	{'Accept', X};
header(X) ->
	X.



