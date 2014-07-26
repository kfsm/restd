-module(echo_api_get).

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
   [{application, json}].

%%
content_accepted() ->
   [].

%%
'GET'(_, {Url, Heads, Env}) ->
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



