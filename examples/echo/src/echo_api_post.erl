-module(echo_api_post).

-export([
	allowed_methods/1,
	content_provided/1, 
   content_accepted/1,
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['POST'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [{'*', '*'}].

%%
'POST'(_, {Url, Heads, Env}, Msg) ->
	{_, Peer}  = lists:keyfind(peer, 1, Env),
	H = [header(X) || X <- Heads],
	{ok, 
		jsx:encode([
			{headers, H}, 
			{origin,  uri:host(Peer)}, 
			{url,     uri:s(Url)},
			{data,    Msg}
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



