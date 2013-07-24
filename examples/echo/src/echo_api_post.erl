-module(echo_api_post).

-export([
	resource/0,
	allowed_methods/0,
	content_provided/0, 
   content_accepted/0,
   'POST'/4
]).

%%
resource() ->
	{post}.

%%
allowed_methods() ->
   ['POST'].

%%
content_provided() ->
   [{application, json}].

%%
content_accepted() ->
   [{'*', '*'}].

%%
'POST'(_, Url, Heads, Msg) ->
	{_, Env} = lists:keyfind(env,  1, Heads),
	{_, IP}  = lists:keyfind(peer, 1, Env),
	H = [header(X) || X <- lists:keydelete(env, 1, Heads)],
	{ok, 
		jsx:encode([
			{headers, H}, 
			{origin,  list_to_binary(inet_parse:ntoa(IP))}, 
			{url,     uri:to_binary(Url)},
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



