-module(echo_api_headers).

-export([
	resource/0,
	allowed_methods/0,
	content_provided/0, 
   content_accepted/0,
   'GET'/4
]).

%%
resource() ->
	{headers}.

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
	H = [header(X) || X <- lists:keydelete(env, 1, Heads)],
	{ok, 
		jsx:encode([{headers, H}])
	}.

header({'Content-Type', {Type, SubType}}) ->
	{'Content-Type', <<(scalar:s(Type))/binary, $/, (scalar:s(SubType))/binary>>};
header({'Accept', Val}) ->
	L = [<<(scalar:s(Type))/binary, $/, (scalar:s(SubType))/binary>> || {Type, SubType} <- Val],
	X = iolist_to_binary([hd(L)] ++ [[$,, X] || X <- tl(L)]),
	{'Accept', X};
header(X) ->
	X.



