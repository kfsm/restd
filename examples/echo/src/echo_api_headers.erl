-module(echo_api_headers).

-export([
	allowed_methods/1,
	content_provided/1, 
   content_accepted/1,
   'GET'/2
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [].

%%
'GET'(_, {_Url, Heads, _Env}) ->
	{ok, 
		jsx:encode([
			{headers,  [header(X) || X <- Heads]}
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



