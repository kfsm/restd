-module(echo_api_redirect).

-export([
	allowed_methods/0,
	content_provided/0, 
   content_accepted/0,
   'GET'/4
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
'GET'(_, Url, _Heads, Env) ->
	case opts:val(<<"n">>, Env) of
		<<"1">> ->
			U = uri:set(path, <<"/get">>, Url),
			{302, [{'Location', uri:to_binary(U)}], <<>>};
		Count   ->
			N = scalar:i(Count) - 1,
			U = uri:segments([<<"redirect">>, scalar:s(N)], Url),
			{302, [{'Location', uri:s(U)}], <<>>}
	end.
