-module(echo_api_redirect).

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
'GET'(_Type, _Msg, {Url, _Heads, Env}) ->
	case opts:val(<<"n">>, Env) of
		<<"1">> ->
			U = uri:set(path, <<"/get">>, Url),
			{302, [{'Location', uri:to_binary(U)}], <<>>};
		Count   ->
			N = scalar:i(Count) - 1,
			U = uri:segments([<<"redirect">>, scalar:s(N)], Url),
			{302, [{'Location', uri:s(U)}], <<>>}
	end.
