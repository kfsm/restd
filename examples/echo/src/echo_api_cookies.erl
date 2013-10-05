-module(echo_api_cookies).

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
'GET'(_, {Uri, Heads, _Env}) ->
	case uri:get(segments, Uri) of
		[<<"cookies">>] ->
			% TODO: parse Cookie header
			{ok, 
				jsx:encode([
					{cookies, opts:val('Cookie', <<>>, Heads)} 
				])
			};
		[<<"cookies">>, <<"set">>] ->
			H = [{'Set-Cookie', <<(scalar:s(K))/binary, $=, (scalar:s(V))/binary, $;, "Path=/">>} || {K, V} <- uri:q(Uri)],
			U = uri:set(path, <<"/cookies">>, Uri),
			{302, [{'Location', uri:to_binary(U)} | H], <<>>};
		[<<"cookies">>, <<"delete">>] ->
			H = [{'Set-Cookie', <<(scalar:s(K))/binary, $=, $;, "expires=Thu, 01-Jan-1970 00:00:00 GMT; Max-Age=0; Path=/">>} || K <- uri:q(Uri)],
			U = uri:set(path, <<"/cookies">>, Uri),
			{302, [{'Location', uri:to_binary(U)} | H], <<>>}
	end.


