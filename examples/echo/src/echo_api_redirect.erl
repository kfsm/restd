-module(echo_api_redirect).

-export([
	resource/0,
	allowed_methods/0,
	content_provided/0, 
   content_accepted/0,
   'GET'/4
]).

%%
resource() ->
	{redirect, '_'}.

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
'GET'(_, Url, _Heads, _Env) ->
	case uri:get(segments, Url) of
		[_, <<"1">>] ->
			U = uri:set(path, <<"/get">>, Url),
			{302, [{'Location', uri:to_binary(U)}], <<>>};
		[_, Count]   ->
			N = list_to_integer(binary_to_list(Count)) - 1,
			U = uri:set(segments, [<<"redirect">>, scalar:s(N)], Url),
			{302, [{'Location', uri:to_binary(U)}], <<>>}
	end.
