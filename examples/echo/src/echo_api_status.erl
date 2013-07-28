-module(echo_api_status).

-export([
	resource/0,
	allowed_methods/0,
	content_provided/0, 
   content_accepted/0,
   'GET'/4
]).

%%
resource() ->
	{status, '_'}.

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
'GET'(_, Uri, _Heads, _Env) ->
	[_, Code] = uri:get(segments, Uri),
	list_to_integer(
		binary_to_list(Code)
	).
