-module(echo_api_redirect_to).

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
'GET'(_, Url, _Heads, _Env) ->
	{302, [{'Location', uri:q(url, <<>>, Url)}], <<>>}.

