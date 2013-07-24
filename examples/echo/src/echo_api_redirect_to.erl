-module(echo_api_redirect_to).

-export([
	resource/0,
	allowed_methods/0,
	content_provided/0, 
   content_accepted/0,
   'GET'/3
]).

%%
resource() ->
	{'redirect-to'}.

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
'GET'(_, Url, _Heads) ->
	{302, [{'Location', uri:q(url, Url)}], <<>>}.

