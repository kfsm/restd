-module(echo_api_redirect_to).

-export([
	allowed_methods/1,
	content_provided/1, 
   content_accepted/1,
   'GET'/3
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
'GET'(_Type, _Msg, {Url, _Heads, _Env}) ->
	{302, [{'Location', uri:q(url, <<>>, Url)}], <<>>}.

