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
	"*://*/status/:code".

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
'GET'(_, _Uri, _Heads, Env) ->
	scalar:i(opts:val(<<"code">>, Env)).

