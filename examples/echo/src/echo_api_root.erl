-module(echo_api_root).

-export([
	resource/0,
	allowed_methods/0,
	content_provided/0, 
   content_accepted/0,
   'GET'/4
]).

%% 
resource() ->
	"*://*/".

%%
allowed_methods() ->
   ['GET'].

%%
content_provided() ->
   [{text, html}].

%%
content_accepted() ->
   [].

%%
'GET'(_, _Uri, _Heads, _Env) ->
	file:read_file(
      filename:join([code:priv_dir(echo), "index.html"])
   ).
