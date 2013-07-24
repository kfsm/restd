-module(echo_api_root).

-export([
	resource/0,
	allowed_methods/0,
	content_provided/0, 
   content_accepted/0,
   'GET'/3
]).

%% 
resource() ->
	{}.

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
'GET'(_, _, _) ->
	file:read_file(
      filename:join([code:priv_dir(echo), "index.html"])
   ).
