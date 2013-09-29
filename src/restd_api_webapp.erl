%% @description
%%    webapp html container
-module(restd_api_webapp).

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
   [{text, html}].

%%
content_accepted() ->
   [].

%%
'GET'(_, _Url, _Heads, Env) ->
	File = opts:val(<<"index">>, <<"index.html">>, Env),
	Root = case opts:val(htdoc, Env) of
		X when is_atom(X) -> code:priv_dir(X);
		X when is_list(X) -> X
	end,
	file:read_file(
		filename:join([Root, htdoc, scalar:c(File)])
	).
