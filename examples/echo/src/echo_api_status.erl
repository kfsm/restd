-module(echo_api_status).

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
'GET'(_, {_Uri, _Heads, Env}) ->
	scalar:i(opts:val(<<"code">>, Env)).

