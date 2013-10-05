-module(echo_api_response_headers).

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
'GET'(_, {Uri, _Heads, _Env}) ->
	H = uri:q(Uri),
	{ok, H, 
		jsx:encode([
			{headers, H} 
		])
	}.
