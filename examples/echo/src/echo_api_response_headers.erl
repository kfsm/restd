-module(echo_api_response_headers).

-export([
	allowed_methods/1,
	content_provided/1, 
   content_accepted/1,
   'GET'/2
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
'GET'(_, {Uri, _Heads, _Env}) ->
	H = uri:q(Uri),
	{ok, H, 
		jsx:encode([
			{headers, H} 
		])
	}.
