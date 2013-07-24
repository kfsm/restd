-module(echo_api_response_headers).

-export([
	resource/0,
	allowed_methods/0,
	content_provided/0, 
   content_accepted/0,
   'GET'/3
]).

%%
resource() ->
	{'response-headers'}.

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
'GET'(_, Uri, _) ->
	H = uri:q(Uri),
	{ok, H, 
		jsx:encode([
			{headers, H} 
		])
	}.
