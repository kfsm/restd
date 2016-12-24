-module(restd_get).

-export([
   allowed_methods/1,
   content_provided/1,
   'GET'/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{text, plain}].

%%
'GET'({{text, plain}, _}, _, {_Url, _Heads, _Env}) ->
   {ok, [], <<"restd">>}.
