%%
%% @doc
%%   rest api test module
-module(restd_restapi_auth).

-export([
   allowed_methods/1,
   content_provided/1,
   authorize/2,
   'GET'/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{text, plain}].

authorize('GET', {_Url, Head, _}) ->
io:format("~p~n", [Head]),
   case lens:get(lens:pair('Authorization', undefined), Head) of
      <<"private">> -> 
         ok;
      <<"public">> -> 
         {error, forbidden};
      _ ->
         {error, unauthorized}
   end.

%%
'GET'({{text, plain}, _}, _, {_Url, _Head, _Env}) ->
   {ok, [], <<"restd">>}.
