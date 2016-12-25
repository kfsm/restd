%%
%% @doc
%%   rest api test module
-module(restd_restapi_test).

-export([
   allowed_methods/1,
   content_provided/1,
   'GET'/3,
   'PUT'/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{text, plain}].

%%
'GET'({{text, plain}, _}, _, {_Url, _Head, _Env}) ->
   {ok, [], <<"restd">>}.

%%
'PUT'(_, Msg, {_Url, _Heads, _Env}) ->
   {ok, [], Msg}.