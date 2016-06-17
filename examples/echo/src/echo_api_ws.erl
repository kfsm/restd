-module(echo_api_ws).

-export([
   allowed_methods/1,
   content_provided/1, 
   recv/3,
   send/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}, {text, plain}].

%%
%%
recv(_Type, Msg, {_Url, _Head, _Env}) ->
   Self = self(),
   spawn(fun() -> Self ! Msg end),
   {ok, Msg}.

%%
send(_Type, Msg, {_Url, _Head, _Env}) ->
   {ok, <<$+, $+, $+, $ , Msg/binary>>}.
