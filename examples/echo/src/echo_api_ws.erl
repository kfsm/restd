-module(echo_api_ws).

-export([
   allowed_methods/1,
   content_provided/1, 
   'GET'/3,
   stream/4
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}, {text, plain}].

%%
'GET'(_, {_Url, _Heads, Env}, Msg) ->
   io:format("==> ~p~n", [Msg]),
   Self = self(),
   spawn(fun() -> Self ! Msg end),
   ok.   

%%
stream(_, _, _, Msg) ->
   io:format("--> ~p~n", [Msg]),
   Msg.