-module(echo_api_null).

-export([
   allowed_methods/1,
   content_provided/1, 
   'GET'/3,
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['GET', 'POST'].

%%
content_provided(_Req) ->
   [{'*', '*'}].

%%
'GET'(_Type, _Msg, {Url, Heads, Env}) ->
   error_logger:error_report([{url, uri:s(Url)}] ++ Heads ++ Env),
   {ok, <<>>}.   

'POST'(_Type, Msg, {Url, Heads, Env}) ->
   error_logger:error_report([{url, uri:s(Url)}, {payload, Msg}] ++ Heads ++ Env),
   {ok, <<>>}.
