-module(echo_api_null).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   'GET'/2,
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['GET', 'POST'].

%%
content_provided(_Req) ->
   [{'*', '*'}].

%%
content_accepted(_Req) ->
   [].

%%
'GET'(_, {Url, Heads, Env}) ->
   error_logger:error_report([{url, uri:s(Url)}] ++ Heads ++ Env),
   {ok, <<>>}.   

'POST'(_, {Url, Heads, Env}, Msg) ->
   error_logger:error_report([{url, uri:s(Url)}, {payload, Msg}] ++ Heads ++ Env),
   {ok, <<>>}.
