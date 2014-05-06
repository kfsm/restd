-module(echo_api_null).

-export([
   allowed_methods/0,
   content_provided/0, 
   content_accepted/0,
   'GET'/2,
   'POST'/3
]).

%%
allowed_methods() ->
   ['GET', 'POST'].

%%
content_provided() ->
   [{'*', '*'}].

%%
content_accepted() ->
   [].

%%
'GET'(_, {Url, Heads, Env}) ->
   error_logger:error_report([{url, uri:s(Url)}] ++ Heads ++ Env),
   {ok, <<>>}.   

'POST'(_, {Url, Heads, Env}, Msg) ->
   error_logger:error_report([{url, uri:s(Url)}, {payload, Msg}] ++ Heads ++ Env),
   {ok, <<>>}.
