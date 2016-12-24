-module(restd_SUITE).
-include_lib("common_test/include/ct.hrl").

%%
%% common test
-export([
   all/0
  ,groups/0
  ,init_per_suite/1
  ,end_per_suite/1
  ,init_per_group/2
  ,end_per_group/2
]).

-export([
   restd_get/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, restapi}
   ].

groups() ->
   [
      {restapi, [parallel], 
         [restd_get]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   restd:start(),
   restapi(),
   Config.

end_per_suite(_Config) ->
   ok.

%% 
%%
init_per_group(_, Config) ->
   Config.

end_per_group(_, _Config) ->
   ok.

%%
%%
restapi() ->
   {ok, Pid} = restd:start_link(default, [
      {port,    "http://*:8888"},
      {backlog, 2},
      {route, [
         {"/get",  restd_get}
      ]}
   ]),
   erlang:unlink(Pid).


%%%----------------------------------------------------------------------------   
%%%
%%% unit
%%%
%%%----------------------------------------------------------------------------   
-define(URI,     "http://127.0.0.1:8888/").

restd_get(_) ->
   Uri  = uri:path(<<"/get">>, uri:new(?URI)),
   Sock = knet:socket(Uri),
   _    = knet:send(Sock, {'GET', Uri, []}),
   {ioctl, b, Sock} = knet:recv(Sock),
   {http, Sock, {200, <<"OK">>, _Head, _Env}} = knet:recv(Sock),
   {http, Sock, <<"restd">>} = knet:recv(Sock),
   {http, Sock, eof} = knet:recv(Sock).

