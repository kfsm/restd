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
   restd_get_ok/1, restd_not_available/1, restd_not_implemented/1, restd_not_allowed/1,
   restd_authorized/1, restd_forbidden/1, restd_not_authorized/1,
   restd_cors/1
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
         [
            restd_get_ok, restd_not_available, restd_not_implemented, restd_not_allowed,
            restd_authorized, restd_forbidden, restd_not_authorized,
            restd_cors
         ]}
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
   {ok, Pid} = restd:start_link(default, #{
      port => "http://*:8888",
      backlog => 1024,
      route => [
         {"/test/a",     restd_restapi_test},
         {"/test/auth",  restd_restapi_auth}
      ]
   }),
   erlang:unlink(Pid).


%%%----------------------------------------------------------------------------   
%%%
%%% unit
%%%
%%%----------------------------------------------------------------------------   
-define(URI,     "http://127.0.0.1:8888/").

restd_get_ok(_) ->
   Uri  = uri:path(<<"/test/a">>, uri:new(?URI)),
   Sock = socket(Uri, {'GET', Uri, [{'Connection', 'keep-alive'}]}),
   {http, Sock, {200, <<"OK">>, _Head, _Env}} = knet:recv(Sock),
   {http, Sock, <<"restd">>} = knet:recv(Sock),
   {http, Sock, eof} = knet:recv(Sock).


restd_not_available(_) ->
   Uri  = uri:path(<<"/test/unavailable">>, uri:new(?URI)),
   Sock = socket(Uri, {'DELETE', Uri, [{'Connection', 'keep-alive'}]}),
   {http, Sock, {503, <<"Service Unavailable">>, _Head, _Env}} = knet:recv(Sock),
   {http, Sock, _} = knet:recv(Sock),
   {http, Sock, eof} = knet:recv(Sock).


restd_not_implemented(_) ->
   Uri  = uri:path(<<"/test/a">>, uri:new(?URI)),
   Sock = socket(Uri, {'DELETE', Uri, [{'Connection', 'keep-alive'}]}),
   {http, Sock, {501, <<"Not Implemented">>, _Head, _Env}} = knet:recv(Sock),
   {http, Sock, _} = knet:recv(Sock),
   {http, Sock, eof} = knet:recv(Sock).


restd_not_allowed(_) ->
   Uri  = uri:path(<<"/test/a">>, uri:new(?URI)),
   Sock = socket(Uri, {'PUT', Uri, [{'Connection', 'keep-alive'}, {'Transfer-Encoding', <<"chunked">>}]}),
   knet:send(Sock, <<"abcdef">>),
   knet:send(Sock, eof),
   {http, Sock, {405, <<"Method Not Allowed">>, _Head, _Env}} = knet:recv(Sock),
   {http, Sock, _} = knet:recv(Sock),
   {http, Sock, eof} = knet:recv(Sock).


restd_authorized(_) ->
   Uri  = uri:path(<<"/test/auth">>, uri:new(?URI)),
   Sock = socket(Uri, {'GET', Uri, [{'Connection', 'keep-alive'}, {<<"Authorization">>, <<"private">>}]}),
   {http, Sock, {200, <<"OK">>, _Head, _Env}} = knet:recv(Sock),
   {http, Sock, <<"restd">>} = knet:recv(Sock),
   {http, Sock, eof} = knet:recv(Sock).


restd_forbidden(_) ->
   Uri  = uri:path(<<"/test/auth">>, uri:new(?URI)),
   Sock = socket(Uri, {'GET', Uri, [{'Connection', 'keep-alive'}, {<<"Authorization">>, <<"public">>}]}),
   {http, Sock, {403, <<"Forbidden">>, _Head, _Env}} = knet:recv(Sock),
   {http, Sock, _} = knet:recv(Sock),
   {http, Sock, eof} = knet:recv(Sock).


restd_not_authorized(_) ->
   Uri  = uri:path(<<"/test/auth">>, uri:new(?URI)),
   Sock = socket(Uri, {'GET', Uri, [{'Connection', 'keep-alive'}]}),
   {http, Sock, {401, <<"Unauthorized">>, _Head, _Env}} = knet:recv(Sock),
   {http, Sock, _} = knet:recv(Sock),
   {http, Sock, eof} = knet:recv(Sock).


restd_cors(_) ->
   Origin = <<"http://example.org/">>,
   Uri    = uri:path(<<"/test/a">>, uri:new(?URI)),
   Sock   = socket(Uri, {'GET', Uri, [{'Connection', 'keep-alive'}, {'Origin', Origin}]}),
   {http, Sock, {200, <<"OK">>, Head, _Env}} = knet:recv(Sock),
   Origin = lens:get(lens:pair(<<"Access-Control-Allow-Origin">>), Head),
   <<"GET">> = lens:get(lens:pair(<<"Access-Control-Allow-Methods">>), Head), 
   {http, Sock, <<"restd">>} = knet:recv(Sock),
   {http, Sock, eof} = knet:recv(Sock).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

socket(Uri, {_, _, _} = Request) ->
   Sock = knet:socket(Uri),
   _    = knet:send(Sock, Request),
   {ioctl, b, Sock} = knet:recv(Sock),
   Sock.
