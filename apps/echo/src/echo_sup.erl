%%
%%
-module(echo_sup).
-behaviour(supervisor).

-export([
   start_link/0, 
   init/1
]).

%%
-define(CHILD(Type, I),            {I,  {I, start_link,   []}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, permanent, 5000, Type, dynamic}).

%%
%%
start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).
   
init([]) -> 
   {ok,
      {
         {one_for_one, 4, 1800},
         [
            restd:spec(endpoints(), [{port, "http://*:8888"}])
         ]
      }
   }.

endpoints() ->
   [
      fun echo_restapi:ipaddr_json/1,
      fun echo_restapi:ipaddr_text/1,
      fun echo_restapi:user_agent/1,
      fun echo_restapi:headers/1,
      fun echo_restapi:get/1,
      fun echo_restapi:post/1,
      fun echo_restapi:put/1,
      fun echo_restapi:patch/1,
      fun echo_restapi:delete/1,
      fun echo_restapi:utf8/1,
      fun echo_restapi:deflate/1,
      fun echo_restapi:gzip/1,
      fun echo_restapi:compress/1,
      fun echo_restapi:status_code/1,
      fun echo_restapi:response_header/1
   ].
