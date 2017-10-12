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
      echo_restapi:ipaddr_json(),
      echo_restapi:ipaddr_text(),
      echo_restapi:user_agent(),
      echo_restapi:headers(),
      echo_restapi:get(),
      echo_restapi:post(),
      echo_restapi:post_json(),
      echo_restapi:put(),
      echo_restapi:patch(),
      echo_restapi:delete(),
      echo_restapi:utf8(),
      echo_restapi:deflate(),
      echo_restapi:gzip(),
      echo_restapi:compress(),
      echo_restapi:status_code(),
      echo_restapi:response_header(),
      echo_restapi:redirect_n(),
      echo_restapi:cookies(),
      echo_restapi:stream(),
      echo_restapi:websocket(),

      restd_static:reader("/", echo),
      restd_static:reader("/_", echo)
   ].
