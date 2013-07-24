%% @description
%%    acceptor factory
-module(restd_acceptor_sup).
-behaviour(supervisor).

-export([
   start_link/1, 
   init/1
]).

%%
%%
start_link(Uid) ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, [Uid]).
   
init([Uid]) -> 
   {ok,
      {
         {simple_one_for_one, 0, 3600},
         [child(Uid)]
      }
   }.

child(Uid) ->
   {
      undefined,
      {restd_acceptor, start_link, [Uid]},
      temporary, 5000, worker, dynamic
   }.
