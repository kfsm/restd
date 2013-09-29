%%
%%
-module(restd_service_sup).
-behaviour(supervisor).

-export([
	start_link/2,
	init/1
]).

%%
-define(CHILD(Type, I),            {I,  {I, start_link,   []}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, permanent, 5000, Type, dynamic}).


%%
%%
start_link(Service, Opts) ->
	{ok, _} = knet:listen(opts:val(uri, Opts), [
   	{acceptor, {restd_acceptor, [Service]}}, 
   	opts:get(pool,    10, Opts),
   	opts:get(backlog, 25, Opts),
   	nobind
   ]),
   lists:foreach(
		fun(X) -> config(Service, X) end,
		opts:val(mod, [], Opts)
	),
	supervisor:start_link(?MODULE, []).
   
init([]) -> 
   {ok,
      {
         {one_for_one, 4, 1800},
         []
      }
   }.

%%
%%
config(Service, {Uri, Mod}) ->
   restd:register(Service, Uri, Mod);

config(Service, {Uri, Mod, Env}) ->
   restd:register(Service, Uri, Mod, Env).




