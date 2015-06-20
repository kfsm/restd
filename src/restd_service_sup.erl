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
   {ok, _} = listen(opts:val(uri, undefined, Opts), Service, Opts),
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

%%
%% 
listen(Uri, Service, Opts) ->
   listen(opts:val(vhost, undefined, Opts), Uri, Service, Opts).

listen(undefined, undefined, Service, _Opts) ->
   %% spawn pure listener
   pipe:listen(Service, [
      {acceptor, {restd_acceptor, [Service]}}
   ]);

listen(undefined, Uri, Service, Opts) ->
   %% spawn restd listener
   Sock = opts:val(sock, [], Opts),
   knet:listen(Uri, [
      {acceptor, {restd_acceptor, [Service]}}, 
      opts:get(pool,    10, Opts),
      opts:get(backlog, 25, Opts),
      nobind | Sock
   ]);

listen(Vhost, Uri, Service, Opts) ->
   %% spawn vhost proxy
   Sock = opts:val(sock, [], Opts),
   knet:listen(Uri, [
      {acceptor, {restd_vhost, [Service, Vhost]}},
      opts:get(pool,    10, Opts),
      opts:get(backlog, 25, Opts),
      nobind | Sock
   ]).
   





