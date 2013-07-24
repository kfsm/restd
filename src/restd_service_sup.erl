%%
%%
-module(restd_service_sup).
-behaviour(supervisor).

-export([
	start_link/2,
	init/1
]).

%%
%%
start_link(Uid, Opts) ->
   {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, [Uid, Opts]),
   lists:foreach(
		fun(X) -> restd:register(Uid, X) end,
		opts:val(mod, [], Opts)
	),
	{ok, Sup}.
   
init([Uid, Opts]) -> 
   {ok,
      {
         {one_for_one, 4, 1800},
         [acceptor(Uid), listen(Uid, Opts)]
      }
   }.

%% acceptor factory
acceptor(Uid) ->
   {
      acceptor,
      {restd_acceptor_sup, start_link, [Uid]},
      permanent, 60000, supervisor, dynamic
   }.

%% listen socket
listen(Uid, Opts) ->
	Uri     = opts:val(uri,      Opts),
	Pool    = opts:val(pool, 10, Opts),
	{
		listen,
		{restd_acceptor, start_link, [Uid, Uri, Pool]},
		permanent, 60000, worker, dynamic
	}.
