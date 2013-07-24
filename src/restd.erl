-module(restd).

-export([start/0]).
-export([
	register/2
]).

%%
%% start application
start() -> 
   applib:boot(?MODULE, []).

%%
%% start service
start_link(Uid, Opts) ->
	restd_service_sup:start_link(Uid, Opts).


%%
%% register resource
-spec(register/2 :: (atom(), atom()) -> ok).

register(Uid, Mod) ->
	pns:register({restd, Uid}, Mod:resource(), Mod).