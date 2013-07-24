%%
%%
-module(restd_sup).
-behaviour(supervisor).

-export([
   start_link/0, 
   init/1
]).

%%
%%
start_link() ->
   {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
   lists:foreach(
   	fun default_service/1,
   	proplists:delete(included_applications, application:get_all_env())
   ),
   {ok, Sup}.
   
init([]) -> 
   {ok,
      {
         {one_for_one, 4, 1800},
         []
      }
   }.

%%
%% start default rest services
default_service({Uid, Opts}) ->
	{ok, _} = supervisor:start_child(?MODULE, {
		Uid,
		{restd_service_sup, start_link, [Uid, Opts]},
		permanent, 60000, supervisor, dynamic
	}).

