-module(restd_app).
-behaviour(application).

-export([
   start/2,
   stop/1
]).

start(_Type, _Args) -> 
	_ = ets:new(restd, [named_table, public, duplicate_bag]),
   restd_sup:start_link(). 

stop(_State) ->
   ok.