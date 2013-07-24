-module(echo).

-export([start/0]).

%%
%% start application
start() -> 
   applib:boot(?MODULE, filename:join([code:priv_dir(?MODULE), "echo.config"])).
