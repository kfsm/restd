%% @doc
%%    accepts restd traffic and routes it to destination vhost
-module(restd_vhost).
-behaviour(pipe).

-export([
   start_link/4,
   init/1,
   free/2,
   'LISTEN'/3,
   'ACCEPT'/3,
   'STREAM'/3
]).

%%
%% default state
-record(fsm, {
   vhost = undefined :: atom(),
   uid   = undefined :: atom(),
   so    = undefined :: atom(),
   pid   = undefined :: pid()
}).


%%%------------------------------------------------------------------
%%%
%%% Factory
%%%
%%%------------------------------------------------------------------   

start_link(Service, Vhost, Uri, Opts) ->
   pipe:start_link(?MODULE, [Service, Vhost, Uri, Opts], []).

init([Service, Vhost, Uri, Opts]) ->
   {ok, _} = knet:bind(Uri, Opts),
   {ok, 'ACCEPT', 
      #fsm{
         vhost = Vhost,
         uid   = Service,
         so    = [Uri, [vhost|Opts]]
      }
   }.

free(_Reason, _State) ->
   ok.

%%%------------------------------------------------------------------
%%%
%%% LISTEN
%%%
%%%------------------------------------------------------------------   

'LISTEN'(_, _, State) ->
   {next_state, 'LISTEN', State}.

%%%------------------------------------------------------------------
%%%
%%% ACCEPT
%%%
%%%------------------------------------------------------------------   

%%
%%
'ACCEPT'({http, _, {_Mthd, _Uri, _Head, _Env}} = Req, Pipe, #fsm{vhost=vhost, uid=Service, so=SOpt}=State) ->
   {ok, Pid} = pipe:call(
      {Service, erlang:node(pg2:get_closest_pid(Service))}, 
      {accept,  SOpt}
   ),
   'STREAM'(Req, Pipe, State#fsm{pid = Pid});

'ACCEPT'({http, _, {_Mthd, Uri, _Head, _Env}} = Req, Pipe, #fsm{vhost=Mod, uid=Service, so=SOpt}=State) ->
   {ok, Pid} = pipe:call(
      {Service, Mod:whereis(Uri)}, 
      {accept,  SOpt}
   ),
   'STREAM'(Req, Pipe, State#fsm{pid = Pid});
   
'ACCEPT'(_, _, State) ->
   {next_state, 'ACCEPT', State}.


%%
%%
'STREAM'(Msg, Pipe, #fsm{pid = Pid}=State) ->
   case pipe:b(Pipe) of
      undefined -> pipe:send(Pid, Msg);
      _         -> pipe:b(Pipe, Msg)
   end,
   {next_state, 'STREAM', State}.


