%%
%%   Copyright (c) 2012 - 2015, Dmitry Kolesnikov
%%   Copyright (c) 2012 - 2015, Mario Cardona
%%   All Rights Reserved.
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @description
%%    acceptor process
-module(restd_acceptor).
-behaviour(pipe).

-include("restd.hrl").

-export([
	start_link/3,
	init/1,
	free/2,
	ioctl/2,
	'LISTEN'/3,
	'ACCEPT'/3,
	'HANDLE'/3,
	% 'STREAM'/3,
   'WEBSOCK'/3
]).

%% default state
-record(fsm, {
   uid       = undefined :: atom(),    %% service identity
   resource  = undefined :: _,         %% restapi handler
   q         = undefined :: datum:q()  %% current queue

	% method    = undefined :: atom(),
	% resource  = undefined :: atom(),   %% current resource handler (payload handling)
	% request   = undefined :: any(),    %% current request
	% content   = undefined :: any(),    %% current content handler
}).

%%%------------------------------------------------------------------
%%%
%%% Factory
%%%
%%%------------------------------------------------------------------   

start_link(Uid, Uri, Opts) ->
	pipe:start_link(?MODULE, [Uid, Uri, Opts], []).

init([Uid, Uri, Opts]) ->
   knet:bind(Uri, Opts),
   {ok, 'ACCEPT', #fsm{uid = Uid}}.

free(_Reason, _State) ->
	ok.

%%
%%
ioctl(_, _) ->
	throw(not_implemented).

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
'ACCEPT'({http, _, {Mthd, Uri, Head, Env}}, Pipe, #fsm{uid = Service} = State) ->
   
   case
      restd_resource:do(
         restd_resource:new(Service, {Mthd, Uri, Head}, Env),
         prerouting_http()
      )
   of
      {ok,  Resource} ->
         {next_state, 'HANDLE', State#fsm{resource = Resource, q = deq:new()}};

      {error, Reason} ->
         pipe:a(Pipe, failure(Reason)),
         {next_state, 'ACCEPT', State}
   end;

'ACCEPT'({ws, _, {Mthd, Uri, Head, Env}}, Pipe, #fsm{uid = Service} = State) ->
   case
      restd_resource:do(
         restd_resource:new(Service, {Mthd, Uri, Head}, Env),
         prerouting_ws()
      )
   of
      {ok, Resource} ->
         {next_state, 'WEBSOCK', State#fsm{resource = Resource, q = deq:new()}};

      {error, Reason} ->
         pipe:a(Pipe, failure(Reason)),
         {next_state, 'ACCEPT', State}
   end;

%%
%% @todo: ACCEPT
%% handle({ws, _Sock, {_Mthd, _Url, _Head, _Env}}, _Pipe, State)
%% handle({ws, _Sock, {terminated, _Reason}}, _Pipe, State)
%% handle({ws, _Sock, Msg}, Pipe, State)
%% 
%% {http,<0.135.0>,eof}

'ACCEPT'(_, _, State) ->
	{next_state, 'ACCEPT', State}.

%%%------------------------------------------------------------------
%%%
%%% HANDLE
%%%
%%%------------------------------------------------------------------   

'HANDLE'({http, _Uri, Pckt}, _Pipe, #fsm{q = Q} = State)
 when is_binary(Pckt) ->
   {next_state, 'HANDLE', 
   	State#fsm{q = deq:enq(Pckt, Q)}
   }; 

'HANDLE'({http, _Uri, eof}, Pipe, #fsm{resource = Resource, q = Q} = State) ->
   try
		case handle_response(restd_resource:restapi(Resource, deq:list(Q)), Resource#resource.head) of
			%% no-payload, streaming
			{_Code, _Heads} = Http ->
 				_ = pipe:a(Pipe, Http),
				{next_state, 'STREAM', State#fsm{q = deq:new()}};
         %% there is a payload (lazy stream)
         {Code, Heads, {s, _, _}=Stream} ->
            _ = pipe:a(Pipe, {Code, Heads}),
            stream:foreach(
               fun(X) -> pipe:a(Pipe, X) end,
               Stream
            ),
            pipe:a(Pipe, <<>>),
            {next_state, 'ACCEPT', State#fsm{q = deq:new()}};
			%% there is a payload
			{_Code, _Heads, _Msg} = Http ->
 				_ = pipe:a(Pipe, Http),
				{next_state, 'ACCEPT', State#fsm{q = deq:new()}}
		end
   catch _:Reason ->
   	lager:notice("restd request error ~p: ~p", [Reason, erlang:get_stacktrace()]),
   	pipe:a(Pipe, handle_failure(Reason)),
		{next_state, 'ACCEPT', State}
   end.

%%%------------------------------------------------------------------
%%%
%%% STREAM
%%%
%%%------------------------------------------------------------------   

% 'STREAM'({http, _Url, eof}, _Pipe, S) ->
% 	% @todo: use http eof as a trigger for resource invocation
%    {next_state, 'STREAM', S};

% %%
% %% handles message from "external" processes, pipe binds either
% %%  "process" <-> "http" (if external process support pipe protocol)
% %%  "http"    <-> undefined
% 'STREAM'(Msg, Pipe, #fsm{resource=Mod, request=Req, content=Type}=S) ->
% 	try
% 		case Mod:stream(Type, Req, pipe:a(Pipe), Msg) of
% 			eof  -> 
% 				pipe:send(pipe_sink(Pipe), <<>>),
% 				{next_state, 'ACCEPT', S};
% 			undefined ->
% 				{next_state, 'STREAM', S};
% 			Http -> 
% 				pipe:send(pipe_sink(Pipe), Http),
% 				{next_state, 'STREAM', S}
% 		end
% 	catch _:Reason ->
%    	lager:notice("restd request error ~p: ~p", [Reason, erlang:get_stacktrace()]),
%    	pipe:send(pipe_sink(Pipe), handle_failure(Reason)),
% 		{next_state, 'ACCEPT', S}
% 	end.

%%
pipe_sink(Pipe) ->
	case pipe:b(Pipe) of
		undefined -> pipe:a(Pipe);
		Pid       -> Pid
	end.

%%%------------------------------------------------------------------
%%%
%%% WebSock
%%%
%%%------------------------------------------------------------------   

'WEBSOCK'({ws, _, {terminated, _}}, _Pipe, State) ->
   {stop, normal, State};

'WEBSOCK'({ws, _, Msg}, Pipe, #fsm{resource = Resource} = State) ->
   try
      case restd_resource:recv(Resource, Msg) of
         ok ->
            {next_state, 'WEBSOCK', State};
         {ok, Pckt} ->
            _ = pipe:a(Pipe, Pckt),
            {next_state, 'WEBSOCK', State}
      end
   catch _:Reason ->
      lager:notice("restd request error ~p: ~p", [Reason, erlang:get_stacktrace()]),
      pipe:a(Pipe, handle_failure(Reason)),
      {stop, normal, State}
   end;

'WEBSOCK'(Msg, Pipe, #fsm{resource = Resource} = State) ->
   try
      case restd_resource:send(Resource, Msg) of
         ok ->
            {next_state, 'WEBSOCK', State};
         {ok, Pckt} ->
            pipe:send(pipe_sink(Pipe), Pckt),
            {next_state, 'WEBSOCK', State};
         eof  -> 
            {stop, normal,  State}
      end
   catch _:Reason ->
      lager:notice("restd request error ~p: ~p", [Reason, erlang:get_stacktrace()]),
      pipe:send(pipe_sink(Pipe), handle_failure(Reason)),
      {next_state, 'WEBSOCK', State}
   end.



%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------   


%%
%%
handle_response(stream, Head) ->
 	{200, [{'Transfer-Encoding', <<"chunked">>} | Head]};

handle_response({stream, HeadB}, HeadA) ->
   %% @todo: use orddict + merge
   {200, [{'Transfer-Encoding', <<"chunked">>} | HeadB] ++ HeadA};

handle_response({Code, {s, _, _}=Stream}, Head) ->
   {Code, [{'Transfer-Encoding', <<"chunked">>}|Head], Stream};

handle_response({Code, Msg}, Head)
 when is_binary(Msg) ->
 	{Code, [{'Content-Length', size(Msg)}|Head], Msg};

handle_response({Code, Msg}, Head)
 when is_list(Msg) ->
 	handle_response({Code, erlang:iolist_to_binary(Msg)}, Head);

handle_response({Code, HeadB, {s, _, _}=Stream}, HeadA) ->
   %% @todo: use orddict + merge
   {Code, [{'Transfer-Encoding', <<"chunked">>} | HeadB] ++ HeadA, Stream};

handle_response({Code, HeadB, Msg}, HeadA)
 when is_binary(Msg) ->
   %% @todo: use orddict + merge
   {Code, [{'Content-Length', size(Msg)} | HeadB] ++ HeadA, Msg};

handle_response({Code, Heads, Msg}, Head)
 when is_list(Msg) ->
 	handle_response({Code, Heads, erlang:iolist_to_binary(Msg)}, Head);

handle_response(Code, Head)
 when is_atom(Code) orelse is_integer(Code) ->
   {Code, [{'Content-Length', 0}|Head], <<>>}.


%%
%% failure on HTTP request
handle_failure({badmatch, {error, Reason}}) ->
   {Reason, [{'Content-Type', {text, plain}}, {'Content-Length', 0}], <<>>};
handle_failure({error, Reason}) -> 
   {Reason, [{'Content-Type', {text, plain}}, {'Content-Length', 0}], <<>>};
handle_failure(badarg) ->
   {badarg, [{'Content-Type', {text, plain}}, {'Content-Length', 0}], <<>>};
handle_failure({badarg, _}) ->
   {badarg, [{'Content-Type', {text, plain}}, {'Content-Length', 0}], <<>>};
handle_failure(Reason) ->
   lager:error("restd failed: ~p ~p", [Reason, erlang:get_stacktrace()]),
   {500,    [{'Content-Type', {text, plain}}, {'Content-Length', 0}], <<>>}.




%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------   

%%
%% request pre-routing
prerouting_http() ->
   [
      fun restd_resource:is_available/1,
      fun restd_resource:is_method_implemented/1,
      fun restd_resource:is_method_allowed/1,
      fun restd_resource:is_access_authorized/1,
      fun restd_resource:is_content_supported/1,
      fun restd_resource:is_content_acceptable/1,
      fun restd_resource:is_resource_exists/1,
      fun restd_resource:is_cors_allowed/1
   ].

prerouting_ws() ->
   [
      fun restd_resource:is_available/1,
      fun restd_resource:is_method_allowed/1,
      fun restd_resource:is_access_authorized/1,
      fun restd_resource:is_content_supported/1,
      fun restd_resource:is_content_acceptable/1,
      fun restd_resource:is_resource_exists/1      
   ].

%%
%% request failure
failure(Reason) ->
   lager:notice("[restd] request failed: ~p", [Reason]),
   handle_failure({error, Reason}).


