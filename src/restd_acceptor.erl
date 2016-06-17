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
	uid       = undefined :: atom(),   %% service identity

	method    = undefined :: atom(),
	resource  = undefined :: atom(),   %% current resource handler (payload handling)
	request   = undefined :: any(),    %% current request
	content   = undefined :: any(),    %% current content handler
	q         = undefined :: datum:q() %% current queue
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
   {ok, 'ACCEPT', 
      #fsm{
         uid = Uid,
         q   = deq:new()
      }
   }.

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
   case prerouting(http, resource(Service, Mthd, Uri, Head, Env)) of
      {ok,   Request} ->
         {next_state, 'HANDLE', State#fsm{request = Request, q = deq:new()}};

      {error, Reason} ->
         pipe:a(Pipe, failure(Reason)),
         {next_state, 'ACCEPT', State}
   end;

'ACCEPT'({ws, _, {Mthd, Uri, Head, Env}}, Pipe, #fsm{uid = Service} = State) ->
   case prerouting(ws, resource(Service, Mthd, Uri, Head, Env)) of
      {ok,   Request} ->
         {next_state, 'WEBSOCK', State#fsm{request = Request, q = deq:new()}};

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

'HANDLE'({http, _Uri, Pckt}, _Pipe, State)
 when is_binary(Pckt) ->
   {next_state, 'HANDLE', 
   	State#fsm{
   		q = deq:enq(Pckt, State#fsm.q)
   	}
   }; 

'HANDLE'({http, _Uri, eof}, Pipe, #fsm{request = Req = #{accept := Type}, q = Q} = State) ->
   try
      Response = routing(Req, deq:list(Q)),
		case handle_response(Response, Type) of
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

'WEBSOCK'({ws, _, Msg}, Pipe, #fsm{request = #{id := Mod, accept := Accept} = Req} = State) ->
   try
      case Mod:recv(Accept, Msg, req(Req)) of
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

'WEBSOCK'(Msg, Pipe, #fsm{request = #{id := Mod, accept := Accept} = Req} = State) ->
   try
      case Mod:send(Accept, Msg, req(Req)) of
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
handle_response(stream, Type) ->
 	{200, [{'Content-Type', Type}, {'Transfer-Encoding', <<"chunked">>}]};

handle_response({stream, Heads}, Type) ->
	case lists:keyfind('Content-Type', 1, Heads) of
      false -> {200, [{'Transfer-Encoding', <<"chunked">>}, {'Content-Type', Type} | Heads]};
      _     -> {200, [{'Transfer-Encoding', <<"chunked">>} | Heads]}
   end; 	

handle_response({Code, {s, _, _}=Stream}, Type) ->
   {Code, [{'Content-Type', Type}, {'Transfer-Encoding', <<"chunked">>}], Stream};

handle_response({Code, Msg}, Type)
 when is_binary(Msg) ->
 	{Code, [{'Content-Type', Type}, {'Content-Length', size(Msg)}], Msg};

handle_response({Code, Msg}, Type)
 when is_list(Msg) ->
 	handle_response({Code, erlang:iolist_to_binary(Msg)}, Type);

handle_response({Code, Heads, {s, _, _}=Stream}, Type) ->
   case lists:keyfind('Content-Type', 1, Heads) of
      false -> {Code, [{'Content-Type', Type}, {'Transfer-Encoding', <<"chunked">>} | Heads], Stream};
      _     -> {Code, [{'Transfer-Encoding', <<"chunked">>} | Heads], Stream}
   end;  

handle_response({Code, Heads, Msg}, Type)
 when is_binary(Msg) ->
	case lists:keyfind('Content-Type', 1, Heads) of
      false -> {Code, [{'Content-Type', Type}, {'Content-Length', size(Msg)} | Heads], Msg};
      _     -> {Code, [{'Content-Length', size(Msg)} | Heads], Msg}
   end; 	
handle_response({Code, Heads, Msg}, Type)
 when is_list(Msg) ->
 	handle_response({Code, Heads, erlang:iolist_to_binary(Msg)}, Type);

handle_response(Code, Type)
 when is_atom(Code) orelse is_integer(Code) ->
   {Code, [{'Content-Type', Type}, {'Content-Length', 0}], <<>>}.


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
%% create new resource description
resource(Service, Mthd, Url, Head, Env) ->
   #{
      id => Service,
      method => Mthd,
      url => Url,
      header => Head,
      env => Env
   }.

%%
%% request pre-routing
prerouting(http, Request) ->
   do(Request, [
      fun is_resource_available/1,
      fun is_method_implemented/1,
      fun is_method_allowed/1,
      fun is_access_authorized/1,
      fun is_content_supported/1,
      fun is_content_acceptable/1,
      fun is_resource_exists/1
   ]);

prerouting(ws, Request) ->
   do(Request, [
      fun is_resource_available/1,
      fun is_method_allowed/1,
      fun is_access_authorized/1,
      fun is_content_supported/1,
      fun is_content_acceptable/1,
      fun is_resource_exists/1
   ]).


%%
%% request routing
routing(#{method := Mthd, accept := Accept} = Req, Inbound) ->
   f(Req, Mthd, Accept, erlang:iolist_to_binary(Inbound)).


%%
%% request failure
failure(Reason) ->
   lager:notice("[restd] request failed: ~p", [Reason]),
   handle_failure({error, Reason}).


%%
%% 
is_resource_available(#{id := Service, url := Url, env := LEnv} = Req) ->
   try
      #{
         resource := Mod, 
         export := Export, 
         env := GEnv
      } = hornlog:q(Service, uri:segments(Url), Url),
      {ok, Req#{id => Mod, export => Export, env => LEnv ++ GEnv}}
   catch _:_ ->
      {error, not_available}
   end.

%%
%%
is_method_implemented(#{export := Export, method := Mthd} = Req) ->
   case lists:keyfind(Mthd, 1, Export) of
      false ->
         {error, not_implemented};
      _ ->
         {ok, Req}
   end.

%%
%% 
is_method_allowed(#{method := Mthd} = Req) ->
   List = f(Req, allowed_methods),
   case lists:member(Mthd, List) of
      false -> 
         {error, not_allowed};
      true  -> 
         {ok, Req}
   end.

%%
%% 
is_access_authorized(#{method := Mthd} = Req) ->
   case f(Req, authorize, Mthd) of
      ok ->
         {ok, Req};
      _  ->
         {error, unauthorized}
   end.

%%
%% 
is_content_supported(#{header := Head} = Req) ->
   case opts:val('Content-Type', undefined, Head) of
      % content type is not defined
      undefined ->
         {ok, Req};

      % content type is defined by request 
      ContentType ->
         case content_type(ContentType, f(Req, content_accepted)) of
            [Value|_] ->
               {ok, Req#{content_type => Value}};
            _ ->
               {error, not_acceptable}
         end
   end.

%%
%% 
is_content_acceptable(#{header := Head} = Req) ->
   List = f(Req, content_provided),
   case
      lists:flatten(
         lists:map(
            fun(X) -> content_type(X, List) end,
            opts:val('Accept', [{'*', '*'}], Head)
         )
      )
   of
      [Value|_] ->
         {ok, Req#{accept => Value}};
      _ ->
         {error, not_acceptable}
   end.

%%
%
is_resource_exists(#{accept := Accept} = Req) -> 
   case f(Req, exists, Accept) of
      true ->
         {ok, Req};
      _    ->
         {error, not_found}
   end.

%%
%% match content type(s)
content_type({Major, Minor}, List) ->
   MajorF = fun({X, _}) -> 
      scalar:s(X)  =:= Major  
      orelse X     =:= '*'  
      orelse Major =:= '*' 
   end,
   MinorF = fun({_, X}) -> 
      scalar:s(X)  =:= Minor  
      orelse X     =:= '*'  
      orelse Minor =:= '*' 
   end,
   lists:filter(MinorF, lists:filter(MajorF, List)).

%%
%% error monad function binding 
%% fun(Request) -> Request.
do(X, [H | T]) ->
   case H(X) of
      {error, _} = Error ->
         Error;
      {ok, Y} ->
         do(Y, T)
   end;
do(X, []) ->
   {ok, X}.

%%
%% apply resource function
f(#{id := Mod, export := Export} = Req, Fun) ->
   case lists:keyfind(Fun, 1, Export) of
      false -> default(Fun);
      _     -> Mod:Fun(req(Req))
   end.

f(#{id := Mod, export := Export} = Req, Fun, X) ->
   case lists:keyfind(Fun, 1, Export) of
      false -> default(Fun);
      _     -> Mod:Fun(X, req(Req))
   end.

f(#{id := Mod, export := Export} = Req, Fun, X, Y) ->
   case lists:keyfind(Fun, 1, Export) of
      false -> default(Fun);
      _     -> Mod:Fun(X, Y, req(Req))
   end.


%%
%% build a resource context 
req(#{url := Url, header := Head, env := Env}) ->
   {Url, Head, Env}.

%%
%%
default(allowed_methods) -> ['GET', 'HEAD', 'OPTIONS'];
default(authorize) -> ok;
default(content_accepted) -> [{'*', '*'}];
default(content_provided) -> [];
default(exists) -> true.
