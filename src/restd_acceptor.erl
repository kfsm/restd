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
	'STREAM'/3,
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
   case prerouting({Service, Mthd, Uri, Head, Env}) of
      {ok,   Request} ->
         {next_state, 'HANDLE', State#fsm{request = Request, q = deq:new()}};

      {error, Reason} ->
         pipe:a(Pipe, failure(Reason)),
         {next_state, 'ACCEPT', State}
   end;

'ACCEPT'({ws, _, {Mthd, Uri, Head, Env}}, Pipe, #fsm{uid = Service} = State) ->
   case prerouting({Service, Mthd, Uri, Head, Env}) of
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

'HANDLE'({http, _Uri, eof}, Pipe, #fsm{request = Req, q = Q} = State) ->
   try
      {Type, Response} = routing(Req, deq:list(Q)),
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

'STREAM'({http, _Url, eof}, _Pipe, S) ->
	% @todo: use http eof as a trigger for resource invocation
   {next_state, 'STREAM', S};

%%
%% handles message from "external" processes, pipe binds either
%%  "process" <-> "http" (if external process support pipe protocol)
%%  "http"    <-> undefined
'STREAM'(Msg, Pipe, #fsm{resource=Mod, request=Req, content=Type}=S) ->
	try
		case Mod:stream(Type, Req, pipe:a(Pipe), Msg) of
			eof  -> 
				pipe:send(pipe_sink(Pipe), <<>>),
				{next_state, 'ACCEPT', S};
			undefined ->
				{next_state, 'STREAM', S};
			Http -> 
				pipe:send(pipe_sink(Pipe), Http),
				{next_state, 'STREAM', S}
		end
	catch _:Reason ->
   	lager:notice("restd request error ~p: ~p", [Reason, erlang:get_stacktrace()]),
   	pipe:send(pipe_sink(Pipe), handle_failure(Reason)),
		{next_state, 'ACCEPT', S}
	end.

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

'WEBSOCK'({ws, _, Msg}, Pipe, #fsm{request = Req} = State) ->
   try
      case routing(Req, [Msg]) of
         {_, ok} ->
            {next_state, 'WEBSOCK', State};

         {_, {ok, Http}} ->
            _ = pipe:a(Pipe, Http),
            {next_state, 'WEBSOCK', State}
      end
   catch _:Reason ->
      lager:notice("restd request error ~p: ~p", [Reason, erlang:get_stacktrace()]),
      pipe:a(Pipe, handle_failure(Reason)),
      {stop, normal, State}
   end;

'WEBSOCK'(Msg, Pipe, #fsm{request = Req} = State) ->
   try
      {Mod, _Mthd, Url, Head, Env} = Req,
      {_, Accept} = lists:keyfind('Accept', 1, Env),
      case Mod:stream(Accept, {Url, Head, Env}, pipe:a(Pipe), Msg) of
         eof  -> 
            {stop, normal,  State};
         undefined ->
            {next_state, 'WEBSOCK', State};
         Http -> 
            pipe:send(pipe_sink(Pipe), Http),
            {next_state, 'WEBSOCK', State}
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
%% request pre-routing
prerouting(Request) ->
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
routing({Mod, Mthd, Url, Head, Env}, Pckt) -> 
   {_, Accept} = lists:keyfind('Accept', 1, Env),
   Content  = erlang:iolist_to_binary(Pckt),
   {Accept, Mod:Mthd(Accept, {Url, Head, Env}, Content)}.


%%
%% request failure
failure(Reason) ->
   lager:notice("[restd] request failed: ~p", [Reason]),
   handle_failure({error, Reason}).


%%
%% check if resource available (resource handler is registered)
is_resource_available({Service, Mthd, Url, Head, LEnv}) ->
   try
      {Mod, GEnv} = hornlog:q(Service, uri:segments(Url), Url),
      {ok, {Mod, Mthd, Url, Head, LEnv ++ GEnv}}
   catch _:_ ->
      {error, not_available}
   end.

%%
%% check if method is allowed
is_method_allowed({Mod, Mthd, Url, Head, Env} = Req) ->
   try
      List = Mod:allowed_methods({Url, Head, Env}), 
      case lists:member(Mthd, List) of
         false -> 
            {error, not_allowed};
         true  -> 
            {ok, Req}
      end
   catch _:_ ->
      {error, not_implemented}
   end.

%%
%% check if request is authorized
is_access_authorized({Mod, Mthd, Url, Head, Env} = Req) ->
   case erlang:function_exported(Mod, authorize, 2) of
      %
      true  ->
         case Mod:authorize(Mthd, {Url, Head, Env}) of
            ok ->
               {ok, Req};
            _  ->
               {error, unauthorized}
         end;

      % check is not implemented, request is authorized by default
      false ->
         {ok, Req}
   end.

%%
%% check if resource exists
is_content_supported({Mod, Mthd, Url, Head, Env} = Req) ->
   case opts:val('Content-Type', undefined, Head) of
      % content type is not defined
      undefined ->
         {ok, Req};

      % content type is defined by request 
      {Major, Minor} ->
         try
            Content = Mod:content_accepted({Url, Head, Env}),
            MajorLs = lists:filter(fun({X, _}) -> scalar:s(X) =:= Major orelse X =:= '*' end, Content),
            MinorLs = lists:filter(fun({_, X}) -> scalar:s(X) =:= Minor orelse X =:= '*' end, MajorLs),
            {ok, {Mod, Mthd, Url, Head, [{'Content-Type', hd(MinorLs)}|Env]}}
         catch _:_ ->
            {error, not_acceptable}
         end
   end.

%%
%% check if content is accepted by remote entity
is_content_acceptable({Mod, Mthd, Url, Head, Env}) ->
   try
      Content = Mod:content_provided({Url, Head, Env}),
      Accept  = lists:flatten(
         lists:map(
            fun({Major, Minor}) ->
               MajorLs = lists:filter(fun({X, _}) -> scalar:s(X) =:= Major orelse X =:= '*' orelse Major =:= '*' end, Content),
               lists:filter(fun({_, X}) -> scalar:s(X) =:= Minor orelse X =:= '*' orelse Minor =:= '*' end, MajorLs)
            end,
            opts:val('Accept', [{'*', '*'}], Head)
         )
      ),
      {ok, {Mod, Mthd, Url, Head, [{'Accept', hd(Accept)}|Env]}}
   catch _:_ ->
      {error, not_acceptable}
   end.

%%
%% check if request is authorized
%% @todo: there is diff semantic for put / post vs get
is_resource_exists({Mod, _Mthd, Url, Head, Env} = Req) -> 
   case erlang:function_exported(Mod, exists, 2) of
      %
      true  ->
         {_, Accept} = lists:keyfind('Accept', 1, Env),
         case Mod:exists(Accept, {Url, Head, Env}) of
            true ->
               {ok, Req};
            _  ->
               {error, not_found}
         end;

      % check is not implemented, resource exists by default
      false ->
         {ok, Req}
   end.

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


% %%
% %% validate resource request and return request environment
% validate_request({Mthd, Uri, Heads, Env}, Service) ->
% 	{Mod, GEnv} = is_resource_available(Mthd, {Uri, Heads, Env}, Service),
% 	Req = {Uri, Heads, GEnv ++ Env}, 
% 	_   = is_method_allowed(Mthd, Req, Mod),
% 	_   = is_request_authorized(Mthd, Req, Mod),
% 	Type= is_content_supported(Mthd, Req, Mod),
% 	%% @todo accept- langauge, charset, encoding 
%    _   = is_resource_exists(Mthd, Type, Req, Mod),  
%    {Mod, Type, Req}.



% %% check if two uri segments equivalent
% is_equiv(['*'], _) ->
% 	true;
% is_equiv(_, ['*']) ->
% 	true;
% is_equiv([H|A], [_|B])
%  when H =:= '_' orelse H =:= '*' ->
% 	is_equiv(A, B);
% is_equiv([_|A], [H|B])
%  when H =:= '_' orelse H =:= '*' ->
% 	is_equiv(A, B);
% is_equiv([A|AA], [B|BB]) ->
% 	case eq(A, B) of
% 		true  -> is_equiv(AA, BB);
% 		false -> false
% 	end;
% is_equiv([], []) ->
%  	true;
% is_equiv(_,   _) ->
%  	false.

% %% check if two path elements are equal
% eq(A, B)
%  when is_atom(A), is_binary(B) ->
%  	atom_to_binary(A, utf8) =:= B;
% eq(A, B)
%  when is_binary(A), is_atom(B) ->
%  	A =:= atom_to_binary(B, utf8);
% eq(A, B) ->
% 	A =:= B.


% %%
% %% check is http method carries any payload
% is_payload_method('GET')     -> false;
% is_payload_method('HEAD')    -> false;
% is_payload_method('OPTIONS') -> false;
% is_payload_method('TRACE')   -> false;
% is_payload_method('DELETE')  -> false;
% is_payload_method(_)         -> true.



