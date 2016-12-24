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
-compile({parse_transform, category}).
-compile({parse_transform, partial}).

-behaviour(pipe).


-export([
   start_link/3,
   init/1,
   free/2,
   'LISTEN'/3,
   'ACCEPT'/3,
   'HTTP'/3,
   'WEBSOCK'/3
   % 'STREAM'/3,
]).
 
%% resource idle 
-record(idle, {
   route   = undefined :: atom()         %% routing table
}).

%% http connection is established
-record(http, {
   route  = undefined :: atom()          %% routing table
  ,mthd   = undefined :: _
  ,uri    = undefined :: uri:uri()
  ,head   = undefined :: _
  ,env    = undefined :: _
  ,q      = undefined :: datum:q()       %% current queue
}).

%% http request is mapped to resource 
-record(rest, {
   mod    = undefined :: atom()          %% resource implementation
  ,export = undefined :: _               %% resource exports
  ,mthd   = undefined :: _
  ,uri    = undefined :: uri:uri()
  ,head   = undefined :: _
  ,env    = undefined :: _
}).

%%%------------------------------------------------------------------
%%%
%%% Factory
%%%
%%%------------------------------------------------------------------   

start_link(Route, Uri, Opts) ->
   pipe:start_link(?MODULE, [Route, Uri, Opts], []).

init([Route, Uri, Opts]) ->
   knet:bind(Uri, Opts),
   {ok, 'ACCEPT', #idle{route = Route}}.

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
'ACCEPT'({http, _, {Mthd, Uri, Head, Env}}, _Pipe, #idle{route = Route}) ->
   {next_state, 'HTTP', 
      #http{
         route = Route,
         mthd  = Mthd,
         uri   = Uri,
         head  = Head,
         env   = Env,
         q     = deq:new()
      }
   };

'ACCEPT'({ws, _, {Mthd, Uri, Head, Env}}, _Pipe, #idle{route = Route}) ->
   {next_state, 'WEBSOCK', 
      #http{
         route = Route,
         mthd  = Mthd,
         uri   = Uri,
         head  = Head,
         env   = Env,
         q     = deq:new()
      }
   }.

%%%------------------------------------------------------------------
%%%
%%% HTTP
%%%
%%%------------------------------------------------------------------   

'HTTP'({http, _Uri, eof}, Pipe, #http{route = Route, q = Q} = State) ->
   case rest(State#http{q = undefined}, deq:list(Q)) of
      {ok, {s, _, _} = Http} ->
         streams:foreach(pipe:a(Pipe, _), Http);

      {ok, Http} ->
         lists:foreach(pipe:a(Pipe, _), Http);

      {error, {_, _, _} = Error} ->
         pipe:a(Pipe, Error);

      {error, {Code, _} = Error} ->
         {ok, Http} = packetize({application, json}, {Code, jsx:encode([Error])}),
         lists:foreach(pipe:a(Pipe, _), Http)         
   end,
   {next_state, 'ACCEPT', #idle{route = Route}};      


'HTTP'({http, _Uri, Pckt}, _Pipe, #http{q = Q} = State)
 when is_binary(Pckt) ->
   %% @todo: limit on entity, return 413
   {next_state, 'HTTP', 
      State#http{q = deq:enq(Pckt, Q)}
   }.

%%%------------------------------------------------------------------
%%%
%%% WEBSOCK
%%%
%%%------------------------------------------------------------------   

'WEBSOCK'({ws, _, _} = Msg, Pipe, #http{} = State) ->
   case stream(State) of
      {ok, #rest{} = Rest} ->
         'WEBSOCK'(Msg, Pipe, Rest);

      {error, Error} ->
         % web-socket is already established we can send only error and terminate connection
         pipe:a(Pipe, jsx:encode([Error])),
         {stop, normal, State} 
   end;

'WEBSOCK'({ws, _, {terminated, _}}, _Pipe, State) ->
   {stop, normal, State};

'WEBSOCK'({ws, _, Msg}, Pipe, #rest{mod = Mod, head = Head, env = Env} = State) ->
   EgType = opts:val('Accept', Env),
   InType = opts:val('Content-Type', undefined, Head),
   case Mod:recv({EgType, InType}, Msg, req(State)) of
      ok ->
         {next_state, 'WEBSOCK', State};
      {ok, Pckt} ->
         _ = pipe:a(Pipe, Pckt),
         {next_state, 'WEBSOCK', State}
   end;

'WEBSOCK'(Msg, Pipe, #rest{mod = Mod, head = Head, env = Env} = State) ->
   EgType = opts:val('Accept', Env),
   InType = opts:val('Content-Type', undefined, Head),
   case Mod:send({EgType, InType}, Msg, req(State)) of
      ok ->
         {next_state, 'WEBSOCK', State};
      {ok, Pckt} ->
         pipe:send(pipe_sink(Pipe), Pckt),
         {next_state, 'WEBSOCK', State};
      eof  -> 
         {stop, normal,  State}
   end.

pipe_sink(Pipe) ->
   case pipe:b(Pipe) of
      undefined -> pipe:a(Pipe);
      Pid       -> Pid
   end.

%%%------------------------------------------------------------------
%%%
%%% REST
%%%
%%%------------------------------------------------------------------   

%%
%%
rest(#http{} = Http, Entity) ->
   [$^||
      %%
      %% request routing
      resource(Http),
      resource(_, Http),
      is_method_implemented(_),
      is_method_allowed(_),
      is_access_authorized(_),
      is_content_supported(_),
      is_cors_allowed(_),
      %% @todo: terminate OPTION request here with 200
      %%
      %% content negotiation
      is_content_acceptable(_),
      is_language_acceptable(_),
      is_charset_acceptable(_),
      is_encoding_acceptable(_),
      %%
      %% resource negotiation
      is_resource_exists(_),
      is_etags_matched(_),
      is_modified(_),
      %%
      %% execute REST call
      execute(_, Entity)
   ].

%%
%%
stream(#http{} = Http) ->
   [$^||
      %%
      %% request routing
      resource(Http),
      resource(_, Http),
      is_method_allowed(_),
      is_access_authorized(_),
      is_content_supported(_),
      %%
      %% content negotiation
      is_content_acceptable(_),
      is_language_acceptable(_),
      is_charset_acceptable(_),
      is_encoding_acceptable(_),
      %%
      %% resource negotiation
      is_resource_exists(_),
      is_etags_matched(_),
      is_modified(_)
   ].

%%
%%
-spec resource(#http{}) -> {ok, #rest{}} | {error, {not_available, _}}.

resource(#http{route = Route, uri = Uri, env = LEnv}) ->
   try
      #{
         resource := Mod, 
         export   := Export, 
         env      := GEnv
      } = hornlog:q(Route, uri:segments(Uri), Uri),
      {ok, #rest{mod = Mod, export = Export, env = LEnv ++ GEnv}}
   catch _:_ ->
      {error, {not_available, uri:s(Uri)}}
   end.

resource(#rest{} = Rest, #http{mthd = Mthd, uri = Uri, head = Head}) ->
   {ok, Rest#rest{mthd = Mthd, uri = Uri, head = Head}}.


%%
%%
-spec is_method_implemented(#rest{}) -> {ok, #rest{}} | {error, {not_implemented, _}}.

is_method_implemented(#rest{export = Export, mthd = Mthd} = Rest) ->
   case lists:keyfind(Mthd, 1, Export) of
      false ->
         {error, {not_implemented, scalar:s(Mthd)}};
      _ ->
         {ok, Rest}
   end.


%%
%% 
-spec is_method_allowed(#rest{}) -> {ok, #rest{}} | {error, {not_allowed, _}}.

is_method_allowed(#rest{mthd = Mthd} = Rest) ->
   List = f(Rest, allowed_methods),
   case lists:member(Mthd, List) of
      false -> 
         {error, {not_allowed, scalar:s(Mthd)}};
      true  -> 
         {ok, Rest}
   end.

%%
%%
-spec is_access_authorized(#rest{}) -> {ok, #rest{}} | {error, {unauthorized, _}}.
 
is_access_authorized(#rest{mthd = Mthd, uri = Uri} = Rest) ->
   case f(Rest, authorize, Mthd) of
      ok ->
         {ok, Rest};
      forbidden ->
         {error, {forbidden, scalar:s(Uri)}};
      _  ->
         {error, {unauthorized, scalar:s(Uri)}}
   end.

%%
%%
-spec is_cors_allowed(#rest{}) -> {ok, #rest{}} | {error, {unsupported, _}}.

is_cors_allowed(#rest{head = Head} = Rest) ->
   case lists:keyfind(<<"Origin">>, 1, Head) of
      %% this is not a CORS request 
      false ->
         {ok, Rest};

      {_, Origin} ->
         CORS = f(Rest, cors),
         {ok, Rest#rest{head = [{'Access-Control-Allow-Origin', Origin}|CORS] ++ Head}}
   end.

%%
%%
-spec is_content_supported(#rest{}) -> {ok, #rest{}} | {error, {unsupported, _}}.

is_content_supported(#rest{head = Head} = Rest) ->
   case opts:val('Content-Type', undefined, Head) of
      undefined -> 
         {ok, Rest};

      Type ->
         Accept = f(Rest, content_accepted),
         case restd:negotiate(Type, Accept) of
            [] ->
               {error, {unsupported, mimetype(Type)}};
            _  ->
               {ok, Rest}
         end
   end.

%%
%%
-spec is_content_acceptable(#rest{}) -> {ok, #rest{}} | {error, {not_acceptable, _}}.

is_content_acceptable(#rest{head = Head, env = Env} = Rest) ->
   Provide = f(Rest, content_provided),
   Accept  = opts:val('Accept', [{'*', '*'}], Head),
   case
      lists:flatmap(restd:negotiate(_, Provide), Accept)
   of
      [{_, _} = Type | _] ->
         {ok, Rest#rest{env = [{'Accept', Type}|Env]}};
      _ ->
         {error, {not_acceptable, [mimetype(X) || X <- Accept]}}
   end.


%%
%%
-spec is_language_acceptable(#rest{}) -> {ok, #rest{}} | {error, {not_acceptable, _}}.

is_language_acceptable(#rest{} = Rest) ->
   {ok, Rest}.

%%
%%
-spec is_charset_acceptable(#rest{}) -> {ok, #rest{}} | {error, {not_acceptable, _}}.

is_charset_acceptable(#rest{} = Rest) ->
   {ok, Rest}.

%%
%%
-spec is_encoding_acceptable(#rest{}) -> {ok, #rest{}} | {error, {not_acceptable, _}}.

is_encoding_acceptable(#rest{} = Rest) ->
   {ok, Rest}.


%%
%%
-spec is_resource_exists(#rest{}) -> {ok, #rest{}} | {error, {not_found, _}}.

is_resource_exists(#rest{uri = Uri, env = Env} = Rest) ->
   {_, Accept} = lists:keyfind('Accept', 1, Env),
   case f(Rest, exists, Accept) of
      true ->
         {ok, Rest};
      _    ->
         {error, {not_found, uri:path(Uri)}}
   end.

%%
%%
-spec is_etags_matched(#rest{}) -> {ok, #rest{}} | {error, {not_found, _}}.

is_etags_matched(#rest{} = Rest) ->
   {ok, Rest}.

%%
%%
-spec is_modified(#rest{}) -> {ok, #rest{}} | {error, {not_found, _}}.

is_modified(#rest{} = Rest) ->
   {ok, Rest}.

%%
%%
-spec execute(#rest{}, _) -> {ok, {_, _, _}} | {error, {atom(), _}}.

execute(#rest{mthd = Mthd, head = Head, env = Env} = Rest, Entity) ->
   EgType = opts:val('Accept', Env),
   InType = opts:val('Content-Type', undefined, Head),
   % REST call returns either Http Type or Xor 
   case f(Rest, Mthd, {EgType, InType}, Entity) of
      % Xor type
      {ok, {_, _} = Http} -> 
         packetize(EgType, Http);
      {ok, {_, _, _} = Http} -> 
         packetize(EgType, Http);
      {error, Reason} -> 
         {error, fail(Rest, Reason)};
      % Http type
      Http -> 
         packetize(EgType, Http)
   end.

packetize(Type, {Code, Entity}) ->
   packetize(Type, {Code, [], Entity});

packetize(Type, {Code, Head0, Entity}) 
 when is_binary(Entity) ->
   Head1 = [$.|| 
      add_head('Content-Type', Type, Head0), 
      add_head('Content-Length', size(Entity), _)
   ],
   {ok, [{Code, Head1, Entity}]};

packetize(Type, {Code, Head0, Entity})
 when is_list(Entity) ->
   case lists:keyfind('Content-Length', 1, Head0) of
      false ->
         Head1 = [$.||
            add_head('Content-Type', Type, Head0),
            add_head('Transfer-Encoding', <<"chunked">>, _) 
         ],
         {ok, [{Code, Head1}|Entity] ++ [<<>>]};
      _ ->
         Head1 = add_head('Content-Type', Type, Head0),
         {ok, [{Code, Head1, Entity}]}
   end;

packetize(Type, {Code, Head0, {s, _, _} = Entity}) ->
   Head1 = [$.||
      add_head('Content-Type', Type, Head0),
      add_head('Transfer-Encoding', <<"chunked">>, _) 
   ],
   {ok, stream:'++'(stream:new({Code, Head1}), Entity)};

packetize(_Type, Code)
 when is_atom(Code) orelse is_integer(Code) ->
   {ok, [{Code, [{'Content-Length', 0}], <<>>}]}.

add_head(Head, Value, Heads) ->
   case lists:keyfind(Head, 1, Heads) of
      false -> [{Head, Value}|Heads];
      _     -> Heads
   end. 


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%% apply resource function
f(#rest{mod = Mod, export = Export} = Rest, Fun) ->
   case lists:keyfind(Fun, 1, Export) of
      false -> default(Fun);
      _     -> Mod:Fun(req(Rest))
   end.

f(#rest{mod = Mod, export = Export} = Rest, Fun, X) ->
   case lists:keyfind(Fun, 1, Export) of
      false -> default(Fun);
      _     -> Mod:Fun(X, req(Rest))
   end.

f(#rest{mod = Mod, export = Export} = Rest, Fun, X, Y) ->
   case lists:keyfind(Fun, 1, Export) of
      false -> default(Fun);
      _     -> Mod:Fun(X, Y, req(Rest))
   end.

%%
%%
fail(#rest{mod = Mod, export = Export} = Rest, Reason) ->
   case lists:keyfind(fail, 1, Export) of
      false -> fail(Reason);
      _     -> Mod:fail(Reason, req(Rest))   
   end.

fail(Reason)
 when is_atom(Reason) ->
   {Reason, scalar:s(Reason)};

fail(Reason) ->
   {500, scalar:s(io_lib:format("~p~n", [Reason]))}.

%%
%% build a resource context 
req(#rest{uri = Url, head = Head, env = Env}) ->
   {Url, Head, Env}.

%%
%%
default(allowed_methods)  -> 
   ['GET', 'HEAD', 'OPTIONS'];

default(authorize)        -> 
   ok;

default(content_accepted) -> 
   [];

default(content_provided) -> 
   [{application, json}];

default(exists)           -> 
   true;

default(cors)             -> 
   [
      {'Access-Control-Allow-Methods', <<"GET, PUT, POST, DELETE, OPTIONS">>}
     ,{'Access-Control-Allow-Headers', <<"Content-Type">>}
     ,{'Access-Control-Max-Age', 600}
   ].


mimetype({Major, Minor}) ->
   <<(scalar:s(Major))/binary, $/, (scalar:s(Minor))/binary>>.


%%%------------------------------------------------------------------
%%%
%%% STREAM
%%%
%%%------------------------------------------------------------------   

% 'STREAM'({http, _Url, eof}, _Pipe, S) ->
%  % @todo: use http eof as a trigger for resource invocation
%    {next_state, 'STREAM', S};

% %%
% %% handles message from "external" processes, pipe binds either
% %%  "process" <-> "http" (if external process support pipe protocol)
% %%  "http"    <-> undefined
% 'STREAM'(Msg, Pipe, #fsm{resource=Mod, request=Req, content=Type}=S) ->
%  try
%     case Mod:stream(Type, Req, pipe:a(Pipe), Msg) of
%        eof  -> 
%           pipe:send(pipe_sink(Pipe), <<>>),
%           {next_state, 'ACCEPT', S};
%        undefined ->
%           {next_state, 'STREAM', S};
%        Http -> 
%           pipe:send(pipe_sink(Pipe), Http),
%           {next_state, 'STREAM', S}
%     end
%  catch _:Reason ->
%     lager:notice("restd request error ~p: ~p", [Reason, erlang:get_stacktrace()]),
%     pipe:send(pipe_sink(Pipe), handle_failure(Reason)),
%     {next_state, 'ACCEPT', S}
%  end.

