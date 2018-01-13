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

-compile({parse_transform, category}).
-compile({parse_transform, partial}).
-include("restd.hrl").
-include_lib("datum/include/datum.hrl").

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
-record(state, {
   endpoints = undefined :: atom()         %% routing table
  ,request   = undefined :: #request{}     %%
  ,entity    = undefined :: datum:q()      %% incoming entitity queue
}).


%%%------------------------------------------------------------------
%%%
%%% Factory
%%%
%%%------------------------------------------------------------------   

start_link(Endpoints, Uri, Opts) ->
   pipe:start_link(?MODULE, [Endpoints, Uri, Opts], []).

init([Endpoints, Uri, Opts]) ->
   knet:bind(Uri, Opts),
   {ok, 'ACCEPT', #state{endpoints = Endpoints}}.

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
'ACCEPT'({http, _, {Mthd, Uri, Head}}, _Pipe, #state{} = State) ->
   {next_state, 'HTTP', 
      State#state{
         request = #request{t = os:timestamp(), mthd = Mthd, uri = Uri, head = Head},
         entity  = deq:new()
      }
   };

'ACCEPT'({ws, _, {Mthd, Uri, Head}}, _Pipe, #state{} = State) ->
   {next_state, 'WEBSOCK', 
      State#state{
         request = #request{t = os:timestamp(), mthd = Mthd, uri = Uri, head = Head},
         entity  = deq:new()
      }
   };

'ACCEPT'({sidedown, _, _}, _Pipe, State) ->
   {stop, normal, State};

'ACCEPT'({_, _, passive}, Pipe, State) ->
   pipe:a(Pipe, {active, 1024}),
   {next_state, 'ACCEPT', State}.

%%%------------------------------------------------------------------
%%%
%%% HTTP
%%%
%%%------------------------------------------------------------------   

'HTTP'({_, _, passive}, Pipe, State) ->
   pipe:a(Pipe, {active, 1024}),
   {next_state, 'HTTP', State};

'HTTP'({http, _Uri, eof}, Pipe, #state{endpoints = Endpoints} = State) ->
   case execute_rest(State) of
      ?EitherR(#stream{} = Http) ->
         stream:foreach(pipe:a(Pipe, _), Http),
         {next_state, 'ACCEPT', #state{endpoints = Endpoints}};

      ?EitherR(Http) ->
         lists:foreach(pipe:a(Pipe, _), Http),
         {next_state, 'ACCEPT', #state{endpoints = Endpoints}};

      ?EitherL(Reason) ->
         {stop, Reason, State}
   end;

'HTTP'({http, _Uri, Pckt}, _Pipe, #state{entity = Entity} = State)
 when is_binary(Pckt) ->
   %% @todo: limit on entity, return 413
   {next_state, 'HTTP', 
      State#state{entity = deq:enq(Pckt, Entity)}
   }.

%%%------------------------------------------------------------------
%%%
%%% WEBSOCK
%%%
%%%------------------------------------------------------------------   

'WEBSOCK'({ws, _, eof}, _Pipe, State) ->
   {stop, normal, State};

'WEBSOCK'({ws, _, {error, Reason}}, _Pipe, State) ->
   {stop, Reason, State};

'WEBSOCK'({ws, _, Packet}, Pipe, #state{} = State) ->
   case execute_stream(State#state{entity = Packet}) of
      ?EitherR(#stream{} = Http) ->
         stream:foreach(pipe:a(Pipe, _), Http),
         {next_state, 'WEBSOCK', State};

      ?EitherR(Http) ->
         lists:foreach(pipe:a(Pipe, _), Http),
         {next_state, 'WEBSOCK', State};

      ?EitherL(Reason) ->
         {stop, Reason, State}
   end.


% 'WEBSOCK'({ws, _, Msg}, Pipe, #rest{mod = Mod, inhead = InHead, eghead = EgHead} = State) ->
%    EgType = opts:val(<<"Content-Type">>, EgHead),
%    InType = opts:val(<<"Content-Type">>, undefined, InHead),
%    case Mod:recv({EgType, InType}, Msg, req(State)) of
%       ok ->
%          {next_state, 'WEBSOCK', State};
%       {ok, Pckt} ->
%          _ = pipe:a(Pipe, Pckt),
%          {next_state, 'WEBSOCK', State}
%    end;

% 'WEBSOCK'(Msg, Pipe, #rest{mod = Mod, inhead = InHead, eghead = EgHead} = State) ->
%    EgType = opts:val(<<"Content-Type">>, EgHead),
%    InType = opts:val(<<"Content-Type">>, undefined, InHead),
%    case Mod:send({EgType, InType}, Msg, req(State)) of
%       ok ->
%          {next_state, 'WEBSOCK', State};
%       {ok, Pckt} ->
%          pipe:send(pipe_sink(Pipe), Pckt),
%          {next_state, 'WEBSOCK', State};
%       eof  -> 
%          {stop, normal,  State}
%    end.

% pipe_sink(Pipe) ->
%    case pipe:b(Pipe) of
%       undefined -> pipe:a(Pipe);
%       Pid       -> Pid
%    end.

%%%------------------------------------------------------------------
%%%
%%% REST
%%%
%%%------------------------------------------------------------------   

%%
%%
execute_rest(#state{endpoints = Endpoints, request = Request, entity = Entity}) ->
   HttpEntity = deq:list(Entity),
   ?DEBUG("~p~n~p~n", [Request, HttpEntity]),
   case 
      endpoints(Endpoints, [], Request#request{entity = HttpEntity})
   of
      ?EitherR(Http) ->
         packetize(Http);
      ?EitherL(Reasons) ->
         packetize(fail(Request, Reasons))
   end.

%%
%%
execute_stream(#state{endpoints = Endpoints, request = Request, entity = Entity}) ->
   case 
      endpoints(Endpoints, [], Request#request{entity = Entity})
   of
      ?EitherR(Http) ->
         streaming(Http);
      ?EitherL(Reasons) ->
         % web-socket is already established, 
         % we cannot use HTTP status code to indicate routing error
         % Routing is failed send only error and terminate connection
         Reason = fail_sort_by(Reasons),
         {error, Reason}
   end.

%%
%%
endpoints([Head | Tail], Reasons, #request{} = Request) ->
   case Head(Request) of
      ?EitherR(_) = Result ->
         Result;
      ?EitherL(Reason) ->
         endpoints(Tail, [Reason | Reasons], Request)
   end;

endpoints([], Reasons, #request{}) ->
   {error, Reasons}.


%%
%% https://tools.ietf.org/html/rfc7807
fail(#request{uri = Uri}, Reasons) ->
   Reason = fail_sort_by(Reasons),
   restd_codec:encode_error(Uri, Reason).

fail_sort_by(Reasons) ->
   hd(lists:sort(
      fun(A, B) -> 
         fail_priority(A) =< fail_priority(B)
      end,
      Reasons
   )).

fail_priority({not_available, _}) -> 1000;
fail_priority({not_allowed, _}) -> 990;
fail_priority({not_acceptable, _}) -> 820;
fail_priority({unsupported, _}) -> 800;
fail_priority({unauthorized, _}) -> 520;
fail_priority({forbidden, _}) -> 500;
fail_priority({badarg, _}) -> 10;
fail_priority(_) -> 1.


%%
%%
packetize({Code, Head, #stream{} = Stream}) ->
   {HtCode, HtText} = restd_codec:status_code(Code),
   HtHead = [{<<"Transfer-Encoding">>, <<"chunked">>} | Head],
   {ok,
      stream:'++'(
         stream:'++'(
            stream:new({HtCode, HtText, HtHead}), 
            stream:map(fun(X) -> {packet, X} end, Stream)
         ),
         stream:new(eof)
      )
   };

packetize({Code, Head, Entity}) ->
   {HtCode, HtText} = restd_codec:status_code(Code),
   HtEntity = encode(Head, Entity),
   HtHead   = [{<<"Content-Length">>, iolist_size(HtEntity)} | Head],
   {ok, 
      lists:flatten([
         {HtCode, HtText, HtHead},
         [{packet, X} || X <- HtEntity],
         eof
      ])
   }.

encode(Head, Entity) ->
   case lens:get(lens:pair(<<"Content-Encoding">>, undefined), Head) of
      <<"gzip">> ->
         [zlib:gzip([Entity])];
      <<"deflate">> ->
         [zlib:compress([Entity])];
      _ ->
         lists:flatten([Entity])
   end.

%%
%%
streaming(#stream{} = Stream) ->
   {ok, stream:map(fun(X) -> {packet, X} end, Stream)};

streaming(Entity) ->
   {ok,
      [{packet, X} || X <- lists:flatten([Entity])]
   }.
