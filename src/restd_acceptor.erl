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

%% http connection is established
% -record(http, {
%    route  = undefined :: atom()          %% routing table
%   ,mthd   = undefined :: _
%   ,uri    = undefined :: uri:uri()
%   ,head   = undefined :: _
%   ,env    = undefined :: _
%   ,q      = undefined :: datum:q()       %% current queue
% }).

%% http request is mapped to resource 
% -record(rest, {
%    mod    = undefined :: atom()          %% resource implementation
%   ,export = undefined :: _               %% resource exports
%   ,mthd   = undefined :: _
%   ,uri    = undefined :: uri:uri()
%   ,inhead = undefined :: _               %% ingress http headers
%   ,eghead = undefined :: _               %% egress http headers
%   ,env    = undefined :: _
% }).

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
         request = #request{mthd = Mthd, uri = Uri, head = Head},
         entity  = deq:new()
      }
   };

'ACCEPT'({ws, _, {Mthd, Uri, Head}}, _Pipe, #state{} = State) ->
   {next_state, 'WEBSOCK', 
      State#state{
         request = #request{mthd = Mthd, uri = Uri, head = Head},
         entity  = deq:new()
      }
   };

'ACCEPT'({sidedown, _, _}, _Pipe, State) ->
   {stop, normal, State}.

%%%------------------------------------------------------------------
%%%
%%% HTTP
%%%
%%%------------------------------------------------------------------   

'HTTP'({http, _Uri, eof}, Pipe, #state{endpoints = Endpoints} = State) ->
   case execute_rest(State) of
      ?EITHER_R({s, _, _} = Http) ->
         streams:foreach(pipe:a(Pipe, _), Http),
         {next_state, 'ACCEPT', #state{endpoints = Endpoints}};

      ?EITHER_R(Http) ->
         lists:foreach(pipe:a(Pipe, _), Http),
         {next_state, 'ACCEPT', #state{endpoints = Endpoints}};

      ?EITHER_L(Reason) ->
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

'WEBSOCK'(_, _, State) ->
   {stop, not_implemented, State}.

% 'WEBSOCK'({ws, _, _} = Msg, Pipe, #http{} = State) ->
%    case stream(State) of
%       ?XOR_R(#rest{} = Rest) ->
%          'WEBSOCK'(Msg, Pipe, Rest);

%       ?XOR_L(_Error) ->
%          % web-socket is already established, 
%          % we cannot use HTTP status code to indicate routing error
%          % Routing is failed send only error and terminate connection
%          {stop, normal, State} 
%    end;

% 'WEBSOCK'({ws, _, {terminated, _}}, _Pipe, State) ->
%    {stop, normal, State};

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
   case 
      endpoints(Endpoints, [], Request#request{entity = deq:list(Entity)})
   of
      ?EITHER_R(Http) ->
         packetize(Http);
      ?EITHER_L(Reasons) ->
         packetize(fail(Reasons))
   end.

%%
%%
endpoints([Head | Tail], Reasons, #request{} = Request) ->
   case Head(Request) of
      ?EITHER_R(_) = Result ->
         Result;
      ?EITHER_L(Reason) ->
         endpoints(Tail, [Reason | Reasons], Request)
   end;

endpoints([], Reasons, #request{}) ->
   {error, Reasons}.


%%
%% https://tools.ietf.org/html/rfc7807
fail(Reasons) ->
   {Code, Json} = failwith(hd(Reasons)),
   {Code, [{<<"Content-Type">>, <<"application/json">>}], jsx:encode(Json)}.

failwith({Reason, Details}) ->
   {500, 
      #{
         type    => <<"http://example.org">>,
         title   => scalar:s(Reason),
         details => Details
      }
   }.




% %%
% %% 
% rest(#http{route = Endpoints} = Http, Entity) ->
%    endpoints(Endpoints, Http, Entity).


%    case rest(State#http{q = undefined}, deq:list(Q)) of
%       % ?EITHER_R({s, _, _} = Http) ->
%       %    streams:foreach(pipe:a(Pipe, _), Http);

%       ?EITHER_R(Http) ->
%          io:format("==> ~p~n", [Http]),
%          lists:foreach(pipe:a(Pipe, _), Http);

%       ?EITHER_L({_, _, _} = Error) ->
%          pipe:a(Pipe, Error);

%       ?EITHER_L({Code, _} = Error) ->
%          {ok, Http} = packetize([{<<"Content-Type">>, <<"application/json">>}], {Code, jsx:encode([Error])}),
%          lists:foreach(pipe:a(Pipe, _), Http)         
%    end,
%    {next_state, 'ACCEPT', #state{endpoints = Endpoints}};      

%    case f(Rest, Mthd, {EgType, InType}, Entity) of
%       ?XOR_R(Http) ->
%          packetize(EgHead, Http);
%       ?XOR_L(Reason) ->
%          {error, fail(Rest, Reason)};
%       Http -> 
%          packetize(EgHead, Http)
%    end.


% %%
% %%
% endpoints(Endpoints, #http{mthd = Mthd, head = Head, uri = Uri}, Entity) ->
%    endpoints(Endpoints, #request{mthd = Mthd, head = Head, uri = Uri, entity = Entity}).

% endpoints([Head | Tail], Request) ->
%    case Head(Request) of
%       ?EITHER_R(Http) ->
%          packetize(Http);
%       ?EITHER_L(Reason) ->
%          io:format("==> ~p~n", [Reason]),
%          endpoints(Tail, Request)
%    end;

% endpoints([], #request{uri = Uri}) ->
%    {error, {not_available, uri:s(Uri)}}.



%%
%%
packetize({Code, Head, {s, _, _} = Stream}) ->
   {HtCode, HtText} = status_code(Code),
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
   {HtCode, HtText} = status_code(Code),
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


%% encode rest api status code response
status_code(100) -> {100, <<"Continue">>};
status_code(101) -> {101, <<"Switching Protocols">>};
status_code(200) -> {200, <<"OK">>};
status_code(201) -> {201, <<"Created">>};
status_code(202) -> {202, <<"Accepted">>};
status_code(203) -> {203, <<"Non-Authoritative Information">>};
status_code(204) -> {204, <<"No Content">>};
status_code(205) -> {205, <<"Reset Content">>};
status_code(206) -> {206, <<"Partial Content">>};
status_code(300) -> {300, <<"Multiple Choices">>};
status_code(301) -> {301, <<"Moved Permanently">>};
status_code(302) -> {302, <<"Found">>};
status_code(303) -> {303, <<"See Other">>};
status_code(304) -> {304, <<"Not Modified">>};
status_code(307) -> {307, <<"Temporary Redirect">>};
status_code(400) -> {400, <<"Bad Request">>};
status_code(401) -> {401, <<"Unauthorized">>};
status_code(402) -> {402, <<"Payment Required">>};
status_code(403) -> {403, <<"Forbidden">>};
status_code(404) -> {404, <<"Not Found">>};
status_code(405) -> {405, <<"Method Not Allowed">>};
status_code(406) -> {406, <<"Not Acceptable">>};
status_code(407) -> {407, <<"Proxy Authentication Required">>};
status_code(408) -> {408, <<"Request Timeout">>};
status_code(409) -> {409, <<"Conflict">>};
status_code(410) -> {410, <<"Gone">>};
status_code(411) -> {411, <<"Length Required">>};
status_code(412) -> {412, <<"Precondition Failed">>};
status_code(413) -> {413, <<"Request Entity Too Large">>};
status_code(414) -> {414, <<"Request-URI Too Long">>};
status_code(415) -> {415, <<"Unsupported Media Type">>};
status_code(416) -> {416, <<"Requested Range Not Satisfiable">>};
status_code(417) -> {417, <<"Expectation Failed">>};
status_code(422) -> {422, <<"Unprocessable Entity">>};
status_code(500) -> {500, <<"Internal Server Error">>};
status_code(501) -> {501, <<"Not Implemented">>};
status_code(502) -> {502, <<"Bad Gateway">>};
status_code(503) -> {503, <<"Service Unavailable">>};
status_code(504) -> {504, <<"Gateway Timeout">>};
status_code(505) -> {505, <<"HTTP Version Not Supported">>};

%status_code(100) -> <<"100 Continue">>;
%status_code(101) -> <<"101 Switching Protocols">>;
status_code(ok)       -> status_code(200);
status_code(created)  -> status_code(201);
status_code(accepted) -> status_code(202);
%status(203) -> <<"203 Non-Authoritative Information">>;
status_code(no_content) -> status_code(204);
%status(205) -> <<"205 Reset Content">>;
%status(206) -> <<"206 Partial Content">>;
%status(300) -> <<"300 Multiple Choices">>;
%status(301) -> <<"301 Moved Permanently">>;
%status(found) -> <<"302 Found">>;
%status(303) -> <<"303 See Other">>;
%status(304) -> <<"304 Not Modified">>;
%status(307) -> <<"307 Temporary Redirect">>;
status_code(badarg) -> status_code(400);
status_code(unauthorized) -> status_code(401);
%status(402) -> <<"402 Payment Required">>;
status_code(forbidden) -> status_code(403);
status_code(not_found) -> status_code(404);
status_code(enoent)    -> status_code(404);
status_code(not_allowed)    -> status_code(405);
status_code(not_acceptable) -> status_code(406);
%status(407) -> <<"407 Proxy Authentication Required">>;
%status(408) -> <<"408 Request Timeout">>;
status_code(conflict) -> status_code(409);
status_code(duplicate)-> status_code(409);
%status(410) -> <<"410 Gone">>;
%status(411) -> <<"411 Length Required">>;
%status(412) -> <<"412 Precondition Failed">>;
%status(413) -> <<"413 Request Entity Too Large">>;
%status(414) -> <<"414 Request-URI Too Long">>;
status_code(bad_mime_type) -> status_code(415);
%status(416) -> <<"416 Requested Range Not Satisfiable">>;
%status(417) -> <<"417 Expectation Failed">>;
%status(422) -> <<"422 Unprocessable Entity">>;
status_code(not_implemented) -> status_code(501);
%status(502) -> <<"502 Bad Gateway">>;
status_code(not_available) -> status_code(503);
%status(504) -> <<"504 Gateway Timeout">>;
%status(505) -> <<"505 HTTP Version Not Supported">>.
status_code(_) -> status_code(500).






% packetize(EgHead, {Code, Head0, Entity}) 
%  when is_binary(Entity) ->
%    Head1 = orddict:merge(
%       fun(_, _, X) -> X end,
%       orddict:store(<<"Content-Length">>, size(Entity), EgHead),
%       orddict:from_list(Head0)
%    ),
%    {ok, [{Code, Head1, Entity}]};

% packetize(EgHead, {Code, Head0, Entity})
%  when is_list(Entity) ->
%    case lists:keyfind(<<"Content-Length">>, 1, Head0) of
%       false ->
%          Head1 = orddict:merge(
%             fun(_, _, X) -> X end,
%             orddict:store(<<"Content-Length">>, iolist_size(Entity), EgHead),
%             orddict:from_list(Head0)
%          ),
%          Head2 = orddict:store(<<"Transfer-Encoding">>, <<"chunked">>, Head1),
%          {ok, [{Code, Head2}|Entity] ++ [<<>>]};
%       _ ->
%          Head1 = orddict:merge(
%             fun(_, _, X) -> X end,
%             EgHead,
%             orddict:from_list(Head0)
%          ),
%          {ok, [{Code, Head1, Entity}]}
%    end;

% packetize(EgHead, {Code, Head0, {s, _, _} = Entity}) ->
%    Head1 = orddict:merge(
%       fun(_, _, X) -> X end,
%       orddict:store(<<"Transfer-Encoding">>,  <<"chunked">>, EgHead),
%       orddict:from_list(Head0)
%    ),
%    {ok, stream:'++'(stream:new({Code, Head1}), Entity)};

% packetize(EgHead, {Code, Entity}) ->
%    packetize(EgHead, {Code, [], Entity});

% packetize(EgHead, Code)
%  when is_atom(Code) orelse is_integer(Code) ->
%    packetize(EgHead, {Code, [], <<>>});

% packetize(EgHead, Entity) ->
%    packetize(EgHead, {ok, [], Entity}).

