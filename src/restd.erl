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
-module(restd).
-compile({parse_transform, category}).

-include("restd.hrl").

-export([start/0]).
-export([
   start_link/2, 
   spec/2
]).
%% directives
-export([
   url/2,
   path/2,
   q/1,
   q/2,
   method/2,
   headers/1,
   header/2,
   host/1,
   provided_content/2,
   provided_encoding/1,
   provided_encoding/2,
   accepted_content/2,
   % accepted_langauge/2,
   % accepted_charset/2,
   % accepted_encoding/2,
   authorize/2,
   cors/1, 
   cors/2,
   cors/3,
   accesslog/2,
   to_json/2, to_json/3, to_json/4,
   as_json/1,
   to_text/2, to_text/3, to_text/4,
   as_text/1,
   to_form/2, to_form/3, to_form/4,
   as_form/1
]).


-export_type([endpoint/0]).

%%
%% data types
%%
-type endpoint()  :: fun((request()) -> {ok, response()} | {error, _}).

-type request()   :: #request{}.
-type response()  :: {code(), headers(), entity()}.

-type code()      :: integer().
-type header()    :: {binary(), _}.
-type headers()   :: [header()].
-type entity()    :: binary() | datum:stream().
-type method()    :: atom().
-type mimetype()  :: {_, _}.

-type opts()      :: [_].


%%
%% start application
start() -> 
   applib:boot(?MODULE, []).

%%
%% start rest service
-spec start_link([endpoint()], opts()) -> {ok, pid()} | {error, _}.

start_link(Routes, Opts) ->
   restd_service_sup:start_link(Routes, Opts).

%%
%% return a supervisor specification of services
-spec spec(_, _) -> _.

spec(Routes, Opts) ->
   {
      scalar:s(opts:val(port, Opts)),
      {restd, start_link, [Routes, Opts]},
      permanent, 5000, supervisor, dynamic
   }.


%%%----------------------------------------------------------------------------   
%%%
%%% pattern match interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% matches path segments and returns variables
-spec path(uri:path(), request()) -> datum:either( uri:segments() ).

path(Path, #request{uri = Uri}) ->
   path(
      uri:segments(uri:path(Path, Uri)), 
      uri:segments(Uri), 
      [{<<"path">>, uri:path(Uri)} | path_to_env(Uri)]
   ).

path([<<$_>>|A], [_|B], Env) ->
   path(A, B, Env);

path([<<$:, Var/binary>>|A], [Val|B], Env) ->
   path(A, B, [{Var, Val} | Env]);

path([<<"*">>|_], _, Env) ->
   {ok, Env};

path([Head|A], [Head|B], Env) ->
   path(A, B, Env);

path([], [], Env) ->
   {ok, Env};

path(_, _, Env) ->
   {error, {not_available, lens:get(lens:pair(<<"path">>), Env)}}.
   
path_to_env(Uri) ->
   case uri:q(Uri) of
      undefined -> [];
      Query     -> Query
   end.

%%
%% matches path segments and return url
-spec url(uri:path(), request()) -> datum:either( uri:uri() ).

url(Path, #request{uri = Uri} = Request) ->
   [either ||
      path(Path, Request),
      cats:unit(Uri)
   ].

%%
%% matches whole url's query or individual key
-spec q(request()) -> datum:either( uri:params() ).
-spec q(binary() | list(), request()) -> datum:either( binary() ).  

q(#request{uri = Uri}) ->
   case uri:q(Uri) of
      undefined ->
         {ok, []};
      Value ->
         {ok, Value}
   end.

q(Key, #request{} = Request) ->
   [either ||
      q(Request),
      cats:unit(
         case lens:get(lens:pair(Key, undefined), _) of 
            undefined ->
               {error, {badarg, Key}};
            Value ->
               {ok, Value}
         end
      )
   ].


%%
%% matches request method
-spec method(method(), request()) -> datum:either( method() ).

method(Mthd, #request{mthd = Mthd}) ->
   {ok, Mthd};
method(_, #request{mthd = Mthd}) ->
   {error, {not_allowed, Mthd}}.


%%
%% matches request headers
-spec headers(request()) -> datum:either( headers() ).

headers(#request{head = Headers}) ->
   {ok, Headers}.


%%
%% matches request header
-spec header(binary() | list(), request()) -> datum:either( header() ).

header(Head, #request{head = Headers}) ->
   case lens:get(lens:pair(scalar:s(Head), undefined), Headers) of
      undefined ->
         {error, {badarg, Head}};
      Value ->
         {ok, Value}
   end.

%%
%% matches request host header(s)
-spec host(request()) -> datum:either( uri:uri() ).

host(#request{head = Headers} = Request) ->
   [either ||
      Host <- header('Host', Request),
      host_schema(Headers),
      cats:unit(uri:authority(Host, uri:new(_)))
   ].

host_schema(Headers) ->
   case 
      {
         lens:get(lens:pair(<<"X-Forwarded-Proto">>, undefined), Headers),
         lens:get(lens:pair(<<"X-Forwarded-Port">>, undefined), Headers),
         lens:get(lens:pair(<<"X-Knet-Peer">>, undefined), Headers)
      } 
   of
      {<<"https">>, _, _} -> {ok, https};
      {_, <<"443">>, _} -> {ok, https};
      {_, _, <<"ssl://", _/binary>>} -> {ok, https};
      _ -> {ok, http}
   end.

%%
%% match request of supported content type
-spec provided_content(mimetype(), request()) -> datum:either( header() ).

provided_content({_, _} = Provide, #request{} = Request) ->
   [either ||
      Head <- header(<<"Accept">>, Request),
      cats:unit(parse_content_type(Head)),
      cats:unit(negotiate(Provide, _)),
      return_provided_content(_, Head)
   ].

return_provided_content([{_, _} = Type | _], _) ->
   {ok, {<<"Content-Type">>, mimetype(Type)}};
return_provided_content(_, Accept) ->
   {error, {not_acceptable, Accept}}.

%%
%% match request of supported content encoding
-spec provided_encoding(request()) -> datum:either( header() ).
-spec provided_encoding(_, request()) -> datum:either( header() ).

provided_encoding(#request{} = Request) ->
   [either ||
      Head <- header(<<"Accept-Encoding">>, Request),
      cats:unit(parse_encoding(Head)),
      return_provided_encoding(_, Head)
   ].

provided_encoding(Encoding, #request{} = Request) ->
   [either ||
      Head <- header(<<"Accept-Encoding">>, Request),
      cats:unit(parse_encoding(Head)),
      cats:unit(lists:filter(fun(X) -> X =:= scalar:s(Encoding) end, _)),
      return_provided_encoding(_, Head)
   ].

return_provided_encoding([Encoding | _], _) ->
   {ok, {<<"Content-Encoding">>, Encoding}};
return_provided_encoding(_, Encoding) ->
   {error, {not_acceptable, Encoding}}.

%%
%% match request of acceptable content type
-spec accepted_content(mimetype(), request()) -> datum:either( mimetype() ).

accepted_content({_, _} = Accept, #request{} = Request) ->
   [either ||
      Head <- header(<<"Content-Type">>, Request),
      cats:unit(parse_content_type(Head)),
      cats:unit(negotiate(Accept, _)),
      return_accepted_content(_, Head)
   ].

return_accepted_content([{_, _} = Type | _], _) ->
   {ok, Type};
return_accepted_content(_, ContentType) ->
   {error, {unsupported, ContentType}}.

%%
%%
%%accepted_langauge(_, _) -> ok.

%%
%%
%%accepted_charset(_, _) -> ok.

%%
%%
%%accepted_encoding(_, _) -> ok.


%%
%% match request against Authorization header and executes a token validation
-spec authorize(fun((_) -> datum:either(_)), request()) -> datum:either(_).

authorize(Authorize, #request{mthd = Mthd, uri = Uri, head = Head}) ->
   case lens:get(lens:pair(<<"Authorization">>, undefined), Head) of
      undefined ->
         {error, {unauthorized, uri:s(Uri)}};
      Token ->
         case Authorize(Token, {Mthd, Uri, Head}) of
            {error, forbidden} -> 
               {error, {forbidden, uri:s(Uri)}};
            {error, _} -> 
               {error, {unauthorized, uri:s(Uri)}};
            {ok, _} = Value ->
               Value
         end
   end.

%%
%% match request and returns CORS headers
-spec cors(request()) -> datum:either( headers() ).
-spec cors(headers(), request()) -> datum:either( headers() ).
-spec cors(_, headers(), request()) -> datum:either( headers() ).

cors(Request) ->
   cors(default_cors(), Request).

cors(Policy, #request{head = Head}) ->
   case lens:get(lens:pair(<<"Origin">>, undefined), Head) of
      undefined ->
         {ok, []};
      Origin ->
         {ok, [{<<"Access-Control-Allow-Origin">>, Origin} | Policy]}
   end.

cors(Origin, Policy, #request{head = Head}) ->
   case lens:get(lens:pair(<<"Origin">>, undefined), Head) of
      {_, Origin} ->
         {ok, [{<<"Access-Control-Allow-Origin">>, Origin} | Policy]};
      Other ->
         {error, {not_allowed, scalar:s(Other)}}
   end.

default_cors() ->
   [
      {<<"Access-Control-Allow-Methods">>, <<"GET, PUT, POST, DELETE, OPTIONS">>}
     ,{<<"Access-Control-Allow-Headers">>, <<"Content-Type">>}
     ,{<<"Access-Control-Max-Age">>,       600}
   ].

%%
%%
%% output access evidence using common access log format:
%%
%%  peer user "request addr" response "user-agent" byte pack time
%%
%%   * peer - ip address of peer making request
%%   * user - identifier of user
%%   * request - protocol specific request string
%%   * addr - local address
%%   * response - protocol specific request code
%%   * user-agent - user agent string if applicable
%%   * byte - number of transmitted bytes
%%   * time - protocol latency is micro seconds 
%%
%% @example
%%   127.0.0.1  -  "GET http://127.0.0.1:8888/" 200 "curl/7.37.1" 252 4 37123209
-spec accesslog(response(), request()) -> datum:either(response()).

accesslog({Code, _, Payload} = Response, #request{t = T, mthd = Mthd, uri = Uri, head = Head}) ->
   Peer = lens:get(lens:pair(<<"X-Knet-Peer">>, $-), Head),
   UA   = lens:get(lens:pair(<<"User-Agent">>, $-), Head),
   Log  = erlang:iolist_to_binary([Peer, $ , $-, $ , $", scalar:s(Mthd), $ , uri:s(Uri), $", $ , scalar:s(Code), $ , $", UA, $", $ , scalar:s(iolist_size(Payload)), $ , scalar:s(tempus:u(tempus:diff(T)))]),
   lager:notice(Log),
   {ok, Response};

accesslog({ok, Response}, Request) ->
   accesslog(Response, Request).


%%
%%
%%is_resource_exists(_, _) -> ok.

%%
%%
%%is_etags_matched(_, _) -> ok.

%%
%%
%%is_modified(_, _) -> ok.


%%
%% encode result as json
-spec to_json(_, request()) -> response().
-spec to_json(_, _, request()) -> response().
-spec to_json(_, _, _, request()) -> response().

to_json(Json, Request) ->
   to_json([], Json, Request).

to_json(Head, {ok, Json}, Request) ->
   to_json(200, Head, Json, Request);

to_json(_, {error, Reason}, #request{uri = Uri}) ->
   {ok, restd_codec:encode_error(Uri, Reason)}.

to_json(Code, Head, Json, _Request) ->
   {ok,
      {Code, [{<<"Content-Type">>, <<"application/json">>} | Head], jsx:encode(Json)}
   }. 


%%
%% decode payload as json
-spec as_json(request()) -> _.

as_json(#request{entity = Entity}) ->
   try
      {ok, 
         jsx:decode(erlang:iolist_to_binary(Entity), [return_maps])
      }
   catch _:_ ->
      {error, {badarg, payload}}
   end.

%%
%% encode result as plain text
-spec to_text(_, request()) -> response().
-spec to_text(_, _, request()) -> response().
-spec to_text(_, _, _, request()) -> response().

to_text(Text, Request) ->
   to_text([], Text, Request).

to_text(Head, {ok, Text}, Request) ->
   to_text(200, Head, Text, Request);

to_text(_, {error, Reason}, #request{uri = Uri}) ->
   {ok, restd_codec:encode_error(Uri, Reason)}.


to_text(Code, Head, Text, _Request) ->
   {ok,
      {Code, [{<<"Content-Type">>, <<"text/plain">>} | Head], scalar:s(Text)}
   }.

%%
%% decode request payload as plain text
-spec as_text(_) -> _.

as_text(#request{entity = Entity}) ->
   {ok, Entity}.


%%
%% encode result as json
-spec to_form(_, request()) -> response().
-spec to_form(_, _, request()) -> response().
-spec to_form(_, _, _, request()) -> response().

to_form(Form, Request) ->
   to_form([], Form, Request).

to_form(Head, {ok, Form}, Request) ->
   to_form(200, Head, Form, Request);

to_form(_, {error, Reason}, #request{uri = Uri}) ->
   {ok, restd_codec:encode_error(Uri, Reason)}.

to_form(Code, Head, Form, _Request) ->
   [either ||
      restd_codec:encode_form(Form),
      cats:unit({Code, [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>} | Head], _})
   ].

%%
%% decode request payload as application/x-www-form-urlencoded
-spec as_form(_) -> _.

as_form(#request{entity = Entity}) ->
   restd_codec:decode_form(Entity).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% applies content negotiation algorithms
%% (see https://en.wikipedia.org/wiki/Content_negotiation)
negotiate({_, _} = Type, Types) ->
   lists:flatmap(fun(X) -> content_negotiate(X, [Type]) end, Types).

content_negotiate({Major, Minor}, Types) ->
   MajorLn = fun(X) -> content_major(Major, X) end,
   MinorLn = fun(X) -> content_minor(Minor, X) end,
   lists:filter(MinorLn, lists:filter(MajorLn, Types)).

content_major(A, {B, _}) ->
   scalar:s(B) =:= A orelse A =:= '*' orelse B =:= '*'.

content_minor(A, {_, B}) ->
   scalar:s(B) =:= A orelse A =:= '*' orelse B =:= '*'.

%%
%%
parse_content_type(Value) ->
   lists:map(
      fun(Content) ->
         [Type | _QVal] = binary:split(Content, <<$;>>, []),
         case binary:split(Type, <<$/>>, []) of
            [<<$*>>,<<$*>>] -> {'*',  '*'};
            [Major, <<$*>>] -> {Major, '*'};
            [<<$*>>, Minor] -> {'*', Minor};
            [Major,  Minor] -> {Major, Minor};
            [Major]         -> {Major, '*'}
         end
      end,
      binary:split(Value, <<$,>>, [trim, global])
   ).

mimetype({Major, Minor}) ->
   <<(scalar:s(Major))/binary, $/, (scalar:s(Minor))/binary>>.


%%
%%
parse_encoding(Value) ->
   [X || X <- binary:split(Value, [<<$,>>, <<$ >>], [trim, global]), X /= <<>>].


