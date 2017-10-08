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
%% @todo
%%   * abstract routing table
%%   * clean-up acceptor
-module(restd).
% -compile({parse_transform, partial}).
-compile({parse_transform, category}).

-include("restd.hrl").

-export([behaviour_info/1]).
-export([start/0]).
-export([start_link/2, spec/2]).

-export([
   url/2,
   path/2,
   method/2,
   headers/1,
   header/2,
   provided_content/2,
   provided_encoding/1,
   provided_encoding/2,
   accepted_content/2,
   authorize/2,
   cors/1, 
   cors/2,

   to_json/1,
   to_json/2,
   to_json/3,
   as_json/1,
   to_text/1,
   to_text/2,
   to_text/3,
   as_text/1
]).


-export([
   host/0,
   routes/2,
   return/5,
   negotiate/2
]).


%%%----------------------------------------------------------------------------   
%%%
%%% restd behavior interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% 
behaviour_info(callbacks) ->
   [
      %%
      %% return list of allowed methods 
      %%
      %% -spec(allowed_methods/0 :: (request()) -> [method()]).
      %% {allowed_methods, 1}

      %%
      %% return list of content accepted by resource
      %%
      %% -spec(content_accepted/0 :: (request()) -> [mime()]).
      %% {content_accepted, 1}

      %%
      %% return list of provided content by resource
      %%
      %% -spec(content_provided/0 :: (request()) -> [mime()]).
      %% {content_provided, 1}

      %%
      %% check if resource exists
      %%
      %% -spec(exists/2 :: (mime(), request()) -> true | false).
      %% {exists, 2}
      
      %%
      %% authorize request
      %%
      %% -spec(authorize/2 :: (method(), request()) -> ok | any()).
      %% {authorize, 2}

      %%
      %% define cors policy
      %%
      %% -spec(cors/1 :: (request()) -> [header()])

      %%
      %% stream
      %%
      %% -spec(stream/? :: (...) -> ?).
      %% {stream,   ?}

      %%
      %% http method handler
      %%
      %% -spec(xxx/2 :: (mime(), request()) -> response()).
      %% -spec(xxx/3 :: (mime(), request(), binary()) -> response()).
      %% {xxx,      2}
      %% {xxx,      3}
   ];
behaviour_info(_) ->
   undefined.


%%%----------------------------------------------------------------------------   
%%%
%%% application interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% start application
start() -> 
   applib:boot(?MODULE, []).

%%
%%
-spec host() -> uri:uri().

host() ->
   uri:new(opts:val(host, restd)).

%%
%% start rest service
-spec start_link(atom(), [_]) -> {ok, pid()} | {error, any()}.

start_link(Routes, Opts) ->
	restd_service_sup:start_link(Routes, Opts).

%%
%% 
-spec spec(_, _) -> _.

spec(Routes, Opts) ->
   {
      scalar:s(opts:val(port, Opts)),
      {restd, start_link, [Routes, Opts]},
      permanent, 5000, supervisor, dynamic
   }.


%%%----------------------------------------------------------------------------   
%%%
%%% pattern matches
%%%
%%%----------------------------------------------------------------------------   

%%
%% matches path segements
path(Path, #request{uri = Uri}) ->
   path(
      uri:segments(uri:path(Path, Uri)), 
      uri:segments(Uri), 
      Uri
   ).

path([<<"_">>|A], [_|B], Uri) ->
   path(A, B, Uri);

path([<<"*">>|_], _, Uri) ->
   {ok, uri:segments(Uri)};

path([Head|A], [Head|B], Uri) ->
   path(A, B, Uri);

path([], [], Uri) ->
   {ok, uri:segments(Uri)};

path(_, _, Uri) ->
   {error, {not_available, uri:s(Uri)}}.
   
%%
%% matches path segments and return url
url(Path, #request{uri = Uri} = Request) ->
   [either ||
      path(Path, Request),
      fmap(Uri)
   ].

%%
%% matches request method
method(Mthd, #request{mthd = Mthd}) ->
   {ok, Mthd};
method(_, #request{mthd = Mthd}) ->
   {error, {not_allowed, Mthd}}.

%%
%%
headers(#request{head = Headers}) ->
   {ok, Headers}.

%%
%%
header(Head, #request{head = Headers}) ->
   case lens:get(lens:pair(scalar:s(Head), undefined), Headers) of
      undefined ->
         {error, {badarg, Head}};
      Value ->
         {ok, Value}
   end.

%%
%% match request of supported content type
provided_content({_, _} = Provide, #request{} = Request) ->
   [either ||
      Head <- header(<<"Accept">>, Request),
      fmap(parse_content_type(Head)),
      fmap(negotiate(Provide, _)),
      return_provided_content(_, Head)
   ].

return_provided_content([{_, _} = Type | _], _) ->
   {ok, {<<"Content-Type">>, mimetype(Type)}};
return_provided_content(_, Accept) ->
   {error, {not_acceptable, Accept}}.

%%
%%
provided_encoding(#request{} = Request) ->
   [either ||
      Head <- header(<<"Accept-Encoding">>, Request),
      fmap(parse_encoding(Head)),
      return_provided_encoding(_, Head)
   ].

provided_encoding(Encoding, #request{} = Request) ->
   [either ||
      Head <- header(<<"Accept-Encoding">>, Request),
      fmap(parse_encoding(Head)),
      fmap(lists:filter(fun(X) -> X =:= scalar:s(Encoding) end, _)),
      return_provided_encoding(_, Head)
   ].

return_provided_encoding([Encoding | _], Head) ->
   {ok, {<<"Content-Encoding">>, Encoding}};
return_provided_encoding(_, Encoding) ->
   {error, {not_acceptable, Encoding}}.

%%
%% match request of acceptable content type
accepted_content({_, _} = Accept, #request{} = Request) ->
   [either ||
      Head <- header(<<"Content-Type">>, Request),
      fmap(parse_content_type(Head)),
      fmap(negotiate(Accept, _)),
      return_accepted_content(_, Head)
   ].

return_accepted_content([{_, _} = Type | _], _) ->
   {ok, Type};
return_accepted_content(_, ContentType) ->
   {error, {unsupported, ContentType}}.


   % Content = lens:get(htstream:http_content_type(), Head),
   % case
   %    restd:negotiate(Content, [Accept])
   % of
   %    [{_, _} = Type | _] ->
   %       {ok, Type};
   %    _ ->
   %       {error, {unsupported, mimetype(Content)}}
   % end.

%%
%%
authorize(Authorize, #request{mthd = Mthd, uri = Uri, head = Head}) ->
   case lens:get(htstream:http_authorization(), Head) of
      undefined ->
         {error, {unauthorized, uri:s(Uri)}};
      Token ->
         Authorize(Token, {Mthd, Uri, Head})
   end.

%%
%%
cors(Request) ->
   cors([
      {<<"Access-Control-Allow-Methods">>, <<"GET, PUT, POST, DELETE, OPTIONS">>}
     ,{<<"Access-Control-Allow-Headers">>, <<"Content-Type">>}
     ,{<<"Access-Control-Max-Age">>, 600}
   ], Request).

cors(CORS, Request) -> ok.

%%
%%
accepted_langauge(_, _) -> ok.

%%
%%
accepted_charset(_, _) -> ok.

%%
%%
accepted_encoding(_, _) -> ok.

%%
%%
is_resource_exists(_, _) -> ok.

%%
%%
is_etags_matched(_, _) -> ok.

%%
%%
is_modified(_, _) -> ok.


%%
%%
-spec to_json(_) -> {_, _, _}.

to_json(Json) ->
   to_json([], Json).

to_json(Head, Json) ->
   to_json(200, Head, Json).

to_json(Code, Head, Json) ->
   {ok,
      {Code, [{<<"Content-Type">>, <<"application/json">>} | Head], jsx:encode(Json)}
   }.

%%
%%
-spec as_json(_) -> _.

as_json(#request{entity = Entity}) ->
   {ok, 
      jsx:decode(Entity, [return_maps])
   }.

%%
%%
-spec to_text(_) -> {_, _, _}.

to_text(Text) ->
   to_text([], Text).

to_text(Head, Text) ->
   to_json(200, Head, Text).

to_text(Code, Head, Text) ->
   {ok,
      {Code, [{<<"Content-Type">>, <<"text/plain">>} | Head], scalar:s(Text)}
   }.

%%
%%
-spec as_text(_) -> _.

as_text(#request{entity = Entity}) ->
   {ok, Entity}.


%%
%% compile routing table
-spec routes(atom(), [_]) -> {module, atom()}.

routes(Id, Routes) ->
   hornlog:c(Id, 
      [route(X) || X <- Routes]).

%% compile route to hornlog rule
route({Path, Resource, Env}) ->
   %%
   %% the load of resource is required for any other deployment configuration
   %% except OTP releases. Not Available error is returned if code is not loaded. 
   %% The restd do the best effort to load the module but ignores any possible errors.
   %% Purge is required if same module is bound to multiple end-points
   %%
   %% Note: purge impacts on code test coverage
   %% 
   %% code:purge(Resource),
   %% code:load_file(Resource),
   Pattern = uri:segments( uri:new(Path) ),
   Export  = Resource:module_info(exports),
   hornlog:rule(
      hornlog:head(fun restd:return/5, [Pattern, Resource, Export, Env]), 
      lists:map(fun pattern/1 , Pattern)
   );
route({Route, Resource}) ->
   route({Route, Resource, []}).

%% build uri pattern matcher
pattern(<<$:, _/binary>>) ->
   '_';
pattern(<<$_>>) ->
   '_';
pattern(X) ->
   X.

%% return matched result
return(Pattern, Resource, Export, Env, Uri) ->
   Lenv = lists:map(
      fun({<<$:, Key/binary>>, Val}) ->
         {Key, Val}
      end,
      lists:filter(
         fun({<<$:, _/binary>>, _}) -> true; (_) -> false end,
         lists:zip(Pattern, uri:segments(Uri))
      )
   ),
   #{resource => Resource, export => Export, env => Lenv ++ Env}.

   
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


