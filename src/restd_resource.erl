-module(restd_resource).
-include("restd.hrl").

-export([
   new/3,
   do/2,
   restapi/2,
   recv/2,
   send/2,
   %%
   is_available/1,
   is_method_implemented/1,
   is_method_allowed/1,
   is_access_authorized/1,
   is_content_supported/1,
   is_content_acceptable/1,
   is_resource_exists/1,
   is_cors_allowed/1
]).


%%
%%
new(Service, Request, Env) ->
   #resource{service = Service, request = Request, env = Env}.

%%
%% do-notation: fold request over set of functions
%%   fun(#restapi{}) -> #restapi{}.
do(Request0, [H | T]) ->
   case H(Request0) of
      {error, _} = Error ->
         Error;
      {ok, Request1} ->
         do(Request1, T)
   end;

do(Request0, []) ->
   {ok, Request0}.

%%
%% apply rest resource function
restapi(#resource{request = {Mthd, _, _}, env = Env} = Resource, Inbound) ->
   {_, Mime} = lists:keyfind('Content-Type', 1, Env),
   f(Resource, Mthd, Mime, erlang:iolist_to_binary(Inbound)).

recv(#resource{module = Mod, env = Env} = Resource, Msg) ->
   {_, Mime} = lists:keyfind('Content-Type', 1, Env),
   Mod:recv(Mime, Msg, req(Resource)).

send(#resource{module = Mod, env = Env} = Resource, Msg) ->
   {_, Mime} = lists:keyfind('Content-Type', 1, Env),
   Mod:send(Mime, Msg, req(Resource)).

%%
%% 
is_available(#resource{service = Service, request = {_, Url, _}, env = LEnv} = Resource) ->
   try
      #{resource := Module, export := Export, env := GEnv} = hornlog:q(Service, uri:segments(Url), Url),
      {ok, Resource#resource{module = Module, export = Export, env = LEnv ++ GEnv}}
   catch _:_ ->
      % {error, {not_available, Url}}
      {error, not_available}
   end.

%%
%%
is_method_implemented(#resource{export = Export, request = {Mthd, _, _}} = Resource) ->
   case lists:keyfind(Mthd, 1, Export) of
      false ->
         % {error, {not_implemented, Mthd}};
         {error, not_implemented};
      _ ->
         {ok, Resource}
   end.

%%
%% 
is_method_allowed(#resource{request = {Mthd, _, _}} = Resource) ->
   List = f(Resource, allowed_methods),
   case lists:member(Mthd, List) of
      false -> 
         % {error, {not_allowed, Mthd}};
         {error, not_allowed};
      true  -> 
         {ok, Resource}
   end.

%%
%% 
is_access_authorized(#resource{request = {Mthd, Url, _}} = Resource) ->
   case f(Resource, authorize, Mthd) of
      ok ->
         {ok, Resource};
      _  ->
         % {error, {unauthorized, Url}}
         {error, unauthorized}
   end.

%%
%% 
is_content_supported(#resource{request = {_, _, Head}} = Resource) ->
   List = f(Resource, content_accepted),
   case content_type(opts:val('Content-Type', {'*', '*'}, Head), List) of
      [Value|_] -> 
         {ok, set_ingress_mime(Value, Resource)};
      _ ->
         {ok, set_ingress_mime({'*', '*'}, Resource)}
   end.

%%
%% 
is_content_acceptable(#resource{request = {_, _, Head}} = Resource) ->
   List = f(Resource, content_provided),
   case
      lists:flatten(
         lists:map(
            fun(X) -> content_type(X, List) end,
            opts:val('Accept', [{'*', '*'}], Head)
         )
      )
   of
      [Value | _] ->
         {ok, set_egress_mime(Value, Resource)};
      _ ->
         {error, not_acceptable}
   end.

%%
%%
is_resource_exists(#resource{head = Head} = Req) -> 
   {_, Accept} = lists:keyfind('Content-Type', 1, Head),
   case f(Req, exists, Accept) of
      true ->
         {ok, Req};
      _    ->
         {error, not_found}
   end.

%%
%%
is_cors_allowed(#resource{request = {_, _, HeadIn}, head = HeadEg} = Resource) ->
   case lists:keyfind(<<"Origin">>, 1, HeadIn) of
      %% this is not a CORS request 
      false ->
         {ok, Resource};

      {_, Origin} ->
         CORS = f(Resource, cors),
         {ok, Resource#resource{head = [{'Access-Control-Allow-Origin', Origin}|CORS] ++ HeadEg}}
   end.

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------   

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
%%
set_ingress_mime(Type, #resource{env = Env} = Resource) ->
   Resource#resource{env = [{'Content-Type', Type}|Env]}.

%%
%%
set_egress_mime(Type, #resource{head = Head} = Resource) ->
   Resource#resource{head = [{'Content-Type', Type}|Head]}.

%%
%% apply resource function
f(#resource{module = Mod, export = Export} = Resource, Fun) ->
   case lists:keyfind(Fun, 1, Export) of
      false -> default(Fun);
      _     -> Mod:Fun(req(Resource))
   end.

f(#resource{module = Mod, export = Export} = Resource, Fun, X) ->
   case lists:keyfind(Fun, 1, Export) of
      false -> default(Fun);
      _     -> Mod:Fun(X, req(Resource))
   end.

f(#resource{module = Mod, export = Export} = Resource, Fun, X, Y) ->
   case lists:keyfind(Fun, 1, Export) of
      false -> default(Fun);
      _     -> Mod:Fun(X, Y, req(Resource))
   end.

%%
%% build a resource context 
req(#resource{request = {_, Url, Head}, env = Env}) ->
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
