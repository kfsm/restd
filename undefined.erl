

   % [$^||
   %    %%
   %    %% request routing
   %    resource(Http),
   %    resource(_, Http),
   %    is_method_implemented(_),
   %    is_method_allowed(_),
   %    is_access_authorized(_),
   %    is_content_supported(_),
   %    is_cors_allowed(_),
   %    %% @todo: terminate OPTION request here with 200
   %    %%
   %    %% content negotiation
   %    is_content_acceptable(_),
   %    is_language_acceptable(_),
   %    is_charset_acceptable(_),
   %    is_encoding_acceptable(_),
   %    %%
   %    %% resource negotiation
   %    is_resource_exists(_),
   %    is_etags_matched(_),
   %    is_modified(_),
   %    %%
   %    %% execute REST call
   %    execute(_, Entity)
   % ].


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
http_content_type(#http{head = Head} = Http) ->
   Lens  = htstream:http_content_type(),
   Value = head_content_type(lens:get(Lens, Head)),
   Http#http{
      head = lens:put(Lens, Value, Head)
   }.

head_content_type(undefined) ->
   undefined;
head_content_type(Value) ->
   mime_type(Value).

%%
%%
http_accept(#http{head = Head} = Http) ->
   Lens  = htstream:http_accept(),
   Value = head_accept(lens:get(Lens, Head)),
   Http#http{
      head = lens:put(Lens, Value, Head)
   }.

head_accept(undefined) ->
   [{'*', '*'}];
head_accept(Value) ->
   lists:filter(
      fun(X) -> X /= undefined end,
      lists:map(
         fun mime_type/1, 
         binary:split(Value, <<$,>>, [trim, global])
      )
   ). 

%%
%% TODO: support q-values
mime_type(Val) ->
   try
      [Mime | _QVal] = binary:split(Val, <<$;>>, []),
      case binary:split(Mime, <<$/>>, []) of
         [<<$*>>,<<$*>>] -> {'*',  '*'};
         [Major, <<$*>>] -> {scalar:a(Major), '*'};
         [<<$*>>, Minor] -> {'*', scalar:a(Minor)};
         [Major,  Minor] -> {scalar:a(Major), scalar:a(Minor)};
         [Major]         -> {scalar:a(Major), '*'}
      end
   catch _:_ ->
      undefined
   end.


% decode_header_value('Content-Type', Val) ->
%    {'Content-Type', decode_mime_type(Val)};
% decode_header_value('Accept', Val) ->
%    {'Accept', [decode_mime_type(X) || X <- binary:split(Val, <<$,>>, [trim, global])]};


%%
%%
% -spec resource(#http{}) -> {ok, #rest{}} | {error, {not_available, _}}.

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
   {ok, Rest#rest{mthd = Mthd, uri = Uri, inhead = orddict:from_list(Head), eghead = []}}.


%%
%%
% -spec is_method_implemented(#rest{}) -> {ok, #rest{}} | {error, {not_implemented, _}}.

is_method_implemented(#rest{export = Export, mthd = Mthd} = Rest) ->
   case lists:keyfind(Mthd, 1, Export) of
      false ->
         {error, {not_implemented, scalar:s(Mthd)}};
      _ ->
         {ok, Rest}
   end.


%%
%% 
% -spec is_method_allowed(#rest{}) -> {ok, #rest{}} | {error, {not_allowed, _}}.

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
% -spec is_access_authorized(#rest{}) -> {ok, #rest{}} | {error, {unauthorized, _}}.
 
is_access_authorized(#rest{mthd = Mthd, uri = Uri} = Rest) ->
   case f(Rest, authorize, Mthd) of
      ok ->
         {ok, Rest};
      {ok, _} ->
         {ok, Rest};
      {error, forbidden} ->
         {error, {forbidden, uri:s(Uri)}};
      {error, _}  ->
         {error, {unauthorized, uri:s(Uri)}}
   end.

%%
%%
-spec is_cors_allowed(#rest{}) -> {ok, #rest{}} | {error, {unsupported, _}}.

is_cors_allowed(#rest{inhead = InHead, eghead = EgHead} = Rest) ->
   case orddict:find(<<"Origin">>, InHead) of
      %% this is not a CORS request 
      error ->
         {ok, Rest};

      {_, Origin} ->
         Head = lists:foldl(
            fun({Key, Val}, Acc) -> orddict:store(Key, Val, Acc) end,
            orddict:store(<<"Access-Control-Allow-Origin">>, Origin, EgHead),
            f(Rest, cors)
         ),
         {ok, Rest#rest{eghead = Head}}
   end.

%%
%%
% -spec is_content_supported(#rest{}) -> {ok, #rest{}} | {error, {unsupported, _}}.

is_content_supported(#rest{inhead = InHead} = Rest) ->
   case opts:val(<<"Content-Type">>, undefined, InHead) of
      undefined -> 
         {ok, Rest};

      Type ->
         Accept = f(Rest, content_accepted),
         case restd:negotiate(Type, Accept) of
            [{_, _} = NType | _] ->
               {ok, Rest#rest{inhead = orddict:store(<<"Content-Type">>, NType, InHead)}};
            [] ->
               {error, {unsupported, mimetype(Type)}}
         end
   end.

%%
%%
% -spec is_content_acceptable(#rest{}) -> {ok, #rest{}} | {error, {not_acceptable, _}}.

is_content_acceptable(#rest{inhead = Head, eghead = EgHead} = Rest) ->
   Provide = f(Rest, content_provided),
   Accept  = opts:val(<<"Accept">>, [{'*', '*'}], Head),
   case
      lists:flatmap(restd:negotiate(_, Provide), Accept)
   of
      [{_, _} = Type | _] ->
         % handle content negotiation and preset content type accepted by client 
         {ok, Rest#rest{eghead = orddict:store('Content-Type', Type, EgHead)}};
      _ ->
         {error, {not_acceptable, [mimetype(X) || X <- Accept]}}
   end.


%%
%%
% -spec is_language_acceptable(#rest{}) -> {ok, #rest{}} | {error, {not_acceptable, _}}.

is_language_acceptable(#rest{} = Rest) ->
   {ok, Rest}.

%%
%%
% -spec is_charset_acceptable(#rest{}) -> {ok, #rest{}} | {error, {not_acceptable, _}}.

is_charset_acceptable(#rest{} = Rest) ->
   {ok, Rest}.

%%
%%
% -spec is_encoding_acceptable(#rest{}) -> {ok, #rest{}} | {error, {not_acceptable, _}}.

is_encoding_acceptable(#rest{} = Rest) ->
   {ok, Rest}.


%%
%%
-spec is_resource_exists(#rest{}) -> {ok, #rest{}} | {error, {not_found, _}}.

is_resource_exists(#rest{uri = Uri, eghead = EgHead} = Rest) ->
   Accept = orddict:fetch(<<"Content-Type">>, EgHead),
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

execute(#rest{mthd = Mthd, inhead = InHead, eghead = EgHead} = Rest, Entity) ->
   EgType = opts:val(<<"Content-Type">>, EgHead),
   InType = opts:val(<<"Content-Type">>, undefined, InHead),
   % REST call returns either Http Type or Xor 
   case f(Rest, Mthd, {EgType, InType}, Entity) of
      ?XOR_R(Http) ->
         packetize(EgHead, Http);
      ?XOR_L(Reason) ->
         {error, fail(Rest, Reason)};
      Http -> 
         packetize(EgHead, Http)
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
req(#rest{uri = Url, inhead = Head, env = Env}) ->
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
      {<<"Access-Control-Allow-Methods">>, <<"GET, PUT, POST, DELETE, OPTIONS">>}
     ,{<<"Access-Control-Allow-Headers">>, <<"Content-Type">>}
     ,{<<"Access-Control-Max-Age">>, 600}
   ].


headers(Head) ->
   lists:map(fun header/1, lists:flatten(Head)).

header({Head, {_, _} = Value}) ->
   {scalar:s(Head), mimetype(Value)};
header({Head, Value}) ->
   {scalar:s(Head), scalar:s(Value)}.

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


