%% @description
%%    acceptor process
-module(restd_acceptor).
-behaviour(pipe).

-export([
	start_link/2,
	init/1,
	free/2,
	ioctl/2,
	'LISTEN'/3,
	'ACCEPT'/3,
	'HANDLE'/3,
	'STREAM'/3
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

start_link(Uid, Uri) ->
	pipe:start_link(?MODULE, [Uid, Uri], []).

init([Uid, Uri]) ->
	{ok, _} = knet:bind(Uri),
	{ok, 'ACCEPT', 
		#fsm{
			uid = Uid,
			q   = deq:new()
		}
	}.

free(_Reason, _S) ->
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

'LISTEN'(_, _, S) ->
	{next_state, 'LISTEN', S}.

%%%------------------------------------------------------------------
%%%
%%% ACCEPT
%%%
%%%------------------------------------------------------------------   

%%
%%
'ACCEPT'({http, _, {Mthd, _Uri, _Head, _Env}=Req0}, Pipe, S) ->
	try
		{Mod, Type, Req} = validate_request(Req0, S#fsm.uid),
		case is_payload_method(Mthd) of
			%% stream payload in
			true  ->
   			{next_state, 'HANDLE',
      			S#fsm{
      				method   = Mthd,
         			resource = Mod,
         			request  = Req,
         			content  = Type,
         			q        = deq:new()
      			}
   			};

			false ->
				case handle_response(Mod:Mthd(Type, Req), Type) of
					%% no-payload, streaming
					{_Code, _Heads} = Http ->
		 				_ = pipe:a(Pipe, Http),
 						{next_state, 'STREAM', S#fsm{resource=Mod, request = Req, content=Type}};
 					%% there is a payload
 					{_Code, _Heads, _Msg} = Http ->
		 				_ = pipe:a(Pipe, Http),
 						{next_state, 'ACCEPT', S}
 				end
		end
	catch _:Reason ->
		lager:notice("restd request error ~p: ~p", [Reason, erlang:get_stacktrace()]),
		pipe:a(Pipe, handle_failure(Reason)),
		{next_state, 'ACCEPT', S}
	end;

'ACCEPT'(_, _, S) ->
	{next_state, 'ACCEPT', S}.

%%%------------------------------------------------------------------
%%%
%%% HANDLE
%%%
%%%------------------------------------------------------------------   

'HANDLE'({http, _Uri, Msg}, _Pipe, S)
 when is_binary(Msg) ->
   {next_state, 'HANDLE', 
   	S#fsm{
   		q = deq:enq(Msg, S#fsm.q)
   	}
   }; 

'HANDLE'({http, _Uri, eof}, Pipe, #fsm{method=Mthd, resource=Mod, request=Req, content=Type}=S) ->
   try
   	Msg  = erlang:iolist_to_binary(deq:list(S#fsm.q)),
		case handle_response(Mod:Mthd(Type, Req, Msg), Type) of
			%% no-payload, streaming
			{_Code, _Heads} = Http ->
 				_ = pipe:a(Pipe, Http),
				{next_state, 'STREAM', S#fsm{q = deq:new()}};
         %% there is a payload (lazy stream)
         {Code, Heads, {s, _, _}=Stream} ->
            _ = pipe:a(Pipe, {Code, Heads}),
            stream:foreach(
               fun(X) -> pipe:a(Pipe, X) end,
               Stream
            ),
            pipe:a(Pipe, <<>>),
            {next_state, 'ACCEPT', S#fsm{q = deq:new()}};
			%% there is a payload
			{_Code, _Heads, _Msg} = Http ->
 				_ = pipe:a(Pipe, Http),
				{next_state, 'ACCEPT', S#fsm{q = deq:new()}}
		end
   catch _:Reason ->
   	lager:notice("restd request error ~p: ~p", [Reason, erlang:get_stacktrace()]),
   	pipe:a(Pipe, handle_failure(Reason)),
		{next_state, 'ACCEPT', S}
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
%% validate resource request and return request environment
validate_request({Mthd, Uri, Heads, Env}, Service) ->
	{Mod, GEnv} = is_resource_available(Mthd, {Uri, Heads, Env}, Service),
	Req = {Uri, Heads, GEnv ++ Env}, 
	_   = is_method_allowed(Mthd, Req, Mod),
	_   = is_request_authorized(Mthd, Req, Mod),
	Type= is_content_supported(Mthd, Req, Mod),
	%% @todo accept- langauge, charset, encoding 
   _   = is_resource_exists(Mthd, Req, Mod),  
   {Mod, Type, Req}.


%%
%% check if resource available (resource handler is registered)
is_resource_available(_Mthd, {Uri, _Head, _Env}, Service) ->
	Resources = ets:lookup(restd, Service),
	case list_available_resources(Uri, Resources) of
		%% not available
		[]    -> 
			throw({error, not_available});
		%% available
		Match ->
			{_, Env, Mod, GEnv} = hd(Match),
			case code:is_loaded(Mod) of
				false ->
					throw({error, not_available});
				_     ->
					{Mod, build_request_env(GEnv, Env)}
			end
	end.

list_available_resources(Uri, Resources) ->
	Match = [{length(uri:segments(TUri)), uri:match(Uri, TUri), Mod, Env} || {_, TUri, Mod, Env} <- Resources],
	lists:sort(
		fun({A, _, _, _}, {B, _, _, _}) -> A >= B end,
		lists:filter(
			fun({_, X, _, _}) -> X =/= false end,
			Match
		)
	).

build_request_env(undefined, true) -> [];
build_request_env(undefined,  Env) -> Env;
build_request_env(GEnv,      true) -> GEnv;
build_request_env(GEnv,       Env) -> Env ++ GEnv.

%%
%% check if method is allowed
is_method_allowed(Mthd, {_Uri, _Head, _Env}, Mod) ->
	case erlang:function_exported(Mod, allowed_methods, 0) of
		%
		true  ->
			assert_allowed_method(Mthd, Mod:allowed_methods());

		% check is not implemented, by default only read methods are allowed
		false ->
			assert_allowed_method(Mthd, ['GET', 'HEAD', 'OPTIONS', 'TRACE'])
	end.

assert_allowed_method(_Mthd, ['*']) ->
   true;

assert_allowed_method(Mthd, Allowed) ->
   case lists:member(Mthd, Allowed) of
      false -> 
      	throw({error, not_allowed});
      true  -> 
      	ok
   end.

%%
%% check if request is authorized
is_request_authorized(_Mthd, {Uri, _Head, _Env}=Req, Mod) ->
	case erlang:function_exported(Mod, is_authorized, 1) of
		%
		true  ->
			case Mod:is_authorized(Uri, Req) of
				ok ->
					ok;
				_  ->
					throw({error, unauthorized})
			end;

		% check is not implemented, request is authorized by default
		false ->
			ok
	end.

%%
%% check if resource exists
is_content_supported(Mthd, {_Uri, Heads, _Env}, Mod) ->
	case is_payload_method(Mthd) of
		true  ->
			assert_content_type([opts:val('Content-Type', Heads)], Mod:content_accepted());
			
		false ->
		   assert_content_type(opts:val('Accept', [{'*', '*'}], Heads), Mod:content_provided())
	end.

assert_content_type([], _B) ->
	throw({error, not_acceptable});

assert_content_type([H | T], B) ->
	case assert_content_type(H, B) of
		false -> assert_content_type(T, B);
		Type  -> Type
	end;
assert_content_type(A, B) ->
	Req = tuple_to_list(A),
	case lists:filter(fun(X) -> is_equiv(Req, tuple_to_list(X)) end, B) of
		[]   -> false;
		List -> hd(List)
	end.

%%
%% check if request is authorized
%% @todo: there is diff semantic for put / post vs get
is_resource_exists(_Mthd, {_Uri, _Head, _Env}=Req, Mod) ->
	case erlang:function_exported(Mod, exists, 1) of
		%
		true  ->
			case Mod:exists(Req) of
				true ->
					ok;
				_  ->
					throw({error, not_found})
			end;

		% check is not implemented, request is authorized by default
		false ->
			ok
	end.





%% check if two uri segments equivalent
is_equiv(['*'], _) ->
	true;
is_equiv(_, ['*']) ->
	true;
is_equiv([H|A], [_|B])
 when H =:= '_' orelse H =:= '*' ->
	is_equiv(A, B);
is_equiv([_|A], [H|B])
 when H =:= '_' orelse H =:= '*' ->
	is_equiv(A, B);
is_equiv([A|AA], [B|BB]) ->
	case eq(A, B) of
		true  -> is_equiv(AA, BB);
		false -> false
	end;
is_equiv([], []) ->
 	true;
is_equiv(_,   _) ->
 	false.

%% check if two path elements are equal
eq(A, B)
 when is_atom(A), is_binary(B) ->
 	atom_to_binary(A, utf8) =:= B;
eq(A, B)
 when is_binary(A), is_atom(B) ->
 	A =:= atom_to_binary(B, utf8);
eq(A, B) ->
	A =:= B.


%%
%% check is http method carries any payload
is_payload_method('GET')     -> false;
is_payload_method('HEAD')    -> false;
is_payload_method('OPTIONS') -> false;
is_payload_method('TRACE')   -> false;
is_payload_method('DELETE')  -> false;
is_payload_method(_)         -> true.



