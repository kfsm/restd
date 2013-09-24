%%
%%   Copyright (c) 2012 - 2013, Dmitry Kolesnikov
%%   Copyright (c) 2012 - 2013, Mario Cardona
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
%%   basho bench driver
-module(restd_benchmark).

-export([
   new/1, 
   run/4
]).

%%
%%
new(_Id) ->
	_ = knet:start(),

 	lager:set_loglevel(lager_console_backend, basho_bench_config:get(log_level, info)),
   Url = uri:new(basho_bench_config:get(url, "http://localhost:8080")),

   %%{ok, Sock} = knet:socket(uri:get(schema, Url), []),
   %%{ok, {Sock, Url}}.
   {ok, Url}.

%%
%%
%run(_, _KeyGen, _ValGen, {Sock, Url}=S) ->
run(_, _KeyGen, _ValGen, Url) ->
   {ok, Sock} = knet:socket(uri:get(schema, Url), []),

	pipe:send(Sock, {'GET', Url, [
		{'Connection', 'close'}, 
		{'Accept',     [{'*', '*'}]}, 
		{'Host',       uri:get(authority, Url)}
	]}),
	200 = http_wait(undefined),
	knet:close(Sock),
   {ok, Url}.


http_wait(X) ->
	case pipe:recv(5000) of
		{http, _, {Code, _, _}} ->
			http_wait(Code);
		{http, _, eof} ->
			X;
		_ ->
			http_wait(X)
	end.


