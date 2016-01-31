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
%%   basho bench driver
-module(restd_benchmark).

-export([
   new/1, 
   run/4
]).

%%
%%
new(1) ->
   knet:start(),
   lager:set_loglevel(lager_console_backend, basho_bench_config:get(log_level, info)),
   Url = uri:new(basho_bench_config:get(url, "http://localhost:8080")),
   {ok, #{url => Url}};

new(_) ->
   Url = uri:new(basho_bench_config:get(url, "http://localhost:8080")),
   {ok, #{url => Url}}.

%%
%%
run(_, _KeyGen, _ValGen, #{url := Url, sock := Sock} = State) ->
   pipe:send(Sock, {'GET', Url, [
      {'Connection', 'keep-alive'}, 
      {'Accept',     [{'*', '*'}]}, 
      {'Host',       uri:authority(Url)}
   ]}),
   200 = http_wait(Sock, undefined),
   {ok, State};

run(Op, KeyGen, ValGen, #{url := Url} = State) ->
   Sock = knet:socket(Url, []),
   {ioctl, b, _} = knet:recv(Sock),
   run(Op, KeyGen, ValGen, State#{sock => Sock}).

http_wait(Sock, X) ->
	case knet:recv(Sock) of
		{http, _, {Code, _Text, _Head, _Env}} ->
			http_wait(Sock, Code);
		{http, _, eof} ->
			X;
		_ ->
			http_wait(Sock, X)
	end.


