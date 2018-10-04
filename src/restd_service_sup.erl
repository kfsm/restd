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
-module(restd_service_sup).
-behaviour(supervisor).

-export([
   start_link/3,
   init/1
]).

%%
-define(CHILD(Type, I),            {I,  {I, start_link,   []}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, permanent, 5000, Type, dynamic}).


%%
%%
start_link(Routes, Filters, #{port := Port} = Opts) ->
   supervisor:start_link(?MODULE, [Port, Routes, Filters, Opts]).
   
init([Port, Routes, Filters, Opts]) -> 
   listen(Port, Routes, Filters, Opts),
   {ok,
      {
         {one_for_one, 4, 1800},
         []
      }
   }.

%%
%%
listen(Uri, Routes, Filters, Opts) ->
   knet:listen(Uri, 
      maps:merge(
         #{
            acceptor => {restd_acceptor, [Routes, Filters]}
         ,  pool     => lens:get(lens:at(pool, 10), Opts)
         ,  backlog  => lens:get(lens:at(backlog, 25), Opts)
         ,  shutdown => true
         ,  pipe     => false
         },
         lens:get(lens:at(sock, #{}), Opts)
      )
   ).
