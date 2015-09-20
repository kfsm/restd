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

-export([behaviour_info/1]).
-export([start/0]).
-export([
   start_link/2,
	register/3,
	register/4
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
%% start service
start_link(Service, Opts) ->
	restd_service_sup:start_link(Service, Opts).


%%
%% register resource
-spec(register/3 :: (atom(), list() | binary(), atom()) -> ok).
-spec(register/4 :: (atom(), list() | binary(), atom(), any()) -> ok).

register(Service, Uri, Mod) ->
	register(Service, Uri, Mod, undefined).

register(Service, Uri, Mod, Env) ->
	true = ets:insert(restd, {Service, uri:template(Uri), Mod, Env}),
	ok.
