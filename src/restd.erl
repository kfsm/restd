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
-module(restd).

-export([start/0]).
-export([
	register/2
]).

%%
%% start application
start() -> 
   applib:boot(?MODULE, []).

%%
%% start service
start_link(Uid, Opts) ->
	restd_service_sup:start_link(Uid, Opts).


%%
%% register resource
-spec(register/2 :: (atom(), atom()) -> ok).

register(Uid, Mod) ->
	pns:register({restd, Uid}, Mod:resource(), Mod).

