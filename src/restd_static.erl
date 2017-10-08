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
%%    support for static file
-module(restd_static).
-compile({parse_transform, category}).

-export([
   reader/2
]).


%%
%% returns static file
%%
reader(Pattern, Root) ->
   [pattern ||
      Path /= restd:path(Pattern),
         _ /= restd:method('GET'),
      File <- filename(Root, Path),
      readfile(File),
      sendfile(File, _)
   ].

%%
%%
filename(Root, Path)
 when is_atom(Root) ->
   filename(filename:join([code:priv_dir(Root), <<"htdoc">>]), Path);

filename(Root, Path) ->
   {ok, 
      scalar:s(filename:join([scalar:s(Root), path_to_filename(Path)]))
   }.

path_to_filename([]) ->
   [ "index.html" ];
path_to_filename(Segments) ->
   segments(Segments).

segments(Segments) ->
   [scalar:s(X) || X <- Segments, X =/= <<".">>, X =/= <<"..">>, X =/= <<$~>>].

%%
%%
readfile(File) ->
   file:read_file(File).


%%
%%
sendfile(File, Content) ->
   Type = content_type(filename:extension(File)),
   {ok,
      {200, [{<<"Content-Type">>, Type}], Content}
   }.

%%
%%
content_type(<<>>)         -> <<"text/html">>;
content_type(<<".html">>)  -> <<"text/html">>;
content_type(<<".txt">>)   -> <<"text/plain">>;
content_type(<<".css">>)   -> <<"text/css">>;
content_type(<<".js">>)    -> <<"text/javascript">>;
content_type(<<".xml">>)   -> <<"text/xml">>;

content_type(<<".json">>)  -> <<"application/json">>;
content_type(<<".png">>)   -> <<"image/png">>;
content_type(<<".jpg">>)   -> <<"image/jpeg">>;
content_type(<<".jpeg">>)  -> <<"image/jpeg">>;
content_type(<<".svg">>)   -> <<"image/svg+xml">>;

content_type(<<".pdf">>)   -> <<"application/pdf">>;

content_type(_)            -> <<"application/octet-stream">>.
