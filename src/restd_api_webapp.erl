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
%%    webapp container
-module(restd_api_webapp).

-export([
   allowed_methods/1,
   content_provided/1,
   cors/1,
   exists/2,
   'GET'/3
]).

allowed_methods(_Req) ->
   ['GET'].

%%
%%
content_provided(_Req) ->
   [{'*', '*'}].

%%
%%
cors(_Req) ->
   [
      {'Access-Control-Allow-Origin', <<"*">>}
     ,{'Access-Control-Allow-Methods', <<"GET">>}
     ,{'Access-Control-Max-Age', 86400}
   ].

%%
%%
exists(_, {Url, _Heads, Env}) ->
   filelib:is_file(filename(Url, Env)).

%%
%%
'GET'(_Type, _Msg, {Url, _Heads, Env}) ->
   Filename   = filename(Url, Env),
   {ok, File} = file:read_file(Filename),
   {ok, 
      [
         {'Content-Type', mime(filename:extension(Filename))}
      ], 
      File
   }.

%%
%%
filename(Url, Env) ->
   case opts:val(<<"file">>, undefined, Env) of
      undefined ->
         filename:join([htdoc(Env), path(Env)| file(Url)]);
      File ->
         filename:join([htdoc(Env), path(Env), hd(segments([scalar:s(File)]))])
   end.

%%
%% 
htdoc(Env) ->
   case opts:val(htdoc, Env) of
      X when is_atom(X) -> 
         case code:priv_dir(X) of
            {error,bad_name} -> 
               filename:join([priv, htdoc]);
            Root -> 
               filename:join([Root, htdoc])
         end;
      X when is_list(X) ->
         X 
   end.

%%
%%
path(Env) ->
   case opts:val(path, undefined, Env) of
      undefined ->
         <<>>;
      Path ->
         Path
   end.

%%
%%
segments(Segments) ->
   [scalar:s(X) || X <- Segments, X =/= <<".">>, X =/= <<"..">>, X =/= <<$~>>].

%%
%%
file(Url) ->
   case uri:segments(Url) of
      undefined -> <<"index.html">>;
      []        -> <<"index.html">>;
      Segments  -> segments(Segments)
   end.


%%
%%
mime(<<>>)         -> {text, html};
mime(<<".html">>)  -> {text, html};
mime(<<".txt">>)   -> {text, plain};
mime(<<".css">>)   -> {text, css};
mime(<<".js">>)    -> {text, javascript};
mime(<<".xml">>)   -> {text, xml};

mime(<<".json">>)  -> {application, json};
mime(<<".png">>)   -> {image, png};
mime(<<".jpg">>)   -> {image, jpeg};
mime(<<".jpeg">>)  -> {image, jpeg};
mime(<<".svg">>)   -> {image, 'svg+xml'};

mime(<<".pdf">>)   -> {application, pdf};

mime(_)            -> {application, 'octet-stream'}.
