%% 
%% @doc
%%
-module(echo_restapi).
-compile({parse_transform, category}).

-export([
   ipaddr_json/1,
   ipaddr_text/1,
   user_agent/1,
   headers/1,
   get/1,
   post/1,
   put/1,
   patch/1,
   delete/1,
   utf8/1,
   deflate/1,
   gzip/1,
   compress/1,
   status_code/1,
   response_header/1,
   redirect_n/1,
   cookies/1
]).

%%
%% Returns Origin IP.
%%
ipaddr_json(Req) ->
   [either ||
      restd:path("/ip", Req),
      restd:method('GET', Req),
      restd:provided_content({application, json}, Req),
      restd:header(<<"X-Knet-Peer">>, Req),
      restd:to_json(#{ip => _})
   ].


ipaddr_text(Req) ->
   [either ||
      restd:path("/ip", Req),
      restd:method('GET', Req),
      restd:provided_content({text, plain}, Req),
      restd:header(<<"X-Knet-Peer">>, Req),
      restd:to_text(_)
   ].


%%
%% Returns user-agent.
%%
user_agent(Req) ->
   [either ||
      restd:path("/user-agent", Req),
      restd:header(<<"User-Agent">>, Req),
      restd:to_json(#{'user-agent' => _})
   ].

%%
%% Returns header dict.
%%
headers(Req) ->
   [either ||
      restd:path("/headers", Req),
      restd:method('GET', Req),
      restd:provided_content({application, json}, Req),
      restd:headers(Req),
      restd:to_json(#{headers => _})
   ].

%%
%% Returns GET data.
%%
get(Req) ->
   [either ||
      Url  <- restd:url("/get", Req),
      Mthd <- restd:method('GET', Req),
      Head <- restd:headers(Req),
      Peer <- restd:header(<<"X-Knet-Peer">>, Req),
      restd:to_json(#{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd})
   ].

%%
%% Returns POST data.
%% 
post(Req) ->
   [either ||
      Url  <- restd:url("/post", Req),
      Mthd <- restd:method('POST', Req),
      Head <- restd:headers(Req),
      Peer <- restd:header(<<"X-Knet-Peer">>, Req),
      restd:accepted_content({'*', '*'}, Req),
      Data <- restd:as_text(Req),
      restd:to_json(#{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd, data => Data})
   ].

%%
%% Returns PUT data.
%% 
put(Req) ->
   [either ||
      Url  <- restd:url("/put", Req),
      Mthd <- restd:method('PUT', Req),
      Head <- restd:headers(Req),
      Peer <- restd:header(<<"X-Knet-Peer">>, Req),
      restd:accepted_content({'*', '*'}, Req),
      Data <- restd:as_text(Req),
      restd:to_json(#{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd, data => Data})
   ].

%%
%% Returns PATCH data.
%% 
patch(Req) ->
   [either ||
      Url  <- restd:url("/patch", Req),
      Mthd <- restd:method('PATCH', Req),
      Head <- restd:headers(Req),
      Peer <- restd:header(<<"X-Knet-Peer">>, Req),
      restd:accepted_content({'*', '*'}, Req),
      Data <- restd:as_text(Req),
      restd:to_json(#{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd, data => Data})
   ].


%%
%% Returns DELETE data.
%% 
delete(Req) ->
   [either ||
      Url  <- restd:url("/delete", Req),
      Mthd <- restd:method('DELETE', Req),
      Head <- restd:headers(Req),
      Peer <- restd:header(<<"X-Knet-Peer">>, Req),
      restd:accepted_content({'*', '*'}, Req),
      Data <- restd:as_text(Req),
      restd:to_json(#{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd, data => Data})
   ].

%%
%% Returns page containing UTF-8 data.
%%
utf8(Req) ->
   [either ||
      restd:path("/encoding/utf8", Req),
      fmap({
         200, 
         [{<<"Content-Type">>, <<"text/html; charset=utf-8">>}],
         [
            <<"¡™£¢∞§¶•ªº–≠"/utf8>>, 
            <<"œ∑´®†¥øasdfghjkl;'π“‘«"/utf8>>, 
            <<"Ω≈ç√∫˜µ≤≥÷"/utf8>>
         ]
      })
   ].

%%
%% Return deflate-encoded data.
%%
deflate(Req) ->
   [either ||
      Url  <- restd:url("/encoding/deflate", Req),
      Mthd <- restd:method('GET', Req),
      Head <- restd:headers(Req),
      Peer <- restd:header(<<"X-Knet-Peer">>, Req),
      restd:provided_encoding(deflate, Req), 
      restd:to_json([{<<"Content-Encoding">>, <<"deflate">>}],
         #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd})
   ].

%%
%% Return gzip-encoded data.
%%
gzip(Req) ->
   [either ||
      Url  <- restd:url("/encoding/gzip", Req),
      Mthd <- restd:method('GET', Req),
      Head <- restd:headers(Req),
      Peer <- restd:header(<<"X-Knet-Peer">>, Req),
      restd:provided_encoding(gzip, Req), 
      restd:to_json([{<<"Content-Encoding">>, <<"gzip">>}],
         #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd})
   ].

%%
%% Negotiate compression protocol and return encoded data
%%
compress(Req) ->
   [either ||
      Url  <- restd:url("/encoding/compress", Req),
      Mthd <- restd:method('GET', Req),
      Head <- restd:headers(Req),
      Peer <- restd:header(<<"X-Knet-Peer">>, Req),
      Encoding <- restd:provided_encoding(Req), 
      restd:to_json([Encoding],
         #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd})
   ].

%%
%% Returns given HTTP Status code.
%%
status_code(Req) ->
   [either ||
      Path <- restd:path("/status/_", Req),
      restd:method('GET', Req),
      do_status_code(Path)
   ].

do_status_code([_, Code]) ->
   restd:to_text(scalar:i(Code), [], ["-=[ ", Code, " ]=-"]).

%%
%% Return given response headers
%%
response_header(Req) ->
   [either ||
      restd:path("/response-header", Req),
      restd:method('GET', Req),
      Query <- restd:q(Req),
      restd:to_json(Query, Query)
   ].

%%
%% 302 Redirects n times.
%%
redirect_n(Req) ->
   [either ||
      Path <- restd:path("/redirect/_", Req),
      restd:method('GET', Req),
      redirect(Path)
   ].

redirect([_, <<"1">>]) ->
   restd:to_text(redirect, [{<<"Location">>, <<"/get">>}], <<>>);

redirect([_, N]) ->
   restd:to_text(redirect, [{<<"Location">>, <<"/redirect/", (scalar:s(scalar:i(N) - 1))/binary>>}], <<>>).

%%
%% Returns cookie data.
%% Sets one or more simple cookies.
%% Deletes one or more simple cookies.
%%
cookies(Req) ->
   [either ||
      Path <- restd:path("/cookies/*", Req),
      restd:method('GET', Req),
      Cookie <- restd:header(<<"Cookie">>, Req),
      Values <- restd:q(Req),
      cookies(Path, Cookie, Values)
   ].

cookies([_], Cookies, _) ->
   restd:to_json(#{cookies => Cookies});

cookies([_, <<"set">>], _, Values) ->
   Head = [set_cookie(Key, Val) || {Key, Val} <- Values],
   restd:to_text(redirect, [{<<"Location">>, <<"/cookies">>} | Head], <<>>);

cookies([_, <<"delete">>], _, Values) ->
   Head = [set_cookie(Key) || Key <- Values],
   restd:to_text(redirect, [{<<"Location">>, <<"/cookies">>} | Head], <<>>).

set_cookie(Key, Val) ->
   {<<"Set-Cookie">>, <<(scalar:s(Key))/binary, $=, (scalar:s(Val))/binary, $;, "Path=/">>}.

set_cookie(Key) ->
   {<<"Set-Cookie">>, <<(scalar:s(Key))/binary, $=, $;, "expires=Thu, 01-Jan-1970 00:00:00 GMT; Max-Age=0; Path=/">>}.

