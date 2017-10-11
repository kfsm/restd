%% 
%% @doc
%%
-module(echo_restapi).
-compile({parse_transform, category}).

-export([
   ipaddr_json/0,
   ipaddr_text/0,
   user_agent/0,
   headers/0,
   get/0,
   post/0,
   post_json/0,
   put/0,
   patch/0,
   delete/0,
   utf8/0,
   deflate/0,
   gzip/0,
   compress/0,
   status_code/0,
   response_header/0,
   redirect_n/0,
   cookies/0,
   stream/0
]).

%%
%% Returns Origin IP.
%%
ipaddr_json() ->
   [pattern ||
      _ /= restd:path("/ip"),
      _ /= restd:method('GET'),
      _ /= restd:provided_content({application, json}),
      Peer /= restd:header(<<"X-Knet-Peer">>),
      restd:to_json(#{ip => Peer})
   ].


ipaddr_text() ->
   [pattern ||
      _ /= restd:path("/ip"),
      _ /= restd:method('GET'),
      _ /= restd:provided_content({text, plain}),
      Peer /= restd:header(<<"X-Knet-Peer">>),
      restd:to_text(Peer)
   ].


%%
%% Returns user-agent.
%%
user_agent() ->
   [pattern ||
      _ /= restd:path("/user-agent"),
      UA /= restd:header(<<"User-Agent">>),
      restd:to_json(#{'user-agent' => UA})
   ].

%%
%% Returns header dict.
%%
headers() ->
   [pattern ||
      _ /= restd:path("/headers"),
      _ /= restd:method('GET'),
      _ /= restd:provided_content({application, json}),
      Head /= restd:headers(),
      restd:to_json(#{headers => Head})
   ].

%%
%% Returns GET data.
%%
get() ->
   [pattern ||
      Url  /= restd:url("/get"),
      Mthd /= restd:method('GET'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
      restd:to_json(#{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd})
   ].

%%
%% Returns POST data.
%% 
post() ->
   [pattern ||
      Url  /= restd:url("/post"),
      Mthd /= restd:method('POST'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
         _ /= restd:accepted_content({'*', '*'}),
      Data /= restd:as_text(),
      restd:to_json(#{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd, data => Data})
   ].

%%
%% Return POST JSON only
%%
post_json() ->
   [pattern ||
      Url  /= restd:url("/post/json"),
      Mthd /= restd:method('POST'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
         _ /= restd:accepted_content({application, json}),
      Json /= restd:as_json(),
      restd:to_json(#{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd, data => Json})
   ].

%%
%% Returns PUT data.
%% 
put() ->
   [pattern ||
      Url  /= restd:url("/put"),
      Mthd /= restd:method('PUT'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
         _ /= restd:accepted_content({'*', '*'}),
      Data /= restd:as_text(),
      restd:to_json(#{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd, data => Data})
   ].

%%
%% Returns PATCH data.
%% 
patch() ->
   [pattern ||
      Url  /= restd:url("/patch"),
      Mthd /= restd:method('PATCH'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
         _ /= restd:accepted_content({'*', '*'}),
      Data <- restd:as_text(),
      restd:to_json(#{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd, data => Data})
   ].


%%
%% Returns DELETE data.
%% 
delete() ->
   [pattern ||
      Url  /= restd:url("/delete"),
      Mthd /= restd:method('DELETE'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
        _  /= restd:accepted_content({'*', '*'}),
      Data <- restd:as_text(),
      restd:to_json(#{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd, data => Data})
   ].

%%
%% Returns page containing UTF-8 data.
%%
utf8() ->
   [pattern ||
      _ /= restd:path("/encoding/utf8"),
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
deflate() ->
   [pattern ||
      Url  /= restd:url("/encoding/deflate"),
      Mthd /= restd:method('GET'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
      Encoding /= restd:provided_encoding(deflate), 
      restd:to_json([Encoding],
         #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd})
   ].

%%
%% Return gzip-encoded data.
%%
gzip() ->
   [pattern ||
      Url  /= restd:url("/encoding/gzip"),
      Mthd /= restd:method('GET'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
      Encoding /= restd:provided_encoding(gzip), 
      restd:to_json([Encoding],
         #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd})
   ].

%%
%% Negotiate compression protocol and return encoded data
%%
compress() ->
   [pattern ||
      Url  /= restd:url("/encoding/compress"),
      Mthd /= restd:method('GET'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
      Encoding /= restd:provided_encoding(), 
      restd:to_json([Encoding],
         #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd})
   ].

%%
%% Returns given HTTP Status code.
%%
status_code() ->
   [pattern ||
      Path /= restd:path("/status/_"),
         _ /= restd:method('GET'),
      do_status_code(Path)
   ].

do_status_code([_, Code]) ->
   restd:to_text(scalar:i(Code), [], ["-=[ ", Code, " ]=-"]).

%%
%% Return given response headers
%%
response_header() ->
   [pattern ||
      _ /= restd:path("/response-headers"),
      _ /= restd:method('GET'),
      Query /= restd:q(),
      restd:to_json(Query, Query)
   ].

%%
%% 302 Redirects n times.
%%
redirect_n() ->
   [pattern ||
      Path /= restd:path("/redirect/_"),
         _ /= restd:method('GET'),
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
cookies() ->
   [pattern ||
      Path /= restd:path("/cookies/*"),
         _ /= restd:method('GET'),
      Cookie /= restd:header(<<"Cookie">>),
      Values /= restd:q(),
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

%%
%% Stream N chunks
%%
stream() ->
   [pattern ||
      Path /= restd:path("/stream/_"),
         _ /= restd:method('GET'),
         fmap({200, [], stream_data(Path)})
   ].

stream_data([_, N]) ->
   stream:take(scalar:i(N),
      stream:cycle([
         <<"<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>\n">>
      ])
   ). 


