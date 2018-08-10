%% 
%% @doc
%%
-module(echo_restapi).
-compile({parse_transform, category}).

-export([filters/0, endpoints/0]).

%%
%%
filters() ->
   [
      restd:cors() 
   ].

%%
%%
endpoints() ->
   [
      restd:preflight(),
      ipaddr_json(),
      ipaddr_text(),
      user_agent(),
      headers(),
      http_get(),
      http_post(),
      http_post_json(),
      http_put(),
      http_patch(),
      http_delete(),
      utf8(),
      deflate(),
      gzip(),
      compress(),
      status_code(),
      response_header(),
      redirect_n(),
      cookies(),
      cookies_set(),
      cookies_del(),
      accesslog(),
      stream(),
      url_query(),
      absolute_url(),
      websocket(),

      restd_static:reader("/*", echo)
   ].


%%
%% Returns Origin IP.
%%
ipaddr_json() ->
   [reader ||
      _ /= restd:path("/ip"),
      _ /= restd:method('GET'),
      _ /= restd:provided_content({application, json}),
      Peer /= restd:header(<<"X-Knet-Peer">>),
      _ /= restd:to_json({ok, #{ip => Peer}})
   ].


ipaddr_text() ->
   [reader ||
      _ /= restd:path("/ip/text"),
      _ /= restd:method('GET'),
      Peer /= restd:header(<<"X-Knet-Peer">>),
      _ /= restd:to_text({ok, Peer})
   ].


%%
%% Returns user-agent.
%%
user_agent() ->
   [reader ||
      _ /= restd:path("/user-agent"),
      UA /= restd:header(<<"User-Agent">>),
      _ /= restd:to_json({ok, #{'user-agent' => UA}})
   ].

%%
%% Returns header dict.
%%
headers() ->
   [reader ||
      _ /= restd:path("/headers"),
      _ /= restd:method('GET'),
      _ /= restd:provided_content({application, json}),
      Head /= restd:headers(),
      _ /= restd:to_json({ok, #{headers => Head}})
   ].

%%
%% Returns GET data.
%%
http_get() ->
   [reader ||
      Url  /= restd:url("/get"),
      Mthd /= restd:method('GET'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
         _ /= restd:to_json({ok, #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd}})
   ].

%%
%% Returns POST data.
%% 
http_post() ->
   [reader ||
      Url  /= restd:url("/post"),
      Mthd /= restd:method('POST'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
         _ /= restd:accepted_content({'*', '*'}),
      Data /= restd:as_text(),
         _ /= restd:to_json({ok, #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd, data => Data}})
   ].

%%
%% Return POST JSON only
%%
http_post_json() ->
   [reader ||
      Url  /= restd:url("/post/json"),
      Mthd /= restd:method('POST'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
         _ /= restd:accepted_content({application, json}),
      Json /= restd:as_json(),
         _ /= restd:to_json({ok, #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd, data => Json}})
   ].

%%
%% Returns PUT data.
%% 
http_put() ->
   [reader ||
      Url  /= restd:url("/put"),
      Mthd /= restd:method('PUT'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
         _ /= restd:accepted_content({'*', '*'}),
      Data /= restd:as_text(),
         _ /= restd:to_json({ok, #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd, data => Data}})
   ].

%%
%% Returns PATCH data.
%% 
http_patch() ->
   [reader ||
      Url  /= restd:url("/patch"),
      Mthd /= restd:method('PATCH'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
         _ /= restd:accepted_content({'*', '*'}),
      Data <- restd:as_text(),
         _ /= restd:to_json({ok, #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd, data => Data}})
   ].


%%
%% Returns DELETE data.
%% 
http_delete() ->
   [reader ||
      Url  /= restd:url("/delete"),
      Mthd /= restd:method('DELETE'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
         _ /= restd:accepted_content({'*', '*'}),
      Data <- restd:as_text(),
         _ /= restd:to_json({ok, #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd, data => Data}})
   ].

%%
%% Returns page containing UTF-8 data.
%%
utf8() ->
   [reader ||
      _ /= restd:path("/encoding/utf8"),
      cats:unit({
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
   [reader ||
      Url  /= restd:url("/encoding/deflate"),
      Mthd /= restd:method('GET'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
      Encoding /= restd:provided_encoding(deflate), 
         _ /= restd:to_json([Encoding],
                  {ok, #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd}})
   ].

%%
%% Return gzip-encoded data.
%%
gzip() ->
   [reader ||
      Url  /= restd:url("/encoding/gzip"),
      Mthd /= restd:method('GET'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
      Encoding /= restd:provided_encoding(gzip), 
         _ /= restd:to_json([Encoding],
                  {ok, #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd}})
   ].

%%
%% Negotiate compression protocol and return encoded data
%%
compress() ->
   [reader ||
      Url  /= restd:url("/encoding/compress"),
      Mthd /= restd:method('GET'),
      Head /= restd:headers(),
      Peer /= restd:header(<<"X-Knet-Peer">>),
      Encoding /= restd:provided_encoding(), 
         _ /= restd:to_json([Encoding],
                  {ok, #{headers => Head, peer => Peer, url => uri:s(Url), method => Mthd}})
   ].

%%
%% Returns given HTTP Status code.
%%
status_code() ->
   [reader ||
      Path /= restd:path("/status/:code"),
      Code <- cats:unit( lens:get(lens:pair(<<"code">>), Path) ),
         _ /= restd:method('GET'),
         _ /= restd:to_text(scalar:i(Code), [], ["-=[ ", Code, " ]=-"])
   ].

%%
%% Return given response headers
%%
response_header() ->
   [reader ||
      _ /= restd:path("/response-headers"),
      _ /= restd:method('GET'),
      Query /= restd:q(),
      _ /= restd:to_json(Query, {ok, Query})
   ].

%%
%% 302 Redirects n times.
%%
redirect_n() ->
   [reader ||
      Path /= restd:path("/redirect/:nth"),
      Nth  <- cats:unit( lens:get(lens:pair(<<"nth">>), Path) ),

         _ /= restd:method('GET'),
       Uri <- redirect(Nth),
         _ /= restd:to_text(redirect, [{<<"Location">>, Uri}], <<$ >>)
   ].

redirect(<<"1">>) ->
   {ok, <<"/get">>};

redirect(N) ->
   {ok, <<"/redirect/", (scalar:s(scalar:i(N) - 1))/binary>>}.

%%
%% Returns cookie data.
%% Sets one or more simple cookies.
%% Deletes one or more simple cookies.
%%
cookies() ->
   [reader ||
         _ /= restd:path("/cookies"),
         _ /= restd:method('GET'),
      Cookie /= restd:header(<<"Cookie">>),
         _ /= restd:to_json({ok, #{cookies => Cookie}})
   ].

cookies_set() ->
   [reader ||
         _ /= restd:path("/cookies/set"),
         _ /= restd:method('GET'),
      Values /= restd:q(),
      Cookie <- set_cookies(Values),
         _ /= restd:to_json(redirect, [{<<"Location">>, <<"/cookies">>} | Cookie], <<>>)
   ].

cookies_del() ->
   [reader ||
         _ /= restd:path("/cookies/delete"),
         _ /= restd:method('GET'),
      Values /= restd:q(),
      Cookie <- del_cookies(Values),
         _ /= restd:to_json(redirect, [{<<"Location">>, <<"/cookies">>} | Cookie], <<>>)
   ].

set_cookies(Values) ->
   {ok, [set_cookie(Key, Val) || {Key, Val} <- Values]}.

del_cookies(Values) ->   
   {ok, [set_cookie(Key) || Key <- Values]}.

set_cookie(Key, Val) ->
   {<<"Set-Cookie">>, <<(scalar:s(Key))/binary, $=, (scalar:s(Val))/binary, $;, "Path=/">>}.

set_cookie(Key) ->
   {<<"Set-Cookie">>, <<(scalar:s(Key))/binary, $=, $;, "expires=Thu, 01-Jan-1970 00:00:00 GMT; Max-Age=0; Path=/">>}.

%%
%% trace access log
accesslog() ->
   [reader ||
      _ /= restd:path("/accesslog"),
      _ /= restd:method('GET'),
      Peer /= restd:header(<<"X-Knet-Peer">>),
      Http /= restd:to_text({ok, Peer}),
      _ /= restd:accesslog(Http)
   ].   


%%
%% Stream N chunks
%%
stream() ->
   [reader ||
      Path /= restd:path("/stream/:n"),
      N    <- cats:unit( lens:get(lens:pair(<<"n">>), Path) ),
         _ /= restd:method('GET'),
      cats:unit({200, [], stream_data(N)})
   ].

stream_data(N) ->
   stream:take(scalar:i(N),
      stream:cycle([
         <<"<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>\n">>
      ])
   ). 

%%
%% Decodes query variables
%%
url_query() ->
   [reader ||
      Path /= restd:path("/q"),
         _ /= cats:optionT(badarg, lens:get(lens:pair(<<"id">>, undefined), Path)),
         _ /= restd:method('GET'),
         _ /= restd:to_json({ok, Path})
   ].

%%
%% Build absolute url
%%
absolute_url() ->
   [reader ||
      _ /= restd:path("/host"),
      _ /= restd:method('GET'),

      Root /= restd:host(),
      _ /= restd:to_json({ok, uri:s(uri:path(<<"/ip">>, Root))})
   ].

%%
%% echo websocket
%%
websocket() ->
   [reader ||
           _ /= restd:path("/ws"),
      Packet /= restd:as_text(),
      cats:unit([<<$+, $+, $+, $ , Packet/binary>>])
   ].

