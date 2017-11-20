-module(restd_codec).
-compile({parse_transform, category}).

-export([
   encode_error/2,
   status_code/1,
   encode_form/1,
   decode_form/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% https://tools.ietf.org/html/rfc7807 
%%%   * "type" (string) - A URI reference that identifies the problem type.
%%%   * "title" (string) - A short, human-readable summary of the problem.
%%%   * "status" (number) - The HTTP status code
%%%   * "detail" (string) - A human-readable explanation specific to problem.
%%%   * "instance" (string) - A URI reference that identifies the specific problem
%%%
%%%----------------------------------------------------------------------------   

encode_error(Uri, Reason) ->
   {Code, Text} = encode_status_code(Reason),
   Json = #{
      type     => uri:s(uri:segments([Code], uri:new(<<"https://httpstatuses.com">>))),
      status   => Code,
      title    => Text,
      instance => uri:s(Uri),
      details  => encode_error_details(Reason)
   },
   {Code, [{<<"Content-Type">>, <<"application/json">>}], jsx:encode(Json)}.


encode_error_details({Reason, Details}) ->
   scalar:s([scalar:s(Reason), $:, $ , scalar:s(Details)]);
encode_error_details(Reason) ->
   scalar:s(Reason).

encode_status_code({Reason, _}) -> status_code(Reason);
encode_status_code(Reason) -> status_code(Reason).

%% encode rest api status code response
status_code(100) -> {100, <<"Continue">>};
status_code(101) -> {101, <<"Switching Protocols">>};
status_code(200) -> {200, <<"OK">>};
status_code(201) -> {201, <<"Created">>};
status_code(202) -> {202, <<"Accepted">>};
status_code(203) -> {203, <<"Non-Authoritative Information">>};
status_code(204) -> {204, <<"No Content">>};
status_code(205) -> {205, <<"Reset Content">>};
status_code(206) -> {206, <<"Partial Content">>};
status_code(300) -> {300, <<"Multiple Choices">>};
status_code(301) -> {301, <<"Moved Permanently">>};
status_code(302) -> {302, <<"Found">>};
status_code(303) -> {303, <<"See Other">>};
status_code(304) -> {304, <<"Not Modified">>};
status_code(307) -> {307, <<"Temporary Redirect">>};
status_code(400) -> {400, <<"Bad Request">>};
status_code(401) -> {401, <<"Unauthorized">>};
status_code(402) -> {402, <<"Payment Required">>};
status_code(403) -> {403, <<"Forbidden">>};
status_code(404) -> {404, <<"Not Found">>};
status_code(405) -> {405, <<"Method Not Allowed">>};
status_code(406) -> {406, <<"Not Acceptable">>};
status_code(407) -> {407, <<"Proxy Authentication Required">>};
status_code(408) -> {408, <<"Request Timeout">>};
status_code(409) -> {409, <<"Conflict">>};
status_code(410) -> {410, <<"Gone">>};
status_code(411) -> {411, <<"Length Required">>};
status_code(412) -> {412, <<"Precondition Failed">>};
status_code(413) -> {413, <<"Request Entity Too Large">>};
status_code(414) -> {414, <<"Request-URI Too Long">>};
status_code(415) -> {415, <<"Unsupported Media Type">>};
status_code(416) -> {416, <<"Requested Range Not Satisfiable">>};
status_code(417) -> {417, <<"Expectation Failed">>};
status_code(422) -> {422, <<"Unprocessable Entity">>};
status_code(500) -> {500, <<"Internal Server Error">>};
status_code(501) -> {501, <<"Not Implemented">>};
status_code(502) -> {502, <<"Bad Gateway">>};
status_code(503) -> {503, <<"Service Unavailable">>};
status_code(504) -> {504, <<"Gateway Timeout">>};
status_code(505) -> {505, <<"HTTP Version Not Supported">>};

%status_code(100) -> <<"100 Continue">>;
%status_code(101) -> <<"101 Switching Protocols">>;
status_code(ok)       -> status_code(200);
status_code(created)  -> status_code(201);
status_code(accepted) -> status_code(202);
%status(203) -> <<"203 Non-Authoritative Information">>;
status_code(no_content) -> status_code(204);
%status(205) -> <<"205 Reset Content">>;
%status(206) -> <<"206 Partial Content">>;
%status(300) -> <<"300 Multiple Choices">>;
%status(301) -> <<"301 Moved Permanently">>;
status_code(redirect) -> status_code(302);
%status(303) -> <<"303 See Other">>;
%status(304) -> <<"304 Not Modified">>;
%status(307) -> <<"307 Temporary Redirect">>;
status_code(badarg) -> status_code(400);
status_code(unauthorized) -> status_code(401);
status_code(expired) -> status_code(401);
%status(402) -> <<"402 Payment Required">>;
status_code(forbidden) -> status_code(403);
status_code(not_found) -> status_code(404);
status_code(enoent)    -> status_code(404);
status_code(not_allowed)    -> status_code(405);
status_code(not_acceptable) -> status_code(406);
%status(407) -> <<"407 Proxy Authentication Required">>;
%status(408) -> <<"408 Request Timeout">>;
status_code(conflict) -> status_code(409);
status_code(duplicate)-> status_code(409);
%status(410) -> <<"410 Gone">>;
%status(411) -> <<"411 Length Required">>;
%status(412) -> <<"412 Precondition Failed">>;
%status(413) -> <<"413 Request Entity Too Large">>;
%status(414) -> <<"414 Request-URI Too Long">>;
status_code(bad_mime_type) -> status_code(415);
%status(416) -> <<"416 Requested Range Not Satisfiable">>;
%status(417) -> <<"417 Expectation Failed">>;
%status(422) -> <<"422 Unprocessable Entity">>;
status_code(not_implemented) -> status_code(501);
%status(502) -> <<"502 Bad Gateway">>;
status_code(not_available) -> status_code(503);
%status(504) -> <<"504 Gateway Timeout">>;
%status(505) -> <<"505 HTTP Version Not Supported">>.
status_code(_) -> status_code(500).



%%%----------------------------------------------------------------------------   
%%%
%%% application/x-www-form-urlencoded
%%%
%%%----------------------------------------------------------------------------   

encode_form(Form) ->
   try
      {ok, [identity ||
         maps:to_list(Form),
         lists:map(fun to_pair/1, _),
         scalar:s(lists:join(<<$&>>, _))
      ]}
   catch _:_ ->
      {error, {badarg, payload}}
   end.

to_pair(Pair) ->
   scalar:s(
      lists:join(<<$=>>, 
         [uri:escape(X) || X <- erlang:tuple_to_list(Pair)]
      )
   ).


decode_form(Form) ->
   try
      {ok, [identity ||
         binary:split(scalar:s(Form), <<$&>>, [trim, global]),
         lists:map(fun as_pair/1, _),
         maps:from_list(_)
      ]}
   catch _:_ ->
      {error, {badarg, payload}}
   end.
   

as_pair(Pair) ->
   erlang:list_to_tuple(
      [uri:unescape(X) || X <- binary:split(Pair, <<$=>>)]
   ).
