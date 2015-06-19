%% @description
%%    webapp container
-module(restd_api_webapp).

-export([
	content_provided/1, 
	exists/2,
   'GET'/2
]).

%%
%%
content_provided(_Req) ->
   [{'*', '*'}].

%%
%%
exists(_, {Url, _Heads, Env}) ->
	filelib:is_file(filename(Url, Env)).

%%
%%
'GET'(_, {Url, _Heads, Env}) ->
	Filename   = filename(Url, Env),
	{ok, File} = file:read_file(Filename),
	{ok, 
      [
         {'Content-Type', mime(filename:extension(Filename))}
        ,{'Access-Control-Allow-Origin', <<"*">>}
      ], 
      File
   }.

%%
%%
filename(Url, Env) ->
	case opts:val(<<"file">>, undefined, Env) of
		undefined ->	
			case uri:segments(Url) of
				undefined -> 
				   filename:join([htdoc(Env), "index.html"]);
				[]        ->
				   filename:join([htdoc(Env), "index.html"]);
				List      ->
					filename:join([htdoc(Env)] ++ [scalar:c(X) || X <- List, X =/= <<".">>, X =/= <<"..">>, X =/= <<$~>>])
			end;
		File      ->
			filename:join([htdoc(Env), scalar:c(File)])
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
mime([])       -> {text, html};
mime(".html")  -> {text, html};
mime(".txt")   -> {text, plain};
mime(".css")   -> {text, css};
mime(".js")    -> {text, javascript};
mime(".xml")   -> {text, xml};

mime(".json")  -> {application, json};
mime(".png")   -> {image, png};
mime(".jpg")   -> {image, jpeg};
mime(".jpeg")  -> {image, jpeg};
mime(".svg")   -> {image, 'svg+xml'};


mime(".pdf")   -> {application, pdf};

mime(_)        -> {application, 'octet-stream'}.
