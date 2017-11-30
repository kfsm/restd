
%%
%%
-record(request, {
   t      = undefined :: tempus:t(),      %% time when request is scheduled 
   mthd   = undefined :: atom(),          %% HTTP method
   head   = undefined :: [{binary(), _}], %% HTTP headers 
   uri    = undefined :: uri:uri(),       %% resource URI
   entity = undefined :: _                %% HTTP payload 
}).


-ifdef(CONFIG_DEBUG).
   -define(DEBUG(Str, Args), lager:debug(Str, Args)).
-else.
   -define(DEBUG(Str, Args), ok).
-endif.
