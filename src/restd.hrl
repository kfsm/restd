
%%
%%
-record(request, {
   t      = undefined :: tempus:t(),
   mthd   = undefined :: atom(),
   head   = undefined :: [{binary(), _}],
   uri    = undefined :: uri:uri(),
   entity = undefined :: _
}).