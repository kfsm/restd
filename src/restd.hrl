
%%
%% execution context of rest api resource
-record(resource, {
   service   = undefined   :: atom(),    %% service identity
   module    = undefined   :: atom(),    %% resource erlang module
   export    = undefined   :: atom(),    %% resource exported functions 

   request   = undefined   :: htstream:request(),
   head      = []          :: [_],       %% response headers
   env       = []          :: [{atom(), _}]
}).
