
-define(ERROR    (Format), dm_log:log('ERROR', self(), ?FILE, ?LINE, Format) ).
-define(WARNING  (Format), dm_log:log('WARNING', self(), ?FILE, ?LINE, Format)).
-define(INFO     (Format), dm_log:log('INFO', self(), ?FILE, ?LINE, Format)).
-define(DEBUG    (Format), dm_log:log('DEBUG', self(), ?FILE, ?LINE, Format)).
-define(ERROR_   (Format, Args), dm_log:log('ERROR',   self(), ?FILE, ?LINE, Format, Args)).
-define(WARNING_ (Format, Args), dm_log:log('WARNING', self(), ?FILE, ?LINE, Format, Args)).
-define(INFO_    (Format, Args), dm_log:log('INFO',    self(), ?FILE, ?LINE, Format, Args)).
-define(DEBUG_   (Format, Args), dm_log:log('DEBUG',   self(), ?FILE, ?LINE, Format, Args)).
