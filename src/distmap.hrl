
-define(ERROR    (Format), log:log('ERROR', self(), ?FILE, ?LINE, Format) ).
-define(WARNING  (Format), log:log('WARNING', self(), ?FILE, ?LINE, Format)).
-define(INFO     (Format), log:log('INFO', self(), ?FILE, ?LINE, Format)).
-define(DEBUG    (Format), log:log('DEBUG', self(), ?FILE, ?LINE, Format)).
-define(ERROR_   (Format, Args), log:log('ERROR',   self(), ?FILE, ?LINE, Format, Args)).
-define(WARNING_ (Format, Args), log:log('WARNING', self(), ?FILE, ?LINE, Format, Args)).
-define(INFO_    (Format, Args), log:log('INFO',    self(), ?FILE, ?LINE, Format, Args)).
-define(DEBUG_   (Format, Args), log:log('DEBUG',   self(), ?FILE, ?LINE, Format, Args)).
