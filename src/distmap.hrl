
-define(ERROR    (Format), dm_log:log('ERROR', self(), ?FILE, ?LINE, Format) ).
-define(WARNING  (Format), dm_log:log('WARNING', self(), ?FILE, ?LINE, Format)).
-define(INFO     (Format), dm_log:log('INFO', self(), ?FILE, ?LINE, Format)).
-define(DEBUG    (Format), dm_log:log('DEBUG', self(), ?FILE, ?LINE, Format)).
-define(ERROR_   (Format, Args), dm_log:log('ERROR',   self(), ?FILE, ?LINE, Format, Args)).
-define(WARNING_ (Format, Args), dm_log:log('WARNING', self(), ?FILE, ?LINE, Format, Args)).
-define(INFO_    (Format, Args), dm_log:log('INFO',    self(), ?FILE, ?LINE, Format, Args)).
-define(DEBUG_   (Format, Args), dm_log:log('DEBUG',   self(), ?FILE, ?LINE, Format, Args)).

-define(A2L(X), atom_to_list(X)).
-define(B2L(X), binary_to_list(X)).
-define(B2T(X), binary_to_term(X)).
-define(L2A(X), list_to_atom(X)).
-define(L2B(X), list_to_binary(X)).
-define(L2I(X), list_to_integer(X)).
-define(I2L(X), integer_to_list(X)).
-define(T2B(X), term_to_binary(X)).

