-define('CBE_ADD',      1).
-define('CBE_REPLACE',  2).
-define('CBE_SET',      3).
-define('CBE_APPEND',   4).
-define('CBE_PREPEND',  5).

-define('CBE_JSON',     1).
-define('CBE_GZIP',     2).
-define('CBE_RAW',      3).

-type key() :: string().
-type value() :: string() | list() | integer() | binary().
-type operation_type() :: add | replace | set | append | prepend.
-opaque instance() :: binary().
