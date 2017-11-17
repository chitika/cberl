-define('CBE_ADD',      1).
-define('CBE_REPLACE',  2).
-define('CBE_SET',      3).
-define('CBE_APPEND',   4).
-define('CBE_PREPEND',  5).

-define('CMD_CONNECT',    0).
-define('CMD_STORE',      1).
-define('CMD_MGET',       2).
-define('CMD_UNLOCK',     3).
-define('CMD_MTOUCH',     4).
-define('CMD_ARITHMETIC', 5).
-define('CMD_REMOVE',     6).
-define('CMD_HTTP',       7).
-define('CMD_N1QL',       8).

-type handle() :: binary().

-record(instance, {handle :: handle(),
                   bucketname :: string(),
                   transcoder :: module(),
                   connected :: true | false,
                   opts :: list()}).

-type key() :: string().
-type value() :: string() | list() | integer() | binary().
-type operation_type() :: add | replace | set | append | prepend.
-type instance() :: #instance{}.
-type http_type() :: view | management | raw.
-type http_method() :: get | post | put | delete.
