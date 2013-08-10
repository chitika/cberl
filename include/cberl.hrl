-define('CBE_ADD',      1).
-define('CBE_REPLACE',  2).
-define('CBE_SET',      3).
-define('CBE_APPEND',   4).
-define('CBE_PREPEND',  5).

-type handle() :: binary().

-record(instance, {handle :: handle(), 
                   transcoder :: module()}).

-type key() :: string().
-type value() :: string() | list() | integer() | binary().
-type operation_type() :: add | replace | set | append | prepend.
-type instance() :: #instance{}.
-type http_type() :: view | management | raw.
