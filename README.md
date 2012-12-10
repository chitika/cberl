CBERL
====

NIF based Erlang bindings for couchbase based on libcouchbase. 
CBERL is at  very early stage of development, it only supports very basic functionality. Please submit bugs and patches if you find any.
Only tested on mac so far.

Quick Start
---------
You must have libcouchbase installed. For more information how to visit: http://www.couchbase.com/develop/c/next. You can then normally compile or include it in your project.
    
    ./rebar compile
    erl -pa ebin deps/jiffy/ebin

Example
-------

Make sure you have couchbase running on localhost or use cberl:new(Host) instead.

    1> {ok, Instance} = cberl:new().
    {ok,{instance,<<>>,cberl_transcoder}}
    2> cberl:set(Instance, "fkey", 0, "cberl").
    ok
    3> cberl:get(Instance, "fkey").
    {ok,0,"cberl"}

For more information on all the functions -> ./rebar doc 

TODO
----

1) Write more tests

2) Add informational methods

3) Make function signatures more compatible with other couchbase clients
