CBERL
====

NIF based Erlang bindings for couchbase based on libcouchbase. 
CBERL is at  very early stage of development, it only supports very basic functionality. Please submit bugs and patches if you find any.

Quick Start
---------
if you don't have libcouchbase already installed run ./rebar get-deps then:

    ./rebar compile
    erl -pa ebin

Example
-------

Make sure you have couchbase running on localhost or use cberl:new(Host) instead.

    1> {ok, Instance} = cberl:new().
    {ok,<<>>}
    2> cberl:set(Instance, "fkey", "cberl").
    ok
    3> cberl:mget(Instance, "fkey").
    {ok,"cberl"}


For more information on all the functions -> ./rebar doc 

TODO
----

1) Write more tests

2) Support batch operations, right now all operations are performed on single key

3) Add informational methods

4) Make function signatures more compatible with other couchbase clients
