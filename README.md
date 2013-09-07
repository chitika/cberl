CBERL
====

NIF based Erlang bindings for couchbase based on libcouchbase. 
CBERL is at early stage of development, it only supports very basic functionality. Please submit bugs and patches if you find any.
Tested on mac, debian squeeze and amazon linux.

Quick Setup/Start
---------
First you must have libcouchbase installed. 

On mac install [homebrew](http://mxcl.github.com/homebrew/,"homebrew") if you haven't already then run:

    brew install https://github.com/couchbase/homebrew/raw/stable/Library/Formula/libcouchbase.rb

On Amazon linux:

    sudo wget -O/etc/yum.repos.d/couchbase.repo http://packages.couchbase.com/rpm/couchbase-centos62-x86_64.repo
    sudo yum check-update
    sudo yum install --enablerepo=epel libcouchbase2 libcouchbase-devel

For installing libcouchbase on other systems visit http://www.couchbase.com/develop/c/current.


Then:

    git clone git@github.com:chitika/cberl.git
    cd cberl
    ### assuming you have rebar in your path
    rebar get-deps compile

Or just include it as a dependency in your rebar config.
    

Example
-------

Make sure you have couchbase running on localhost or use cberl:new(Host) instead.

    %% create a connection pool  of 5 connections named cberl_default
    %% you can provide more argument like host, username, password, 
    %% bucket and transcoder - look at [cberl.erl](https://github.com/wcummings/cberl/blob/master/src/cberl.erl) for more detail 
    cberl:start_link(cberl_default, 5).
    {ok, <0.33.0>}
    %% Poolname, Key, Expire - 0 for infinity, Value
    cberl:set(cberl_default, <<"fkey">>, 0, <<"cberl">>).
    ok
    cberl:get(cberl_default, <<"fkey">>).
    {<<"fkey">>, ReturnedCasValue, <<"cberl">>}

For more information on all the functions -> ./rebar doc (most of documentation is out of date right now)

Views
-----

cberl has new (experimental) support for querying views via the view/4 functions:

    cberl:view(default, "all", "all", []).
    {ok,{1,
     [[{<<"id">>,<<"test">>},
       {<<"key">>,<<"test">>},
       {<<"value">>,null}]]}}

Shorthand for foldl, foldr and foreach are also provided.

Custom Transcoders
-----

You can have your custom transcoders, your transcoder must export 3 functions:

__encode_value/2:__

Takes in an encoder|encoder list and the original value and turns it into a binary.

__decode_value/2:__

Takes in a flag (from couchbase) and the value (as binary) and turns it into the actual value.

__flag/1:__

Turns an encoder_name (or list of them) into an integer. This value is sent to CB during set operations and this is what you get in decode value. You must return a value for 'standart' encoder if you are not planning to specify an encoder for every set operation.

Check out [cberl_transcoder.erl](https://github.com/wcummings/cberl/blob/master/src/cberl_transcoder.erl) it is pretty straightforward.

Performance
-------

I included [results](https://github.com/wcummings/cberl/blob/master/bench/macmini_cberl_new.png) of [basho_bench](http://docs.basho.com/riak/latest/cookbooks/Benchmarking/) which I ran on my mac. It is the results of 100 processes using a pool of 5 connections. I included basha_bench driver and config file under bench. Please tweak the config file for your requirement and run your own benchmarks.

TODO
----

1) Update documentation

2) Write more tests
