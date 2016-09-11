CBERL
====

[![Build Status](https://travis-ci.org/chitika/cberl.svg?branch=master)](https://travis-ci.org/chitika/cberl)

NIF based Erlang bindings for couchbase based on libcouchbase. 
CBERL is at early stage of development, it only supports very basic functionality. Please submit bugs and patches if you find any.
Tested on OSX, Debian Squeeze and Amazon Linux.

Quick Setup/Start
---------

First you must have libcouchbase installed. 

On OSX install [homebrew](http://mxcl.github.com/homebrew/,"homebrew") if you haven't already then run:

```shell
brew update && brew install libcouchbase
```

On Amazon linux:

```shell
sudo wget -O/etc/yum.repos.d/couchbase.repo http://packages.couchbase.com/rpm/couchbase-centos62-x86_64.repo
sudo yum check-update
sudo yum install --enablerepo=epel libcouchbase2 libcouchbase-devel
```

For installing libcouchbase on other systems visit http://www.couchbase.com/develop/c/current.

Then:

```shell
git clone git@github.com:chitika/cberl.git
cd cberl
# assuming you have rebar in your path
rebar get-deps compile
```

Or just include it as a dependency in your rebar config.
    

Example
-------

Make sure you have couchbase running on localhost or use cberl:new(Host) instead.

```erlang
% create a connection pool  of 5 connections named cberl_default
% you can provide more argument like host, username, password, 
% bucket and transcoder - look at https://github.com/wcummings/cberl/blob/master/src/cberl.erl for more detail 
cberl:start_link(cberl_default, 5).
{ok, <0.33.0>}
% Poolname, Key, Expire - 0 for infinity, Value
cberl:set(cberl_default, <<"fkey">>, 0, <<"cberl">>).
ok
cberl:get(cberl_default, <<"fkey">>).
{<<"fkey">>, ReturnedCasValue, <<"cberl">>}
```

For more information on all the functions -> ./rebar doc (most of documentation is out of date right now)

Views
-----

cberl has support for querying views via the view/4 functions:

```erlang
cberl:view(cberl_default, "all", "all", []).
{ok,{1,
    [[{<<"id">>,<<"test">>},
    {<<"key">>,<<"test">>},
    {<<"value">>,null}]]}}
```

Shorthand for foldl, foldr and foreach are also provided.

N1QL
-----

cberl has support for N1QL queries via the n1ql/4 and n1ql/5 functions:

```erlang
Dog = {[{<<"type">>,<<"dog">>},
  {<<"name">>,<<"tom">>},
  {<<"age">>,5},
  {<<"color">>,<<"white">>}]}.
cberl:set(cberl_default, <<"tom">>, 0, Dog).
cberl:n1ql(cberl_default, <<"SELECT * FROM default WHERE type=$1 and age=$2 and color=$3">>, [<<"\"dog\"">>, <<"5">>, <<"\"white\"">>], false).
{ok,{[{<<"requestID">>,
       <<"a00b80e8-aea8-4a23-a0a9-5aba26d2b48f">>},
      {<<"signature">>,{[{<<"*">>,<<"*">>}]}},
      {<<"results">>,[]},
      {<<"status">>,<<"success">>},
      {<<"metrics">>,
       {[{<<"elapsedTime">>,<<"21.644116ms">>},
         {<<"executionTime">>,<<"21.60625ms">>},
         {<<"resultCount">>,1},
         {<<"resultSize">>,171}]}}]},
    [{[{<<"default">>,
        {[{<<"age">>,5},
          {<<"color">>,<<"white">>},
          {<<"name">>,<<"tom">>},
          {<<"type">>,<<"dog">>}]}}]}]}
```

Custom Transcoders
-----

You can have your custom transcoders, your transcoder must export 3 functions:

__encode_value/2:__

Takes in an encoder|encoder list and the original value and turns it into a binary.

__decode_value/2:__

Takes in a flag (from couchbase) and the value (as binary) and turns it into the actual value.

__flag/1:__

Turns an encoder_name (or list of them) into an integer. This value is sent to CB during set operations and this is what you get in decode value. You must return a value for 'standard' encoder if you are not planning to specify an encoder for every set operation.

Check out [cberl_transcoder.erl](https://github.com/wcummings/cberl/blob/master/src/cberl_transcoder.erl) it is pretty straightforward.
