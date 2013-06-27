#ifndef CBERL_H
#define CBERL_H

#include <libcouchbase/couchbase.h>
#include "queue.h"
#include "erl_nif.h"

#define A_OK(env)            enif_make_atom(env, "ok")
#define A_ERROR(env)    enif_make_atom(env, "error")

#define NIF(name)  ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])

#define assert_badarg(S, Env) if (! S) { return enif_make_badarg(env); }

typedef struct handle {
    ErlNifTid thread;
    ErlNifThreadOpts* thread_opts;
    queue_t *queue;
    ERL_NIF_TERM (*calltable[9])(ErlNifEnv* env, struct handle* handle, void* obj);
    void* (*args_calltable[9])(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    lcb_t instance;
} handle_t;

#endif
