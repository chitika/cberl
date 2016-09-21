#include <string.h>
#include <stdio.h>
#include "cberl.h"
#include "cberl_nif.h"
#include "cb.h"

static ErlNifResourceType* cberl_handle = NULL;

static void cberl_handle_cleanup(ErlNifEnv* env, void* arg) {}

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    cberl_handle =  enif_open_resource_type(env, "cberl_nif",
                                                 "cberl_handle",
                                                 &cberl_handle_cleanup,
                                                 flags, 0);
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv_data, ERL_NIF_TERM load_info)
{
    return load(env, priv, load_info);
}

NIF(cberl_nif_new)
{
    handle_t* handle = enif_alloc_resource(cberl_handle, sizeof(handle_t));
    handle->queue = queue_new();

    handle->calltable[CMD_CONNECT]         = cb_connect;
    handle->args_calltable[CMD_CONNECT]    = cb_connect_args;
    handle->calltable[CMD_STORE]           = cb_store;
    handle->args_calltable[CMD_STORE]      = cb_store_args;
    handle->calltable[CMD_MGET]            = cb_mget;
    handle->args_calltable[CMD_MGET]       = cb_mget_args;
    handle->calltable[CMD_UNLOCK]          = cb_unlock;
    handle->args_calltable[CMD_UNLOCK]     = cb_unlock_args;
    handle->calltable[CMD_MTOUCH]          = cb_mtouch;
    handle->args_calltable[CMD_MTOUCH]     = cb_mtouch_args;
    handle->calltable[CMD_ARITHMETIC]      = cb_arithmetic;
    handle->args_calltable[CMD_ARITHMETIC] = cb_arithmetic_args;
    handle->calltable[CMD_REMOVE]          = cb_remove;
    handle->args_calltable[CMD_REMOVE]     = cb_remove_args;
    handle->calltable[CMD_HTTP]            = cb_http;
    handle->args_calltable[CMD_HTTP]       = cb_http_args;
    handle->calltable[CMD_N1QL]            = cb_n1ql;
    handle->args_calltable[CMD_N1QL]       = cb_n1ql_args;

    handle->thread_opts = enif_thread_opts_create("thread_opts");

    if (enif_thread_create("", &handle->thread, worker, handle, handle->thread_opts) != 0) {
        return enif_make_atom(env, "error");
    }

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_resource(env, handle));
}

NIF(cberl_nif_control)
{
    handle_t* handle;

    assert_badarg(enif_get_resource(env, argv[0], cberl_handle, (void **) &handle), env);

    unsigned int len;
    enif_get_atom_length(env, argv[1], &len, ERL_NIF_LATIN1);
    int cmd;
    enif_get_int(env, argv[1], &cmd);

    if (cmd == -1) {
        return enif_make_badarg(env);
    }

    ErlNifPid* pid = (ErlNifPid*)enif_alloc(sizeof(ErlNifPid));
    task_t* task = (task_t*)enif_alloc(sizeof(task_t));

    unsigned arg_length;
    if (!enif_get_list_length(env, argv[2], &arg_length)) {
        enif_free(pid);
        enif_free(task);
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM nargs = argv[2];
    ERL_NIF_TERM head, tail;
    ERL_NIF_TERM* new_argv = (ERL_NIF_TERM*)enif_alloc(sizeof(ERL_NIF_TERM) * arg_length);
    int i = 0;
    while (enif_get_list_cell(env, nargs, &head, &tail)) {
        new_argv[i] = head;
        i++;
        nargs = tail;
    }

    void* args = handle->args_calltable[cmd](env, arg_length, new_argv);

    enif_free(new_argv);

    if(args == NULL) {
        enif_free(pid);
        enif_free(task);
        return enif_make_badarg(env);
    }

    enif_self(env, pid);

    task->pid  = pid;
    task->cmd  = cmd;
    task->args = args;

    queue_put(handle->queue, task);

    return A_OK(env);
}

NIF(cberl_nif_destroy) {
    handle_t * handle;
    void* resp;
    assert_badarg(enif_get_resource(env, argv[0], cberl_handle, (void **) &handle), env);      
    queue_put(handle->queue, NULL); // push NULL into our queue so the thread will join
    enif_thread_join(handle->thread, &resp);
    queue_destroy(handle->queue);
    enif_thread_opts_destroy(handle->thread_opts);
    lcb_destroy(handle->instance);
    enif_release_resource(handle); 
    return A_OK(env);
}

static void* worker(void *obj)
{
    handle_t* handle = (handle_t*)obj;

    task_t* task;
    ErlNifEnv* env = enif_alloc_env();

    while ((task = (task_t*)queue_get(handle->queue)) != NULL) {
        ERL_NIF_TERM result = handle->calltable[task->cmd](env, handle, task->args);
        enif_send(NULL, task->pid, env, result);
        enif_free(task->pid);
        enif_free(task->args);
        enif_free(task);
        enif_clear_env(env);
    }

    return NULL;
}

static ErlNifFunc nif_funcs[] = {
    {"new", 0, cberl_nif_new},
    {"control", 3, cberl_nif_control},
    {"destroy", 1, cberl_nif_destroy}
};

ERL_NIF_INIT(cberl_nif, nif_funcs, load, NULL, upgrade, NULL);
