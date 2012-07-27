#include "erl_nif.h"
#include "callbacks.h" 
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <errno.h>

static ErlNifResourceType* cberl_handle = NULL;
static ERL_NIF_TERM a_ok;
static ERL_NIF_TERM a_error;

typedef struct handle {
    libcouchbase_t instance;
    ErlNifMutex * mutex;
} handle_t;



static void init_atoms(ErlNifEnv* env);

#define NIF(name)  ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
// Prototypes
NIF(cberl_nif_new);
NIF(cberl_nif_store);
NIF(cberl_nif_mget);
NIF(cberl_nif_getl);
NIF(cberl_nif_unlock);
NIF(cberl_nif_mtouch);
NIF(cberl_nif_arithmetic);
NIF(cberl_nif_remove);
NIF(cberl_nif_destroy);

static ERL_NIF_TERM return_lcb_error(ErlNifEnv* env, int const value);
static ERL_NIF_TERM return_value(ErlNifEnv* env, void * cookie);

static ErlNifFunc nif_funcs[] =
{
    {"new", 4, cberl_nif_new},
    {"store", 7, cberl_nif_store},
    {"mget", 4, cberl_nif_mget},
    {"getl", 4, cberl_nif_getl},
    {"unlock", 3, cberl_nif_unlock},
    {"mtouch", 4, cberl_nif_mtouch},
    {"arithmetic", 7, cberl_nif_arithmetic},
    {"remove", 3, cberl_nif_remove},
    {"destroy", 1, cberl_nif_destroy}
};


//nif functions
NIF(cberl_nif_new)
{
    char * host;
    char * user;
    char * pass;
    char * bucket;
   
    unsigned arg_length;
    if (! enif_get_list_length(env, argv[0], &arg_length)) {
        return enif_make_badarg(env);
    }
    host = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[0], host, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    if (! enif_get_list_length(env, argv[1], &arg_length)) {
        return enif_make_badarg(env);
    }
    user = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[1], user, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }    

    if (! enif_get_list_length(env, argv[2], &arg_length)) {
        return enif_make_badarg(env);
    }
    pass = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[2], pass, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
     
    if (! enif_get_list_length(env, argv[3], &arg_length)) {
        return enif_make_badarg(env);
    }
    bucket = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[3], bucket, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
    
    handle_t * handle = enif_alloc_resource(cberl_handle,
                                                  sizeof(handle_t));
    
    handle->instance = libcouchbase_create(host, user, pass, bucket, NULL);
    free(host);
    free(user);
    free(pass);
    free(bucket);

    if (handle->instance == NULL) {
        return enif_make_tuple2(env, a_error,
                enif_make_string(env, "Failed to create libcouchbase instance\n", ERL_NIF_LATIN1));
    }

    (void)libcouchbase_set_error_callback(handle->instance, error_callback);
    (void)libcouchbase_set_get_callback(handle->instance, get_callback); 
    (void)libcouchbase_set_storage_callback(handle->instance, storage_callback); 
    (void)libcouchbase_set_unlock_callback(handle->instance, unlock_callback);
    (void)libcouchbase_set_touch_callback(handle->instance, touch_callback);
    (void)libcouchbase_set_arithmetic_callback(handle->instance, arithmetic_callback);
    (void)libcouchbase_set_remove_callback(handle->instance, remove_callback);

    if (libcouchbase_connect(handle->instance) != LIBCOUCHBASE_SUCCESS) {
        return enif_make_tuple2(env, a_error,
                enif_make_string(env, "Failed to connect libcouchbase instance to server\n", ERL_NIF_LATIN1));
    }
    libcouchbase_wait(handle->instance);
    handle->mutex = enif_mutex_create("cberl_instance_mutex");
    return enif_make_tuple2(env, a_ok, enif_make_resource(env, handle));
}

NIF(cberl_nif_store)
{
    handle_t *  handle;
    int op;
    char * hashKey;
    char * key;
    void * value;
    ErlNifBinary value_binary;
    int exp;
    int flags;

    unsigned int nbytes;
    libcouchbase_error_t ret; 
    struct libcouchbase_callback cb; 
    unsigned arg_length;
    
    if (! enif_get_resource(env, argv[0], cberl_handle, (void **) &handle)) {
        return enif_make_badarg(env);
    } 

    if (! enif_get_int(env, argv[1], &op)) {
        return enif_make_badarg(env);
    }

    if (! enif_get_list_length(env, argv[2], &arg_length)) {
        return enif_make_badarg(env);
    }
    hashKey = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[2], hashKey, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }    

    if (! enif_get_list_length(env, argv[3], &arg_length)) {
        return enif_make_badarg(env);
    }
    key = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[3], key, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    } 

    if (! enif_get_int(env, argv[6], &flags)) {
        return enif_make_badarg(env);
    }
    switch(flags) {
        case 1:
            if (! enif_get_int(env, argv[4], (int*)value)) {
                return enif_make_badarg(env);
            }               
            nbytes = sizeof(int);
            break;
        case 2:
            if (! enif_get_list_length(env, argv[4], &nbytes)) {
                return enif_make_badarg(env);
            }
            value = (char *) malloc(nbytes + 1);
            if (! enif_get_string(env, argv[4], value, nbytes + 1, ERL_NIF_LATIN1)) {
                return enif_make_badarg(env);
            }
            break;
        case 3:
            if (! enif_inspect_iolist_as_binary(env, argv[4], &value_binary)) {
                return enif_make_badarg(env);
            }
            value = value_binary.data;
            nbytes = value_binary.size;
            break;
        default:
            return enif_make_badarg(env);
    }
    if (! enif_get_int(env, argv[5], &exp)) {
        return enif_make_badarg(env);
    }               

    
    enif_mutex_lock(handle->mutex);
    ret = libcouchbase_store_by_key(handle->instance,
                                  &cb,
                                  op,
                                  strlen(hashKey) == 0 ? NULL : hashKey,
                                  strlen(hashKey),
                                  key, /* the key or _id of the document */
                                  strlen(key), /* the key length */
                                  value,
                                  nbytes, /* length of */
                                  flags,  /* flags,  */
                                  exp,  /* expiration */
                                  0); /* and CAS values, see API reference */
    
    free(key);
    free(hashKey);
    if (ret != LIBCOUCHBASE_SUCCESS) {
        enif_mutex_unlock(handle->mutex);
        return return_lcb_error(env, ret);
    }
    libcouchbase_wait(handle->instance);
    enif_mutex_unlock(handle->mutex);
    if (cb.error != LIBCOUCHBASE_SUCCESS) {
        return return_lcb_error(env, cb.error);
    }
    return a_ok;
}

NIF(cberl_nif_mget)
{
    handle_t * handle;
    char * hashKey;
    char * key;
    int exp;

    libcouchbase_error_t ret; 
    struct libcouchbase_callback cb; 
    unsigned arg_length;
    
    if (! enif_get_resource(env, argv[0], cberl_handle,
                                         (void **) &handle)) {
        return enif_make_badarg(env);
    } 

    if (! enif_get_list_length(env, argv[1], &arg_length)) {
        return enif_make_badarg(env);
    }
    hashKey = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[1], hashKey, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }    
    
    if (! enif_get_list_length(env, argv[2], &arg_length)) {
        return enif_make_badarg(env);
    }
    key = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[2], key, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }    
    
    if (! enif_get_int(env, argv[3], &exp)) {
        return enif_make_badarg(env);
    }
    const char* keys[1];
    size_t nkey[1];
    keys[0] = key;
    nkey[0] = strlen(key);
    
    enif_mutex_lock(handle->mutex); 
    ret = libcouchbase_mget_by_key(handle->instance,
                             &cb,
                             strlen(hashKey) == 0 ? NULL : hashKey,
                             strlen(hashKey),
                             1,
                             (const void*const*)keys,
                             nkey,
                             exp == 0 ? NULL : (libcouchbase_time_t*)&exp); 
    free(key);
    free(hashKey);
    if (ret != LIBCOUCHBASE_SUCCESS) {
        enif_mutex_unlock(handle->mutex); 
        return_lcb_error(env, ret);
    }
    libcouchbase_wait(handle->instance);
    enif_mutex_unlock(handle->mutex); 
    if(cb.error != LIBCOUCHBASE_SUCCESS) {
        return return_lcb_error(env, cb.error);
    } 
    
    return enif_make_tuple2(env, a_ok, return_value(env, &cb)); 
}

NIF(cberl_nif_getl) {
    handle_t * handle;
    char * hashKey;
    char * key;
    int exp;

    libcouchbase_error_t ret; 
    struct libcouchbase_callback cb; 
    unsigned arg_length;
    
    if (! enif_get_resource(env, argv[0], cberl_handle,
                                         (void **) &handle)) {
        return enif_make_badarg(env);
    } 

    if (! enif_get_list_length(env, argv[1], &arg_length)) {
        return enif_make_badarg(env);
    }
    hashKey = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[1], hashKey, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }    
    
    if (! enif_get_list_length(env, argv[2], &arg_length)) {
        return enif_make_badarg(env);
    }
    key = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[2], key, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }    
    
    if (! enif_get_int(env, argv[3], &exp)) {
        return enif_make_badarg(env);
    }
    enif_mutex_lock(handle->mutex);
    ret = libcouchbase_getl_by_key(handle->instance,
                             &cb,
                             strlen(hashKey) == 0 ? NULL : hashKey,
                             strlen(hashKey),
                             key,
                             strlen(key),
                             exp == 0 ? NULL : (libcouchbase_time_t*)&exp); 
    free(key);
    free(hashKey);
    if (ret != LIBCOUCHBASE_SUCCESS) {
        enif_mutex_unlock(handle->mutex); 
        return_lcb_error(env, ret);
    }
    libcouchbase_wait(handle->instance);
    enif_mutex_unlock(handle->mutex); 
    if(cb.error != LIBCOUCHBASE_SUCCESS) {
        return return_lcb_error(env, cb.error);
    } 
    return enif_make_tuple2(env, a_ok, return_value(env, &cb));
    
}


NIF(cberl_nif_unlock)
{
    handle_t * handle;
    char * key;
    char * hashKey;
    libcouchbase_error_t ret; //for checking responses
    struct libcouchbase_callback cb; 
    unsigned arg_length;
    
    if (! enif_get_resource(env, argv[0], cberl_handle,
                                         (void **) &handle)) {
        return enif_make_badarg(env);
    } 

    if (! enif_get_list_length(env, argv[1], &arg_length)) {
        return enif_make_badarg(env);
    }
    hashKey = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[1], hashKey, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }    
    
    if (! enif_get_list_length(env, argv[2], &arg_length)) {
        return enif_make_badarg(env);
    }
    key = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[2], key, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
    enif_mutex_lock(handle->mutex);
    ret = libcouchbase_unlock_by_key(handle->instance,
                             &cb,
                             strlen(hashKey) == 0 ? NULL : hashKey,
                             strlen(hashKey),
                             key,
                             strlen(key),
                             0); 
    free(key);
    free(hashKey);

    if (ret != LIBCOUCHBASE_SUCCESS) {
        enif_mutex_unlock(handle->mutex); 
        return_lcb_error(env, ret);
    }
    libcouchbase_wait(handle->instance);
    enif_mutex_unlock(handle->mutex); 
    if(cb.error != LIBCOUCHBASE_SUCCESS) {
        return return_lcb_error(env, cb.error);
    } 
    return a_ok;

}

NIF(cberl_nif_mtouch)
{
    handle_t * handle;
    char * key;
    char * hashKey;
    int exp_time;
    libcouchbase_error_t ret; //for checking responses
    struct libcouchbase_callback cb; 
    unsigned arg_length;
    
    if (! enif_get_resource(env, argv[0], cberl_handle,
                                         (void **) &handle)) {
        return enif_make_badarg(env);
    } 

    if (! enif_get_list_length(env, argv[1], &arg_length)) {
        return enif_make_badarg(env);
    }
    hashKey = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[1], hashKey, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
    
    if (! enif_get_list_length(env, argv[1], &arg_length)) {
        return enif_make_badarg(env);
    }
    key = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[1], key, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }   

    if (! enif_get_int(env, argv[2], &exp_time)) {
        return enif_make_badarg(env);
    } 

    const char* keys[1];
    size_t nkey[1];
    keys[0] = key;
    nkey[0] = strlen(key);
    enif_mutex_lock(handle->mutex);
    ret = libcouchbase_mtouch_by_key(handle->instance,
                             &cb,
                             strlen(hashKey) == 0 ? NULL : hashKey,
                             strlen(hashKey),
                             1,
                             (const void*const*)keys,
                             nkey,
                             (libcouchbase_time_t*)&exp_time); 
    free(key);
    if (ret != LIBCOUCHBASE_SUCCESS) {
        enif_mutex_unlock(handle->mutex); 
        return_lcb_error(env, ret);
    }
    libcouchbase_wait(handle->instance);
    enif_mutex_unlock(handle->mutex); 
    if(cb.error != LIBCOUCHBASE_SUCCESS) {
        return return_lcb_error(env, cb.error);
    } 
    return a_ok;
}

NIF(cberl_nif_arithmetic) {
    handle_t * handle;
    char * key;
    char * hashKey;
    int64_t delta;
    uint64_t exp;
    int create;
    uint64_t initial; 

    libcouchbase_error_t ret; //for checking responses
    struct libcouchbase_callback cb; 
    unsigned arg_length;
    
    if (! enif_get_resource(env, argv[0], cberl_handle,
                                         (void **) &handle)) {
        return enif_make_badarg(env);
    } 

    if (! enif_get_list_length(env, argv[1], &arg_length)) {
        return enif_make_badarg(env);
    }
    hashKey = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[1], hashKey, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    if (! enif_get_list_length(env, argv[2], &arg_length)) {
        return enif_make_badarg(env);
    }
    key = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[2], key, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }   

    if (! enif_get_int64(env, argv[3], &delta)) {
        return enif_make_badarg(env);
    } 

    if (! enif_get_uint64(env, argv[4], &exp)) {
        return enif_make_badarg(env);
    }
    
    if (! enif_get_int(env, argv[5], &create)) {
        create = 0;
    }
    
    if (! enif_get_uint64(env, argv[6], &initial)) {
        initial = 0;
    }
    enif_mutex_lock(handle->mutex);
    ret = libcouchbase_arithmetic_by_key(handle->instance,
                                    &cb,
                                    strlen(hashKey) == 0 ? NULL : hashKey,
                                    strlen(hashKey),
                                    key,
                                    strlen(key),
                                    delta,
                                    exp, 
                                    create,
                                    initial); 
    free(key);
    if (ret != LIBCOUCHBASE_SUCCESS) {
        enif_mutex_unlock(handle->mutex); 
        return_lcb_error(env, ret);
    }
    libcouchbase_wait(handle->instance);
    enif_mutex_unlock(handle->mutex); 
    if(cb.error != LIBCOUCHBASE_SUCCESS) {
        return return_lcb_error(env, cb.error);
    } 
    return enif_make_tuple2(env, a_ok, return_value(env, &cb));
}

NIF(cberl_nif_remove) {
    handle_t * handle;
    char * key;
    char * hashKey;
    libcouchbase_error_t ret; //for checking responses
    struct libcouchbase_callback cb; 
    unsigned arg_length;
    
    if (! enif_get_resource(env, argv[0], cberl_handle,
                                         (void **) &handle)) {
        return enif_make_badarg(env);
    } 
    
    if (! enif_get_list_length(env, argv[1], &arg_length)) {
        return enif_make_badarg(env);
    }
    hashKey = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[1], hashKey, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }    
    
    if (! enif_get_list_length(env, argv[2], &arg_length)) {
        return enif_make_badarg(env);
    }
    key = (char *) malloc(arg_length + 1);
    if (! enif_get_string(env, argv[2], key, arg_length + 1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }    
    enif_mutex_lock(handle->mutex); 
    ret = libcouchbase_remove_by_key(handle->instance,
                             &cb,
                             strlen(hashKey) == 0 ? NULL : hashKey,
                             strlen(hashKey),
                             key,
                             strlen(key),
                             0); 
    free(key);
    if (ret != LIBCOUCHBASE_SUCCESS) {
        enif_mutex_unlock(handle->mutex); 
        return_lcb_error(env, ret);
    }
    libcouchbase_wait(handle->instance);
    enif_mutex_unlock(handle->mutex); 
    if(cb.error != LIBCOUCHBASE_SUCCESS) {
        return return_lcb_error(env, cb.error);
    } 
    return a_ok;
    
}

NIF(cberl_nif_destroy) {
    handle_t * handle;
    if (! enif_get_resource(env, argv[0], cberl_handle,
                                         (void **) &handle)) {
        return enif_make_badarg(env);
    }
    enif_mutex_lock(handle->mutex);
    libcouchbase_destroy(handle->instance);
    enif_mutex_unlock(handle->mutex);
    enif_mutex_destroy(handle->mutex);
    enif_release_resource(handle); 
    return a_ok;
}

static void init_atoms(ErlNifEnv* env)
{
    a_ok                         = enif_make_atom(env, "ok");
    a_error                      = enif_make_atom(env, "error");
}

static void cberl_handle_cleanup(ErlNifEnv* env, void* arg)
{
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    cberl_handle =  enif_open_resource_type(env, "cberl_nif",
                                                     "cberl_handle",
                                                     &cberl_handle_cleanup,
                                                     flags, 0);
    init_atoms(env);
    return 0;
}

static ERL_NIF_TERM return_value(ErlNifEnv* env, void * cookie) {
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    ErlNifBinary value_binary;
    switch(cb->flag) {
        case 1:
            return enif_make_int(env, *(int*)cb->data);
        case 2:
            return enif_make_string_len(env, cb->data, strlen(cb->data), ERL_NIF_LATIN1);
        case 3:
            enif_alloc_binary(cb->size, &value_binary);
            memcpy(value_binary.data, cb->data, cb->size);
            return enif_make_binary(env, &value_binary);
        default:
            //assume string
            return enif_make_string_len(env, cb->data, strlen(cb->data), ERL_NIF_LATIN1);
    }
}

static ERL_NIF_TERM return_lcb_error(ErlNifEnv* env, int const value){
    switch (value) {
        case LIBCOUCHBASE_SUCCESS:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "success"));
        case LIBCOUCHBASE_AUTH_CONTINUE:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "auth_continue"));
        case LIBCOUCHBASE_AUTH_ERROR:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "auth_error"));
        case LIBCOUCHBASE_DELTA_BADVAL:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "delta_badval"));
        case LIBCOUCHBASE_E2BIG:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "e2big"));
        case LIBCOUCHBASE_EBUSY:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "ebusy"));
        case LIBCOUCHBASE_EINTERNAL:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "einternal"));
        case LIBCOUCHBASE_EINVAL:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "einval"));
        case LIBCOUCHBASE_ENOMEM:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "enomem"));
        case LIBCOUCHBASE_ERANGE:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "erange"));
        case LIBCOUCHBASE_ERROR:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "error"));
        case LIBCOUCHBASE_ETMPFAIL:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "etmpfail"));
        case LIBCOUCHBASE_KEY_EEXISTS:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "key_eexists"));
        case LIBCOUCHBASE_KEY_ENOENT:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "key_enoent"));
        case LIBCOUCHBASE_LIBEVENT_ERROR:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "libevent_error"));
        case LIBCOUCHBASE_NETWORK_ERROR:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "network_error"));
        case LIBCOUCHBASE_NOT_MY_VBUCKET:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "not_my_vbucket"));
        case LIBCOUCHBASE_NOT_STORED:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "not_stored"));
        case LIBCOUCHBASE_NOT_SUPPORTED:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "not_supported"));
        case LIBCOUCHBASE_UNKNOWN_COMMAND:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "unknown_command"));
        case LIBCOUCHBASE_UNKNOWN_HOST:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "unknown_host"));
        case LIBCOUCHBASE_PROTOCOL_ERROR:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "protocol_error"));
        case LIBCOUCHBASE_ETIMEDOUT:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "etimedout"));
        case LIBCOUCHBASE_CONNECT_ERROR:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "connect_error"));
        case LIBCOUCHBASE_BUCKET_ENOENT:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "bucket_enoent"));
        case LIBCOUCHBASE_CLIENT_ENOMEM:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "client_enomem"));
        default:
            return enif_make_tuple2(env, a_error, enif_make_atom(env, "unknown_error"));            
    }
}
ERL_NIF_INIT(cberl_nif, nif_funcs, &on_load, NULL, NULL, NULL);
