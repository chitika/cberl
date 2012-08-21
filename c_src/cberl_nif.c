#include "erl_nif.h"
#include "callbacks.h" 
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
#define assert_badarg(S, Env) if (! S) { return enif_make_badarg(Env); }
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
    {"mget", 3, cberl_nif_mget},
    {"getl", 3, cberl_nif_getl},
    {"unlock", 3, cberl_nif_unlock},
    {"mtouch", 3, cberl_nif_mtouch},
    {"arithmetic", 6, cberl_nif_arithmetic},
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
    assert_badarg(enif_get_list_length(env, argv[0], &arg_length), env);       
    host = (char *) malloc(arg_length + 1);
    assert_badarg(enif_get_string(env, argv[0], host, arg_length + 1, ERL_NIF_LATIN1), env);       

    assert_badarg(enif_get_list_length(env, argv[1], &arg_length), env);       
    user = (char *) malloc(arg_length + 1);
    assert_badarg(enif_get_string(env, argv[1], user, arg_length + 1, ERL_NIF_LATIN1), env);           

    assert_badarg(enif_get_list_length(env, argv[2], &arg_length), env);       
    pass = (char *) malloc(arg_length + 1);
    assert_badarg(enif_get_string(env, argv[2], pass, arg_length + 1, ERL_NIF_LATIN1), env);       
     
    assert_badarg(enif_get_list_length(env, argv[3], &arg_length), env);       
    bucket = (char *) malloc(arg_length + 1);
    assert_badarg(enif_get_string(env, argv[3], bucket, arg_length + 1, ERL_NIF_LATIN1), env);       
    
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
    struct libcouchbase_callback cb; 
    int operation;
    void * key;
    unsigned int nkey;
    void * bytes;
    libcouchbase_size_t nbytes;
    libcouchbase_uint32_t flags;
    int exp;
    libcouchbase_cas_t cas;

    ErlNifBinary value_binary;
    libcouchbase_error_t ret; 
    
    assert_badarg(enif_get_resource(env, argv[0], cberl_handle, (void **) &handle), env);        
    assert_badarg(enif_get_int(env, argv[1], &operation), env);       
    assert_badarg(enif_get_list_length(env, argv[2], &nkey), env);       
    nkey += 1;
    key = (char *) malloc(nkey);
    assert_badarg(enif_get_string(env, argv[2], key, nkey, ERL_NIF_LATIN1), env);           
    assert_badarg(enif_inspect_iolist_as_binary(env, argv[3], &value_binary), env); 
    
    bytes = malloc(value_binary.size);
    memcpy(bytes, value_binary.data, value_binary.size);
    nbytes = value_binary.size;

    assert_badarg(enif_get_uint(env, argv[4], &flags), env);       
    assert_badarg(enif_get_int(env, argv[5], &exp), env);                      
    assert_badarg(enif_get_uint64(env, argv[6], (unsigned long*)&cas), env);       

    enif_mutex_lock(handle->mutex);
    ret = libcouchbase_store(handle->instance,
                                  &cb,
                                  operation,
                                  key, /* the key or _id of the document */
                                  nkey, /* the key length */
                                  bytes,
                                  nbytes, /* length of */
                                  flags,  /* flags,  */
                                  exp,  /* expiration */
                                  cas); /* and CAS values, see API reference */
    
    free(key);
    free(bytes);
    
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
    struct libcouchbase_callback_m cb; 
    unsigned int numkeys;
    void** keys;
    size_t* nkeys;
    int exp;

    libcouchbase_error_t ret;
    
    ERL_NIF_TERM* results;
    ERL_NIF_TERM* currKey;
    ERL_NIF_TERM returnValue;
    ERL_NIF_TERM tail;
    ErlNifBinary *databin;
    
    assert_badarg(enif_get_resource(env, argv[0], cberl_handle, (void **) &handle), env);      
    assert_badarg(enif_get_list_length(env, argv[1], &numkeys), env);       
    keys = malloc(sizeof(char*) * numkeys);
    nkeys = malloc(sizeof(size_t) * numkeys);
    currKey = malloc(sizeof(ERL_NIF_TERM));
    tail = argv[1];
    unsigned int keylen;
    int i = 0;
    while(0 != enif_get_list_cell(env, tail, currKey, &tail)) {
        assert_badarg(enif_get_list_length(env, *currKey, &keylen), env);       
        nkeys[i] = keylen + 1;
        keys[i] = malloc(sizeof(char) * nkeys[i]);
        assert_badarg(enif_get_string(env, *currKey, keys[i], nkeys[i], ERL_NIF_LATIN1), env);           
        i++;
    }
    
    assert_badarg(enif_get_int(env, argv[2], &exp), env);       
    
    cb.currKey = 0;
    cb.ret = malloc(sizeof(struct libcouchbase_callback*) * numkeys);

    enif_mutex_lock(handle->mutex); 
    ret = libcouchbase_mget(handle->instance,
                             &cb,
                             numkeys,
                             (const void*const*)keys,
                             nkeys,
                             exp == 0 ? NULL : (libcouchbase_time_t*)&exp); 
    
    

    if (ret != LIBCOUCHBASE_SUCCESS) {
        enif_mutex_unlock(handle->mutex); 
        return return_lcb_error(env, ret);
    }
    libcouchbase_wait(handle->instance);
    enif_mutex_unlock(handle->mutex); 
    

    results = malloc(sizeof(ERL_NIF_TERM) * numkeys);
    i = 0; 
    for(; i < numkeys; i++) {
        if (cb.ret[i]->error == LIBCOUCHBASE_SUCCESS) {
            databin = malloc(sizeof(ErlNifBinary));
            enif_alloc_binary(cb.ret[i]->size, databin);
            memcpy(databin->data, cb.ret[i]->data, cb.ret[i]->size);
            results[i] = enif_make_tuple4(env, 
                    enif_make_int(env, cb.ret[i]->cas), 
                    enif_make_int(env, cb.ret[i]->flag), 
                    enif_make_string_len(env, cb.ret[i]->key, cb.ret[i]->nkey - 1, ERL_NIF_LATIN1),
                    enif_make_binary(env, databin));
            free(cb.ret[i]->data);
            free(databin);
        } else {
            results[i] = enif_make_tuple2(env, 
                    enif_make_string_len(env, cb.ret[i]->key, cb.ret[i]->nkey - 1, ERL_NIF_LATIN1),
                    return_lcb_error(env, cb.ret[i]->error));
        }
        free(cb.ret[i]->key);
        free(cb.ret[i]);
        free(keys[i]);
    }
    returnValue = enif_make_list_from_array(env, results, numkeys);
    
    free(results);
    free(cb.ret);
    free(currKey);
    free(keys);
    free(nkeys);
    
    return enif_make_tuple2(env, a_ok, returnValue);
}

NIF(cberl_nif_getl) {
    handle_t * handle;
    struct libcouchbase_callback cb; 
    void * key;
    unsigned int nkey;
    int exp;

    libcouchbase_error_t ret; 
    
    assert_badarg(enif_get_resource(env, argv[0], cberl_handle, (void **) &handle), env);      
    assert_badarg(enif_get_list_length(env, argv[1], &nkey), env);       
    nkey += 1;
    key = (char *) malloc(nkey);
    assert_badarg(enif_get_string(env, argv[1], key, nkey, ERL_NIF_LATIN1), env);           
    
    assert_badarg(enif_get_int(env, argv[3], &exp), env);       
    enif_mutex_lock(handle->mutex);
    ret = libcouchbase_getl(handle->instance,
                             &cb,
                             key,
                             nkey, 
                             exp == 0 ? NULL : (libcouchbase_time_t*)&exp); 
    free(key);
    if (ret != LIBCOUCHBASE_SUCCESS) {
        enif_mutex_unlock(handle->mutex); 
        return return_lcb_error(env, ret);
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
    struct libcouchbase_callback cb; 
    void * key;
    unsigned int nkey;
    int cas;

    libcouchbase_error_t ret; //for checking responses
    
    assert_badarg(enif_get_resource(env, argv[0], cberl_handle, (void **) &handle), env);      
    assert_badarg(enif_get_list_length(env, argv[1], &nkey), env);       
    nkey += 1;
    key = (char *) malloc(nkey);
    assert_badarg(enif_get_string(env, argv[1], key, nkey, ERL_NIF_LATIN1), env);           
    
    assert_badarg(enif_get_int(env, argv[2], &cas), env);       
    enif_mutex_lock(handle->mutex);
    ret = libcouchbase_unlock(handle->instance,
                             &cb,
                             key,
                             nkey,
                             cas); 
    free(key);

    if (ret != LIBCOUCHBASE_SUCCESS) {
        enif_mutex_unlock(handle->mutex); 
        return return_lcb_error(env, ret);
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
    struct libcouchbase_callback cb; 
    void * key;
    unsigned int nkey;
    int exp;

    libcouchbase_error_t ret; //for checking responses
    
    assert_badarg(enif_get_resource(env, argv[0], cberl_handle, (void **) &handle), env);      
    assert_badarg(enif_get_list_length(env, argv[1], &nkey), env);       
    nkey += 1;
    key = (char *) malloc(nkey);
    assert_badarg(enif_get_string(env, argv[1], key, nkey, ERL_NIF_LATIN1), env);       
    
    assert_badarg(enif_get_int(env, argv[2], &exp), env);        

    const char* keys[1];
    size_t nkeys[1];
    keys[0] = key;
    nkeys[0] = nkey;
    enif_mutex_lock(handle->mutex);
    ret = libcouchbase_mtouch(handle->instance,
                             &cb,
                             1,
                             (const void*const*)keys,
                             nkeys,
                             (libcouchbase_time_t*)&exp); 
    free(key);
    if (ret != LIBCOUCHBASE_SUCCESS) {
        enif_mutex_unlock(handle->mutex); 
        return return_lcb_error(env, ret);
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
    struct libcouchbase_callback cb; 
    void * key;
    unsigned int nkey;
    int64_t delta;
    uint64_t exp;
    int create;
    uint64_t initial; 

    libcouchbase_error_t ret; //for checking responses
    
    assert_badarg(enif_get_resource(env, argv[0], cberl_handle, (void **) &handle), env);      
    assert_badarg(enif_get_list_length(env, argv[1], &nkey), env);       
    nkey += 1;
    key = (char *) malloc(nkey);
    assert_badarg(enif_get_string(env, argv[1], key, nkey, ERL_NIF_LATIN1), env);       
    assert_badarg(enif_get_int64(env, argv[2], (long*)&delta), env);        
    assert_badarg(enif_get_uint64(env, argv[3], (unsigned long *)&exp), env);       
    assert_badarg(enif_get_int(env, argv[4], &create), env);   
    assert_badarg(enif_get_uint64(env, argv[5], (unsigned long *)&initial), env);   
    enif_mutex_lock(handle->mutex);
    ret = libcouchbase_arithmetic(handle->instance,
                                    &cb,
                                    key,
                                    nkey,
                                    delta,
                                    exp, 
                                    create,
                                    initial); 
    free(key);
    if (ret != LIBCOUCHBASE_SUCCESS) {
        enif_mutex_unlock(handle->mutex); 
        return return_lcb_error(env, ret);
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
    struct libcouchbase_callback cb; 
    void * key;
    unsigned int nkey;
    int cas;

    libcouchbase_error_t ret; //for checking responses
    
    assert_badarg(enif_get_resource(env, argv[0], cberl_handle, (void **) &handle), env);      
    assert_badarg(enif_get_list_length(env, argv[1], &nkey), env);       
    nkey += 1;
    key = (char *) malloc(nkey);
    assert_badarg(enif_get_string(env, argv[1], key, nkey, ERL_NIF_LATIN1), env);           
    
    assert_badarg(enif_get_int(env, argv[2], &cas), env);       
    enif_mutex_lock(handle->mutex); 
    ret = libcouchbase_remove(handle->instance,
                             &cb,
                             key,
                             nkey,
                             cas); 
    free(key);
    if (ret != LIBCOUCHBASE_SUCCESS) {
        enif_mutex_unlock(handle->mutex); 
        return return_lcb_error(env, ret);
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
    assert_badarg(enif_get_resource(env, argv[0], cberl_handle, (void **) &handle), env);      
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
    ERL_NIF_TERM term;
    enif_alloc_binary(cb->size, &value_binary);
    memcpy(value_binary.data, cb->data, cb->size);
    term  =   enif_make_tuple3(env, enif_make_int(env, cb->cas),
                                           enif_make_int(env, cb->flag), 
                                           enif_make_binary(env, &value_binary));
    free(cb->data);
    return term;
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
