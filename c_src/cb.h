#ifndef CB_H
#define CB_H

#include "erl_nif.h"
#include "cberl.h"

typedef struct connect_args {
    char* host;
    char* user;
    char* pass;
    char* bucket;
} connect_args_t;

typedef struct store_args {
    int operation;
    unsigned int nkey;
    lcb_uint32_t flags;
    int exp;
    lcb_cas_t cas;
    void * key;
    void * bytes;
    lcb_size_t nbytes;
} store_args_t;

typedef struct mget_args {
    unsigned int numkeys;
    void** keys;
    size_t* nkeys;
    int exp;
    int lock;
} mget_args_t;

typedef struct unlock_args {
    void * key;
    unsigned int nkey;
    lcb_cas_t cas;
} unlock_args_t;

typedef struct mtouch_args {
    void** keys;
    size_t* nkeys;
    long *exp;
    unsigned int numkeys;
} mtouch_args_t;

typedef struct arithmetic_args {
    void * key;
    unsigned int nkey;
    int64_t delta;
    uint64_t exp;
    int create;
    uint64_t initial; 
} arithmetic_args_t;

typedef struct remove_args_t {
    void * key;
    unsigned int nkey;
    int cas;
} remove_args_t;

typedef struct http_args {
    char *path;
    char *body;
    lcb_http_method_t method;
    char *content_type;
    lcb_http_type_t type;
} http_args_t;

void* cb_connect_args(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM cb_connect(ErlNifEnv* env, handle_t* handle, void* obj);
void* cb_store_args(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM cb_store(ErlNifEnv* env, handle_t* handle, void* obj);
void* cb_mget_args(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM cb_mget(ErlNifEnv* env, handle_t* handle, void* obj);
void* cb_unlock_args(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM cb_unlock(ErlNifEnv* env, handle_t* handle, void* obj);
void* cb_mtouch_args(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM cb_mtouch(ErlNifEnv* env, handle_t* handle, void* obj);
void* cb_arithmetic_args(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM cb_arithmetic(ErlNifEnv* env, handle_t* handle, void* obj);
void* cb_remove_args(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM cb_remove(ErlNifEnv* env, handle_t* handle, void* obj);
void* cb_http_args(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM cb_http(ErlNifEnv* env, handle_t* handle, void* obj);

ERL_NIF_TERM return_lcb_error(ErlNifEnv* env, int const value);
ERL_NIF_TERM return_value(ErlNifEnv* env, void * cookie);

#endif
