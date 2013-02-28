#ifndef CALLBACKS_H
#define CALLBACKS_H

#include <stdio.h>
#include <string.h>

struct libcouchbase_callback {
    lcb_error_t error;
    size_t size;
    void *data;
    void *key;
    size_t nkey;
    int flag;
    int cas;
};

struct libcouchbase_callback_m {
    int currKey;
    struct libcouchbase_callback** ret;
};
//libcouchbase callbacks
static void error_callback(lcb_t instance,
                           lcb_error_t error,
                           const char *errinfo)
{
    (void)instance;
    fprintf(stderr, "Fatal error\r\n");
    exit(EXIT_FAILURE);
}

static void get_callback(lcb_t instance,
                         const void *cookie,
                         lcb_error_t error,
             const lcb_get_resp_t *item
                         /*const void *key,
                         lcb_size_t nkey,
                         const void *bytes,
                         lcb_size_t nbytes,
                         lcb_uint32_t flags,
                         lcb_cas_t cas*/)
{
    //(void)key; (void)nkey; (void)flags; (void)cas;
    struct libcouchbase_callback_m *cbm;
    cbm = (struct libcouchbase_callback_m *)cookie;
    cbm->ret[cbm->currKey] = malloc(sizeof(struct libcouchbase_callback));
    cbm->ret[cbm->currKey]->key = malloc(item->v.v0.nkey);
    memcpy(cbm->ret[cbm->currKey]->key, item->v.v0.key, item->v.v0.nkey);
    cbm->ret[cbm->currKey]->nkey = item->v.v0.nkey;
    cbm->ret[cbm->currKey]->error = error;
    cbm->ret[cbm->currKey]->flag = item->v.v0.flags == 0 ? 1 : item->v.v0.flags;
    cbm->ret[cbm->currKey]->cas = item->v.v0.cas;
    if (error == LCB_SUCCESS) {
        cbm->ret[cbm->currKey]->data = malloc(item->v.v0.nbytes);
        memcpy(cbm->ret[cbm->currKey]->data, item->v.v0.bytes, item->v.v0.nbytes);
        cbm->ret[cbm->currKey]->size = item->v.v0.nbytes;        
    }
    cbm->currKey += 1;
}

static void arithmetic_callback(lcb_t instance,
                                const void *cookie,
                                lcb_error_t error,
                const lcb_arithmetic_resp_t *resp
                                /*const void *key,
                                lcb_size_t nkey,
                                lcb_uint64_t value,
                                lcb_cas_t cas*/)
{
  //(void)key; (void)nkey; (void)cas;
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
    cb->flag = 1;
    cb->cas = resp->v.v0.cas;
    if (error == LCB_SUCCESS) {
        cb->data = malloc(20*sizeof(char));
        memset(cb->data, 0, 20);
        sprintf(cb->data, "%llu", resp->v.v0.value);
        cb->size = strlen(cb->data); 
    }
}
static void unlock_callback(lcb_t instance,
                             const void *cookie,
                             lcb_error_t error,
                /*const void *key, size_t nkey*/
                const lcb_unlock_resp_t *resp)
{
  (void)instance; //(void)key; (void)nkey; 
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
}

static void touch_callback(lcb_t instance,
                           const void *cookie,
                           lcb_error_t error,
               const lcb_touch_resp_t *resp
                           /*const void *key,
                           lcb_size_t nkey*/)
{
  (void)instance; //(void)key; (void)nkey; 
    struct libcouchbase_callback_m *cbm;
    cbm = (struct libcouchbase_callback_m *)cookie;
    cbm->ret[cbm->currKey] = malloc(sizeof(struct libcouchbase_callback));
    cbm->ret[cbm->currKey]->key = malloc(resp->v.v0.nkey);
    memcpy(cbm->ret[cbm->currKey]->key, resp->v.v0.key, resp->v.v0.nkey);
    cbm->ret[cbm->currKey]->nkey = resp->v.v0.nkey;
    cbm->ret[cbm->currKey]->error = error;
    cbm->currKey += 1;
}

static void store_callback(lcb_t instance,
               const void *cookie,
               lcb_storage_t operation,
               lcb_error_t error,
               const lcb_store_resp_t *item
               /*const void *key, size_t nkey,
                             uint64_t cas*/)
{
  
    (void)instance; (void)operation;
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
    cb->cas = item->v.v0.cas;
}

static void remove_callback(lcb_t instance,
                            const void *cookie,
                            lcb_error_t error,
                const lcb_remove_resp_t *resp
                            /*const void *key,
                  lcb_size_t nkey*/)
{
  (void)instance; //(void)key; (void)nkey; 
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
}
#endif
