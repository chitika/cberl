#include <stdio.h>
#include <string.h>
#include <libcouchbase/couchbase.h>
#include "erl_nif.h"
#include "callbacks.h"

void error_callback(lcb_t instance,
                    lcb_error_t error,
                    const char *errinfo)
{
    (void)instance;
    // do nothing
}

void get_callback(lcb_t instance,
                  const void *cookie,
                  lcb_error_t error,
                  const lcb_get_resp_t *item)
{
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

void arithmetic_callback(lcb_t instance,
                         const void *cookie,
                         lcb_error_t error,
                         const lcb_arithmetic_resp_t *resp)
{
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
    cb->flag = 1;
    if (error == LCB_SUCCESS) {
        cb->cas = resp->v.v0.cas;
        cb->data = malloc(20*sizeof(char));
        memset(cb->data, 0, 20);
        sprintf(cb->data, "%llu", resp->v.v0.value);
        cb->size = strlen(cb->data); 
    }
}

void unlock_callback(lcb_t instance,
                     const void *cookie,
                     lcb_error_t error,
                     const lcb_unlock_resp_t *resp)
{
    (void)instance;
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
}

void touch_callback(lcb_t instance,
                    const void *cookie,
                    lcb_error_t error,
                    const lcb_touch_resp_t *resp)
{
    (void)instance;
    struct libcouchbase_callback_m *cbm;
    cbm = (struct libcouchbase_callback_m *)cookie;
    cbm->ret[cbm->currKey] = malloc(sizeof(struct libcouchbase_callback));
    cbm->ret[cbm->currKey]->key = malloc(resp->v.v0.nkey);
    memcpy(cbm->ret[cbm->currKey]->key, resp->v.v0.key, resp->v.v0.nkey);
    cbm->ret[cbm->currKey]->nkey = resp->v.v0.nkey;
    cbm->ret[cbm->currKey]->error = error;
    cbm->currKey += 1;
}

void store_callback(lcb_t instance,
                    const void *cookie,
                    lcb_storage_t operation,
                    lcb_error_t error,
                    const lcb_store_resp_t *item)
{
  
    (void)instance; (void)operation;
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
    cb->cas = item->v.v0.cas;
}

void remove_callback(lcb_t instance,
                     const void *cookie,
                     lcb_error_t error,
                     const lcb_remove_resp_t *resp)
{
    (void)instance;
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
}

void http_callback(lcb_http_request_t request,
                   lcb_t instance,
                   const void* cookie,
                   lcb_error_t error,
                   const lcb_http_resp_t *resp)
{
    (void)instance;
    struct libcouchbase_callback *cb = (struct libcouchbase_callback*)cookie;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
    if(error == LCB_SUCCESS) {
        cb->data = malloc(resp->v.v0.nbytes);
        cb->size = resp->v.v0.nbytes;
        memcpy(cb->data, resp->v.v0.bytes, resp->v.v0.nbytes);
    }
}
