#ifndef CALLBACKS_H
#define CALLBACKS_H

struct libcouchbase_callback {
    lcb_error_t error;
    size_t size;
    void *data;
    void *key;
    size_t nkey;
    lcb_uint32_t flag;
    lcb_cas_t cas;
};

struct libcouchbase_callback_m {
    int currKey;
    struct libcouchbase_callback** ret;
};

void error_callback(lcb_t instance,
                    lcb_error_t error,
                    const char *errinfo);

void get_callback(lcb_t instance,
                  const void *cookie,
                  lcb_error_t error,
                  const lcb_get_resp_t *item);

void arithmetic_callback(lcb_t instance,
                         const void *cookie,
                         lcb_error_t error,
                         const lcb_arithmetic_resp_t *resp);

void unlock_callback(lcb_t instance,
                     const void *cookie,
                     lcb_error_t error,
                     const lcb_unlock_resp_t *resp);

void touch_callback(lcb_t instance,
                    const void *cookie,
                    lcb_error_t error,
                    const lcb_touch_resp_t *resp);

void store_callback(lcb_t instance,
                    const void *cookie,
                    lcb_storage_t operation,
                    lcb_error_t error,
                    const lcb_store_resp_t *item);

void remove_callback(lcb_t instance,
                     const void *cookie,
                     lcb_error_t error,
                     const lcb_remove_resp_t *resp);

void http_callback(lcb_http_request_t request,
                   lcb_t instance,
                   const void* cookie,
                   lcb_error_t error,
                   const lcb_http_resp_t *resp);

#endif
