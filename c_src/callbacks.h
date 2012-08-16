#ifndef CALLBACKS_H
#define CALLBACKS_H

#include <stdio.h>
#include "config.h"
#include <string.h>
#include <libcouchbase/couchbase.h>


struct libcouchbase_callback {
    libcouchbase_error_t error;
    size_t size;
    void *data;
    int flag;
    int cas;
};

//libcouchbase callbacks
static void error_callback(libcouchbase_t instance,
                           libcouchbase_error_t error,
                           const char *errinfo)
{
    (void)instance;
    exit(EXIT_FAILURE);
}

static void get_callback(libcouchbase_t instance,
                         const void *cookie,
                         libcouchbase_error_t error,
                         const void *key,
                         libcouchbase_size_t nkey,
                         const void *bytes,
                         libcouchbase_size_t nbytes,
                         libcouchbase_uint32_t flags,
                         libcouchbase_cas_t cas)
{
    (void)key; (void)nkey; (void)flags; (void)cas;
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
    cb->flag = flags == 0 ? 1 : flags;
    cb->cas = cas;
    if (error == LIBCOUCHBASE_SUCCESS) {
        cb->data = malloc(nbytes);
        memcpy(cb->data, bytes, nbytes);
        cb->size = nbytes;
    }
}

static void arithmetic_callback(libcouchbase_t instance,
                                const void *cookie,
                                libcouchbase_error_t error,
                                const void *key,
                                libcouchbase_size_t nkey,
                                libcouchbase_uint64_t value,
                                libcouchbase_cas_t cas)
{
    (void)key; (void)nkey; (void)cas;
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
    cb->flag = 1;
    cb->cas = cas;
    if (error == LIBCOUCHBASE_SUCCESS) {
        cb->data = malloc(20*sizeof(char));
        memset(cb->data, 0, 20);
        sprintf(cb->data, "%lu", value);
        cb->size = strlen(cb->data); 
    }
}
static void unlock_callback(libcouchbase_t instance,
                             const void *cookie,
                             libcouchbase_error_t error,
                             const void *key, size_t nkey)
{
    (void)instance; (void)key; (void)nkey; 
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
}

static void touch_callback(libcouchbase_t instance,
                           const void *cookie,
                           libcouchbase_error_t error,
                           const void *key,
                           libcouchbase_size_t nkey)
{
    (void)instance; (void)key; (void)nkey; 
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
}

static void storage_callback(libcouchbase_t instance,
                             const void *cookie,
                             libcouchbase_storage_t operation,
                             libcouchbase_error_t error,
                             const void *key, size_t nkey,
                             uint64_t cas)
{
    (void)instance; (void)operation; (void)key;
    (void)nkey; (void)cas;
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
    cb->cas = cas;
}

static void remove_callback(libcouchbase_t instance,
                            const void *cookie,
                            libcouchbase_error_t error,
                            const void *key,
                            libcouchbase_size_t nkey)
{
    (void)instance; (void)key; (void)nkey; 
    struct libcouchbase_callback *cb;
    cb = (struct libcouchbase_callback *)cookie;
    cb->error = error;
}
#endif
