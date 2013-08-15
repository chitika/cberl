#ifndef CBERL_NIF_H
#define CBERL_NIF_H

#include "erl_nif.h"
#include "cberl.h"

typedef struct task {
    ErlNifPid* pid;
    unsigned int cmd;
    void *args;
} task_t;

static void* worker(void *obj);

#endif
