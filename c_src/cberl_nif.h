#ifndef CBERL_NIF_H
#define CBERL_NIF_H

#include "erl_nif.h"
#include "cberl.h"

// Command enum
#define CMD_CONNECT     0
#define CMD_STORE       1
#define CMD_MGET        2
#define CMD_GETL        3
#define CMD_UNLOCK      4
#define CMD_MTOUCH      5
#define CMD_ARITHMETIC  6
#define CMD_REMOVE      7
#define CMD_HTTP        8 

typedef struct task {
    ErlNifPid* pid;
    unsigned int cmd;
    void *args;
} task_t;

static void* worker(void *obj);

#endif
