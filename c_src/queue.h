#ifndef QUEUE_H
#define QUEUE_H

#include "erl_nif.h"

typedef struct queue_item {
    struct queue_item* next;
    void* data;
} queue_item_t;

typedef struct queue {
    queue_item_t *head;
    queue_item_t *tail;
    ErlNifMutex* mutex;
    ErlNifCond* cond;
} queue_t;

queue_t* queue_new(void);
void queue_put(queue_t* queue, void *data);
void* queue_get(queue_t* queue);
void queue_destroy(queue_t* queue);

#endif