#include "erl_nif.h"
#include "queue.h"
#include <stdio.h>

queue_t* queue_new(void)
{
    queue_t* queue = (queue_t*)enif_alloc(sizeof(queue_t));
    queue->head = NULL;
    queue->tail = NULL;
    queue->mutex = enif_mutex_create("queue_mutex");
    queue->cond = enif_cond_create("queue_cond");
    return queue;
}

void queue_put(queue_t* queue, void *data)
{
    queue_item_t* item = (queue_item_t*)enif_alloc(sizeof(queue_item_t));
    item->next = NULL;
    item->data = data;

    enif_mutex_lock(queue->mutex);

    if (queue->tail != NULL) {
        queue->tail->next = item;
    }

    queue->tail = item;

    if (queue->head == NULL) {
        queue->head = queue->tail;
    }

    enif_cond_signal(queue->cond);
    enif_mutex_unlock(queue->mutex);
}

void* queue_get(queue_t* queue)
{
    queue_item_t* item;

    enif_mutex_lock(queue->mutex);

    // Block until theres something in the queue
    while (queue->head == NULL) {
        enif_cond_wait(queue->cond, queue->mutex);
    }

    item = queue->head;
    queue->head = queue->head->next;
    item->next = NULL;

    if (queue->head == NULL) {
        queue->tail = NULL;
    }

    enif_mutex_unlock(queue->mutex);

    void* data = item->data;

    enif_free(item);

    return data;
}

void queue_destroy(queue_t* queue)
{
    enif_mutex_destroy(queue->mutex);
    enif_cond_destroy(queue->cond);
    enif_free(queue);
}