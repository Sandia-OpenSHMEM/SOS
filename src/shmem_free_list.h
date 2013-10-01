/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 * 
 * This file is part of the Portals SHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef SHMEM_FREE_QUEUE_H
#define SHMEM_FREE_QUEUE_H

#include <pthread.h>
#include <stdint.h>

#include "shmem_atomic.h"

struct shmem_free_list_item_t {
    struct shmem_free_list_item_t *next;
};
typedef struct shmem_free_list_item_t shmem_free_list_item_t;

struct shmem_free_list_alloc_t {
    struct shmem_free_list_alloc_t *next;
};
typedef struct shmem_free_list_alloc_t shmem_free_list_alloc_t;

typedef void (*shmem_free_list_item_init_fn_t)(shmem_free_list_item_t *item);

struct shmem_free_list_t {
    uint32_t element_size;

    shmem_free_list_item_init_fn_t init_fn;
    shmem_free_list_alloc_t *allocs;
    shmem_free_list_item_t* head;
    shmem_spinlock_t lock;
};
typedef struct shmem_free_list_t shmem_free_list_t;

shmem_free_list_t* shmem_free_list_init(unsigned int element_size, 
                                        shmem_free_list_item_init_fn_t init_fn);
void shmem_free_list_destroy(shmem_free_list_t *fl);
int shmem_free_list_more(shmem_free_list_t *fl);


static inline
void*
shmem_free_list_alloc(shmem_free_list_t *fl)
{
    shmem_free_list_item_t *item = NULL;
    int ret;

    shmem_spinlock_lock(&fl->lock);
    if (NULL == fl->head) {
        ret = shmem_free_list_more(fl);
        if (0 != ret) goto done;
    }
    assert(NULL != fl->head);

    item = fl->head;
    fl->head = item->next;

 done:
    shmem_spinlock_unlock(&fl->lock);

    return item;
}


static inline
void
shmem_free_list_free(shmem_free_list_t *fl, void *data)
{
    shmem_free_list_item_t *item = (shmem_free_list_item_t*) data;
    
    shmem_spinlock_lock(&fl->lock);
    item->next = fl->head;
    fl->head = item;
    shmem_spinlock_unlock(&fl->lock);
}

#endif
