/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2016 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef SHMEM_FREE_QUEUE_H
#define SHMEM_FREE_QUEUE_H

#include <stdint.h>

#include "shmem_internal.h"

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
#ifdef ENABLE_THREADS
    shmem_internal_mutex_t lock;
#endif
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

    SHMEM_MUTEX_LOCK(fl->lock);
    if (NULL == fl->head) {
        ret = shmem_free_list_more(fl);
        if (0 != ret) goto done;
    }
    shmem_internal_assert(NULL != fl->head);

    item = fl->head;
    fl->head = item->next;

 done:
    SHMEM_MUTEX_UNLOCK(fl->lock);

    return item;
}


static inline
void
shmem_free_list_free(shmem_free_list_t *fl, void *data)
{
    shmem_free_list_item_t *item = (shmem_free_list_item_t*) data;

    SHMEM_MUTEX_LOCK(fl->lock);
    item->next = fl->head;
    fl->head = item;
    SHMEM_MUTEX_UNLOCK(fl->lock);
}

#endif
