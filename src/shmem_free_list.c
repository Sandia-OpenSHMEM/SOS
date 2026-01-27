/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2017 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include "config.h"

#include <stddef.h>
#include <stdlib.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_free_list.h"

#define NUM_ELEMENTS	2

shmem_free_list_t*
shmem_free_list_init(size_t element_size,
                     shmem_free_list_item_init_fn_t init_fn,
		     size_t max_pool_cnt)
{
    int ret;
    shmem_free_list_t *fl = (shmem_free_list_t*) calloc(1, sizeof(shmem_free_list_t));
    if (NULL == fl) return NULL;

    fl->element_size = element_size;
    fl->init_fn = init_fn;
    fl->nalloc = 0;
    fl->alloc_size = sizeof(shmem_free_list_alloc_t) + NUM_ELEMENTS * fl->element_size;
    fl->pool_size = 0;
    fl->pool_ofs = 0;
    fl->pool = NULL;
    if (max_pool_cnt) {
        /* preallocate pool with shmem malloc */
        /* memory must be reserved as a memory pool to prevent address conflicts between PEs */
        fl->pool_size = fl->alloc_size * max_pool_cnt;
        fl->pool = shmem_internal_shmalloc(fl->pool_size);
    }
    /* if pool count is zero allocate with bounce buffers with malloc */

    SHMEM_MUTEX_INIT(fl->lock);
    ret = shmem_free_list_more(fl);
    if (0 != ret) {
        free(fl);
        return NULL;
    }

    return fl;
}


void
shmem_free_list_destroy(shmem_free_list_t *fl)
{
    if (fl->pool) {
        /* allocated with shmem malloc */
        shmem_internal_free(fl->pool);
        fl->pool = NULL;
        fl->pool_ofs = 0;
    } else {
        /* allocated with malloc */
        shmem_free_list_alloc_t *alloc, *next;
        alloc = fl->allocs;
        while (NULL != alloc) {
            next = alloc->next;
            free(alloc);
            alloc = next;
        }
    }

    SHMEM_MUTEX_DESTROY(fl->lock);
}


int
shmem_free_list_more(shmem_free_list_t *fl)
{
    shmem_free_list_item_t *item, *first, *next, *last = NULL;
    shmem_free_list_alloc_t *header;
    char *buf;
    uint64_t i;

    if (fl->pool) {
       if (fl->pool_ofs >= fl->pool_size) {
           fprintf(stderr, "[%d] pool memory exhausted\n", shmem_internal_my_pe);
           return 1;
       }   

       buf = &fl->pool[fl->pool_ofs];
       fl->pool_ofs += fl->alloc_size;

       if (shmem_internal_params.BOUNCE_MLOCK) {
           mlock(buf, sizeof(shmem_free_list_alloc_t) + NUM_ELEMENTS * fl->element_size);
       }
    } else {
        buf = malloc(sizeof(shmem_free_list_alloc_t) +
                     NUM_ELEMENTS * fl->element_size);
        if (NULL == buf) return 1;
    }

    header = (shmem_free_list_alloc_t*) buf;
    first = item = (shmem_free_list_item_t*) (header + 1);
    for (i = 0 ; i < NUM_ELEMENTS ; ++i) {
        fl->init_fn(item);
        next = (shmem_free_list_item_t*)((char*)item + fl->element_size);
        if (i == NUM_ELEMENTS - 1) {
            item->next = NULL;
            last = item;
        } else {
            item->next = next;
        }
        item = next;
    }

    header->next = fl->allocs;
    fl->allocs = header;

    if (NULL != last) last->next = fl->head;
    fl->head = first;

    return 0;
}
