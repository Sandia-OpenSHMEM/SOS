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

#include "config.h"

#include <stdlib.h>

#include "shmem.h"
#include "shmem_free_list.h"


shmem_free_list_t*
shmem_free_list_init(unsigned int element_size,
                     shmem_free_list_item_init_fn_t init_fn)
{
    int ret;
    shmem_free_list_t *fl = (shmem_free_list_t*) calloc(1, sizeof(shmem_free_list_t));
    if (NULL == fl) return NULL;

    fl->element_size = element_size;
    fl->init_fn = init_fn;
    shmem_spinlock_init(&fl->lock);
    ret = shmem_free_list_more(fl);
    if (0 != ret) return NULL;

    return fl;
}


void
shmem_free_list_destroy(shmem_free_list_t *fl)
{
    shmem_free_list_alloc_t *alloc, *next;

    alloc = fl->allocs;
    while (NULL != alloc) {
        next = alloc->next;
        free(alloc);
        alloc = next;
    }

    shmem_spinlock_fini(&fl->lock);
}


int
shmem_free_list_more(shmem_free_list_t *fl)
{
    int page_size = 4096 - sizeof(shmem_free_list_alloc_t);
    int num_elements = (fl->element_size < page_size) ? page_size / fl->element_size : 1;
    shmem_free_list_item_t *item, *first, *next, *last = NULL;
    shmem_free_list_alloc_t *header;
    char *buf;
    int i;

    num_elements = 2;

    buf = malloc(sizeof(shmem_free_list_alloc_t) + 
                 num_elements * fl->element_size);
    if (NULL == buf) return 1;

    header = (shmem_free_list_alloc_t*) buf;
    first = item = (shmem_free_list_item_t*) (header + 1);
    for (i = 0 ; i < num_elements ; ++i) {
        fl->init_fn(item);
        next = (shmem_free_list_item_t*)((char*)item + fl->element_size);
        if (i == num_elements - 1) {
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
