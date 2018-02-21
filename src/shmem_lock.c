/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2018 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include <stdlib.h>
#include <pthread.h>
#include "shmem_internal.h"
#include "shmem_lock.h"
#include "uthash.h"

typedef struct {
    long           *key;
    pthread_mutex_t mutex;
    UT_hash_handle  hh;
} shmem_internal_lock_guard_t;

static shmem_internal_lock_guard_t *guards = NULL;

void shmem_internal_lock_guard_enter(long *lockp) {
    int ret;
    shmem_internal_lock_guard_t *g;

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_lock_guards);
    HASH_FIND_PTR(guards, &lockp, g);

    if (g == NULL) {
        DEBUG_MSG("Registering guard for lock %p\n", (void*) lockp);

        g = (shmem_internal_lock_guard_t*)
            malloc(sizeof(shmem_internal_lock_guard_t));
        if (g == NULL)
            RAISE_ERROR_STR("Out of memory allocating lock guard");

        g->key = lockp;
        ret = pthread_mutex_init(&g->mutex, NULL);
        if (ret) RAISE_ERROR_MSG("pthread_mutex_init failed: %s\n", strerror(ret));

        HASH_ADD_PTR(guards, key, g);
    }
    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_lock_guards);

    ret = pthread_mutex_lock(&g->mutex);
    if (ret) RAISE_ERROR_MSG("pthread_mutex_lock failed: %s\n", strerror(ret));
    /* FIXME: Use strerror_r */
}


void shmem_internal_lock_guard_exit(long *lockp) {
    shmem_internal_lock_guard_t *g;

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_lock_guards);
    HASH_FIND_PTR(guards, &lockp, g);
    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_lock_guards);

    if (g == NULL) {
        RAISE_ERROR_MSG("Attempted to clear a lock that is not held (%p)\n",
                        (void*) lockp);
    }

    pthread_mutex_unlock(&g->mutex);
}


void shmem_internal_lock_guards_free(void) {
    shmem_internal_lock_guard_t *g;

    for (g = guards; g != NULL ; ) {
        shmem_internal_lock_guard_t *nextg = g->hh.next;
        free(g);
        g = nextg;
    }
}
