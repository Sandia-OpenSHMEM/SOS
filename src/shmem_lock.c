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
#include <stdint.h>
#include "shmem_internal.h"
#include "shmem_lock.h"
#include "uthash.h"

typedef struct {
    UT_hash_handle  hh;
    long           *key;
    uint64_t        next_ticket;
    uint64_t        cur_ticket;
    pthread_mutex_t mutex;
    pthread_cond_t  cond[];
} shmem_internal_lock_guard_t;

static shmem_internal_lock_guard_t *guards = NULL;

/* Simple queueing lock using Lamport's bakery algorithm.  Uses LOCK_QUEUE_SIZE
 * condition variables per lock to reduce the number of threads that are woken
 * up when the lock is released. */
static inline void shmem_internal_qlock_lock(shmem_internal_lock_guard_t *g) {
    int ret;
    char errmsg[256];
    uint64_t my_ticket;

    ret = pthread_mutex_lock(&g->mutex);
    if (ret) RAISE_ERROR_MSG("pthread_mutex_lock failed: %s\n",
                             shmem_util_strerror(ret, errmsg, 256));

    my_ticket = g->next_ticket++;
    while (my_ticket != g->cur_ticket) {
        ret = pthread_cond_wait(&g->cond[my_ticket %
                                shmem_internal_params.LOCK_QUEUE_SIZE],
                                &g->mutex);
        if (ret) RAISE_ERROR_MSG("pthread_cond_wait failed: %s\n",
                                 shmem_util_strerror(ret, errmsg, 256));
    }
    ret = pthread_mutex_unlock(&g->mutex);
    if (ret) RAISE_ERROR_MSG("pthread_mutex_unlock failed: %s\n",
                             shmem_util_strerror(ret, errmsg, 256));
}

static inline void shmem_internal_qlock_unlock(shmem_internal_lock_guard_t *g) {
    int ret;
    char errmsg[256];

    ret = pthread_mutex_lock(&g->mutex);
    if (ret) RAISE_ERROR_MSG("pthread_mutex_lock failed: %s\n",
                             shmem_util_strerror(ret, errmsg, 256));

    g->cur_ticket++;
    ret = pthread_cond_broadcast(&g->cond[g->cur_ticket %
                                 shmem_internal_params.LOCK_QUEUE_SIZE]);
    if (ret) RAISE_ERROR_MSG("pthread_cond_broadcast failed: %s\n",
                             shmem_util_strerror(ret, errmsg, 256));
    ret = pthread_mutex_unlock(&g->mutex);
    if (ret) RAISE_ERROR_MSG("pthread_mutex_unlock failed: %s\n",
                             shmem_util_strerror(ret, errmsg, 256));
}

static inline int shmem_internal_qlock_trylock(shmem_internal_lock_guard_t *g) {
    int ret;
    char errmsg[256];

    ret = pthread_mutex_lock(&g->mutex);
    if (ret) RAISE_ERROR_MSG("pthread_mutex_lock failed: %s\n",
                             shmem_util_strerror(ret, errmsg, 256));

    if (g->next_ticket == g->cur_ticket) {
        g->next_ticket++;
        ret = pthread_mutex_unlock(&g->mutex);
        if (ret) RAISE_ERROR_MSG("pthread_mutex_unlock failed: %s\n",
                                 shmem_util_strerror(ret, errmsg, 256));
        return 0;
    }
    else {
        ret = pthread_mutex_unlock(&g->mutex);
        if (ret) RAISE_ERROR_MSG("pthread_mutex_unlock failed: %s\n",
                                 shmem_util_strerror(ret, errmsg, 256));
        return 1;
    }
}

static shmem_internal_lock_guard_t*
shmem_internal_lock_guard_locate(long *lockp) {
    int ret, i;
    char errmsg[256];
    shmem_internal_lock_guard_t *g;

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_lock_guards);
    HASH_FIND_PTR(guards, &lockp, g);

    if (g == NULL) {
        DEBUG_MSG("Registering guard for lock %p\n", (void*) lockp);

        g = (shmem_internal_lock_guard_t*)
            malloc(sizeof(shmem_internal_lock_guard_t) + sizeof(pthread_cond_t)
                   * shmem_internal_params.LOCK_QUEUE_SIZE);
        if (g == NULL)
            RAISE_ERROR_STR("Out of memory allocating lock guard");

        g->key = lockp;
        g->next_ticket = 0;
        g->cur_ticket = 0;
        ret = pthread_mutex_init(&g->mutex, NULL);
        if (ret) RAISE_ERROR_MSG("pthread_mutex_init failed: %s\n",
                                 shmem_util_strerror(ret, errmsg, 256));
        for (i = 0; i < shmem_internal_params.LOCK_QUEUE_SIZE ; i++) {
            ret = pthread_cond_init(&g->cond[i], NULL);
            if (ret) RAISE_ERROR_MSG("pthread_cond_init failed: %s\n",
                                     shmem_util_strerror(ret, errmsg, 256));
        }

        HASH_ADD_PTR(guards, key, g);
    }
    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_lock_guards);
    return g;
}


void shmem_internal_lock_guard_enter(long *lockp) {
    shmem_internal_lock_guard_t *g = shmem_internal_lock_guard_locate(lockp);
    shmem_internal_qlock_lock(g);
}


int shmem_internal_lock_guard_test_enter(long *lockp) {
    shmem_internal_lock_guard_t *g = shmem_internal_lock_guard_locate(lockp);
    return shmem_internal_qlock_trylock(g);
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

    shmem_internal_qlock_unlock(g);
}


void shmem_internal_lock_guards_free(void) {
    int ret, i;
    char errmsg[256];
    shmem_internal_lock_guard_t *g;

    for (g = guards; g != NULL ; ) {
        shmem_internal_lock_guard_t *nextg = g->hh.next;
        ret = pthread_mutex_destroy(&g->mutex);
        if (ret) RAISE_ERROR_MSG("pthread_mutex_destroy failed: %s\n",
                                 shmem_util_strerror(ret, errmsg, 256));
        for (i = 0; i < shmem_internal_params.LOCK_QUEUE_SIZE; i++) {
            ret = pthread_cond_destroy(&g->cond[i]);
            if (ret) RAISE_ERROR_MSG("pthread_cond_destroy failed: %s\n",
                                     shmem_util_strerror(ret, errmsg, 256));
        }
        free(g);
        g = nextg;
    }
}
