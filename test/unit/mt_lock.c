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

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdint.h>
#include "mt_lock.h"
#include "uthash.h"

#define MTL_QUEUE_SIZE 4
#define DEBUG 0

typedef struct {
    UT_hash_handle  hh;
    long           *key;
    uint64_t        next_ticket;
    uint64_t        cur_ticket;
    pthread_mutex_t mutex;
    pthread_cond_t  cond[];
} mtl_guard_t;

static mtl_guard_t *guards = NULL;
pthread_mutex_t guards_mutex = PTHREAD_MUTEX_INITIALIZER;

#define RAISE_ERROR(err, msg)                   \
    do {                                        \
        char errmsg[256];                       \
        strerror_r(err, errmsg, 256);           \
        fprintf(stderr, "%d: %s - %s\n",        \
                shmem_my_pe(), msg, errmsg);    \
        shmem_global_exit(1);                   \
    } while(0)


/* Simple queueing lock using Lamport's bakery algorithm.  Uses MTL_QUEUE_SIZE
 * condition variables per lock to reduce the number of threads that are woken
 * up when the lock is released. */

static inline void mtl_qlock_lock(mtl_guard_t *g) {
    int ret;
    uint64_t my_ticket;

    ret = pthread_mutex_lock(&g->mutex);
    if (ret) RAISE_ERROR(ret, "pthread_mutex_lock failed");

    my_ticket = g->next_ticket++;
    while (my_ticket != g->cur_ticket) {
        ret = pthread_cond_wait(&g->cond[my_ticket % MTL_QUEUE_SIZE],
                                &g->mutex);
        if (ret) RAISE_ERROR(ret, "pthread_cond_wait failed");
    }
    ret = pthread_mutex_unlock(&g->mutex);
    if (ret) RAISE_ERROR(ret, "pthread_mutex_unlock failed");
}


static inline void mtl_qlock_unlock(mtl_guard_t *g) {
    int ret;

    ret = pthread_mutex_lock(&g->mutex);
    if (ret) RAISE_ERROR(ret, "pthread_mutex_lock failed");

    g->cur_ticket++;
    ret = pthread_cond_broadcast(&g->cond[g->cur_ticket % MTL_QUEUE_SIZE]);
    if (ret) RAISE_ERROR(ret, "pthread_cond_broadcast failed");
    ret = pthread_mutex_unlock(&g->mutex);
    if (ret) RAISE_ERROR(ret, "pthread_mutex_unlock failed");
}


static inline int mtl_qlock_trylock(mtl_guard_t *g) {
    int ret;

    ret = pthread_mutex_lock(&g->mutex);
    if (ret) RAISE_ERROR(ret, "pthread_mutex_lock failed");

    if (g->next_ticket == g->cur_ticket) {
        g->next_ticket++;
        ret = pthread_mutex_unlock(&g->mutex);
        if (ret) RAISE_ERROR(ret, "pthread_mutex_unlock failed");
        return 0;
    }
    else {
        ret = pthread_mutex_unlock(&g->mutex);
        if (ret) RAISE_ERROR(ret, "pthread_mutex_unlock failed");
        return 1;
    }
}

/** Guard Implementation **/

static mtl_guard_t* mtl_guard_locate(long *lockp) {
    int ret, i;
    mtl_guard_t *g;

    pthread_mutex_lock(&guards_mutex);
    HASH_FIND_PTR(guards, &lockp, g);

    if (g == NULL) {
        if (DEBUG)
            printf("%d: Registering guard for lock %p\n", shmem_my_pe(),
                   (void*) lockp);

        g = (mtl_guard_t*)
            malloc(sizeof(mtl_guard_t) + sizeof(pthread_cond_t)
                   * MTL_QUEUE_SIZE);
        if (g == NULL) {
            fprintf(stderr, "%d: Out of memory allocating lock guard\n",
                    shmem_my_pe());
            shmem_global_exit(2);
        }

        g->key = lockp;
        g->next_ticket = 0;
        g->cur_ticket = 0;
        ret = pthread_mutex_init(&g->mutex, NULL);
        if (ret) RAISE_ERROR(ret, "pthread_mutex_init failed");
        for (i = 0; i < MTL_QUEUE_SIZE ; i++) {
            ret = pthread_cond_init(&g->cond[i], NULL);
            if (ret) RAISE_ERROR(ret, "pthread_cond_init failed");
        }

        HASH_ADD_PTR(guards, key, g);
    }
    pthread_mutex_unlock(&guards_mutex);
    return g;
}


static void mtl_guard_enter(long *lockp) {
    mtl_guard_t *g = mtl_guard_locate(lockp);
    mtl_qlock_lock(g);
}


static int mtl_guard_test_enter(long *lockp) {
    mtl_guard_t *g = mtl_guard_locate(lockp);
    return mtl_qlock_trylock(g);
}


static void mtl_guard_exit(long *lockp) {
    mtl_guard_t *g;

    pthread_mutex_lock(&guards_mutex);
    HASH_FIND_PTR(guards, &lockp, g);
    pthread_mutex_unlock(&guards_mutex);

    if (g == NULL) {
        fprintf(stderr, "%d: Attempted to clear a lock that is not held (%p)\n",
                        shmem_my_pe(), (void*) lockp);
        shmem_global_exit(3);
    }

    mtl_qlock_unlock(g);
}

/** User-facing API **/

void mtl_cleanup(void) {
    int ret, i;
    mtl_guard_t *g;

    for (g = guards; g != NULL ; ) {
        mtl_guard_t *nextg = g->hh.next;
        ret = pthread_mutex_destroy(&g->mutex);
        if (ret) RAISE_ERROR(ret, "pthread_mutex_destroy failed");
        for (i = 0; i < MTL_QUEUE_SIZE; i++) {
            ret = pthread_cond_destroy(&g->cond[i]);
            if (ret) RAISE_ERROR(ret, "pthread_cond_destroy failed");
        }
        free(g);
        g = nextg;
    }
}


void mtl_clear_lock(long *lockp) {
    int prov;

    shmem_clear_lock(lockp);
    shmem_query_thread(&prov);

    if (prov == SHMEM_THREAD_SERIALIZED || prov == SHMEM_THREAD_MULTIPLE)
        mtl_guard_exit(lockp);
}


void mtl_set_lock(long *lockp) {
    int prov;

    /* Guards use a local mutex to prevent concurrent requests on the same
     * lock.  The guard is "entered" when the SHMEM lock is acquired and
     * "exited" when the SHMEM lock is released. */
    shmem_query_thread(&prov);

    if (prov == SHMEM_THREAD_SERIALIZED || prov == SHMEM_THREAD_MULTIPLE)
        mtl_guard_enter(lockp);

    shmem_set_lock(lockp);
}


int mtl_test_lock(long *lockp) {
    int status, prov;

    shmem_query_thread(&prov);

    if (prov == SHMEM_THREAD_SERIALIZED || prov == SHMEM_THREAD_MULTIPLE) {
        if (mtl_guard_test_enter(lockp))
            return 1;
    }

    status = shmem_test_lock(lockp);
    if (status == 0)
        return status;

    if (prov == SHMEM_THREAD_SERIALIZED || prov == SHMEM_THREAD_MULTIPLE)
        mtl_guard_exit(lockp);

    return 1;
}
