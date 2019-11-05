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

#ifndef SHMEM_SYNCHRONIZATION_H
#define SHMEM_SYNCHRONIZATION_H

#include "shmem_atomic.h"
#include "shmem_comm.h"
#include "transport.h"

static inline void
shmem_internal_quiet(shmem_ctx_t ctx)
{
    int ret;

    if (ctx == SHMEMX_CTX_INVALID)
        return;

    ret = shmem_transport_quiet((shmem_transport_ctx_t *)ctx);
    if (0 != ret) { RAISE_ERROR(ret); }

    shmem_internal_membar();

    /* Transport level memory flush is required to make memory 
     * changes (i.e. subsequent coherent load operations 
     * performed via the shmem_ptr API, the result of atomics 
     * that targeted the local process) visible */
    shmem_transport_syncmem();
}


static inline void
shmem_internal_fence(shmem_ctx_t ctx)
{
    int ret;

    if (ctx == SHMEMX_CTX_INVALID)
        return;

    ret = shmem_transport_fence((shmem_transport_ctx_t *)ctx);
    if (0 != ret) { RAISE_ERROR(ret); }

    shmem_internal_membar_release();

    /* Since fence does not guarantee any memory visibility, 
     * transport level memory flush is not required here. */
}

#define COMP(type, a, b, ret)                            \
    do {                                                 \
        ret = 0;                                         \
        switch (type) {                                  \
        case SHMEM_CMP_EQ:                               \
            if (a == b) ret = 1;                         \
            break;                                       \
        case SHMEM_CMP_NE:                               \
            if (a != b) ret = 1;                         \
            break;                                       \
        case SHMEM_CMP_GT:                               \
            if (a > b) ret = 1;                          \
            break;                                       \
        case SHMEM_CMP_GE:                               \
            if (a >= b) ret = 1;                         \
            break;                                       \
        case SHMEM_CMP_LT:                               \
            if (a < b) ret = 1;                          \
            break;                                       \
        case SHMEM_CMP_LE:                               \
            if (a <= b) ret = 1;                         \
            break;                                       \
        default:                                         \
            RAISE_ERROR(-1);                             \
        }                                                \
    } while(0)


#define SHMEM_WAIT_POLL(var, value)                      \
    do {                                                 \
        while (*(var) == value) {                        \
            shmem_transport_probe();                     \
            SPINLOCK_BODY(); }                           \
    } while(0)

#define SHMEM_WAIT_UNTIL_POLL(var, cond, value)          \
    do {                                                 \
        int cmpret;                                      \
                                                         \
        COMP(cond, *(var), value, cmpret);               \
        while (!cmpret) {                                \
            shmem_transport_probe();                     \
            SPINLOCK_BODY();                             \
            COMP(cond, *(var), value, cmpret);           \
        }                                                \
    } while(0)

#define SHMEM_WAIT_BLOCK(var, value)                                    \
    do {                                                                \
        uint64_t target_cntr;                                           \
                                                                        \
        while (*(var) == value) {                                       \
            target_cntr = shmem_transport_received_cntr_get();          \
            COMPILER_FENCE();                                           \
            if (*(var) != value) break;                                 \
            shmem_transport_received_cntr_wait(target_cntr + 1);        \
        }                                                               \
    } while(0)

#define SHMEM_WAIT_UNTIL_BLOCK(var, cond, value)                        \
    do {                                                                \
        uint64_t target_cntr;                                           \
        int cmpret;                                                     \
                                                                        \
        COMP(cond, *(var), value, cmpret);                              \
        while (!cmpret) {                                               \
            target_cntr = shmem_transport_received_cntr_get();          \
            COMPILER_FENCE();                                           \
            COMP(cond, *(var), value, cmpret);                          \
            if (cmpret) break;                                          \
            shmem_transport_received_cntr_wait(target_cntr + 1);        \
            COMP(cond, *(var), value, cmpret);                          \
        }                                                               \
    } while(0)

#if defined(ENABLE_HARD_POLLING)
#define SHMEM_INTERNAL_WAIT_UNTIL(var, cond, value)                     \
    SHMEM_WAIT_UNTIL_POLL(var, cond, value)
#else
#define SHMEM_INTERNAL_WAIT_UNTIL(var, cond, value)                     \
    if (shmem_internal_thread_level == SHMEM_THREAD_SINGLE) {           \
        SHMEM_WAIT_UNTIL_BLOCK(var, cond, value);                       \
    } else {                                                            \
        SHMEM_WAIT_UNTIL_POLL(var, cond, value);                        \
    }
#endif

#define SHMEM_WAIT(var, value) do {                                     \
        SHMEM_INTERNAL_WAIT_UNTIL(var, SHMEM_CMP_NE, value);            \
        shmem_internal_membar_acquire();                                \
        shmem_transport_syncmem();                                      \
    } while (0)

#define SHMEM_WAIT_UNTIL(var, cond, value) do {                         \
        SHMEM_INTERNAL_WAIT_UNTIL(var, cond, value);                    \
        shmem_internal_membar_acquire();                                \
        shmem_transport_syncmem();                                      \
    } while (0)

#endif
