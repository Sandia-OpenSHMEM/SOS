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

#ifndef SHMEM_SYNCHRONIZATION_H
#define SHMEM_SYNCHRONIZATION_H

#include "shmem_atomic.h"
#include "shmem_comm.h"
#include "transport.h"


static inline void
shmem_internal_quiet(shmem_ctx_t ctx)
{
    int ret;

    ret = shmem_transport_quiet((shmem_transport_ctx_t *)ctx);
    if (0 != ret) { RAISE_ERROR(ret); }

#ifdef USE_XPMEM
    ret = shmem_transport_xpmem_quiet();
    if (0 != ret) { RAISE_ERROR(ret); }
#endif
}


static inline void
shmem_internal_fence(shmem_ctx_t ctx)
{
    int ret;

    ret = shmem_transport_fence((shmem_transport_ctx_t *)ctx);
    if (0 != ret) { RAISE_ERROR(ret); }

#ifdef USE_XPMEM
    ret = shmem_transport_xpmem_fence();
    if (0 != ret) { RAISE_ERROR(ret); }
#endif
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
        while (*(var) == value) { SPINLOCK_BODY(); }     \
    } while(0)

#define SHMEM_WAIT_UNTIL_POLL(var, cond, value)          \
    do {                                                 \
        int cmpret;                                      \
                                                         \
        COMP(cond, *(var), value, cmpret);               \
        while (!cmpret) {                                \
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
#define SHMEM_WAIT(var, value) SHMEM_WAIT_POLL(var, value)
#define SHMEM_WAIT_UNTIL(var, cond, value) SHMEM_WAIT_UNTIL_POLL(var, cond, value)

#else
#define SHMEM_WAIT(var, value) do {                                     \
        if (shmem_internal_thread_level == SHMEM_THREAD_SINGLE) {       \
            SHMEM_WAIT_BLOCK(var, value);                               \
        } else {                                                        \
            SHMEM_WAIT_POLL(var, value);                                \
        }                                                               \
    } while (0)

#define SHMEM_WAIT_UNTIL(var, cond, value) do {                         \
        if (shmem_internal_thread_level == SHMEM_THREAD_SINGLE) {       \
            SHMEM_WAIT_UNTIL_BLOCK(var, cond, value);                   \
        } else {                                                        \
            SHMEM_WAIT_UNTIL_POLL(var, cond, value);                    \
        }                                                               \
    } while (0)
#endif /* HARD_POLLING */

#endif
