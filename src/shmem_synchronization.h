/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 * 
 * Copyright (c) 2015 Intel Corporation. All rights reserved.
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


#if defined(ENABLE_HARD_POLLING)

#define SHMEM_WAIT(var, value)                           \
    do {                                                 \
        while (*(var) == value) { SPINLOCK_BODY(); }     \
    } while(0)

#define SHMEM_WAIT_UNTIL(var, cond, value)               \
    do {                                                 \
        int cmpret;                                      \
                                                         \
        COMP(cond, *(var), value, cmpret);               \
        while (!cmpret) {                                \
            SPINLOCK_BODY();                             \
            COMP(cond, *(var), value, cmpret);           \
        }                                                \
    } while(0)

#else

#define SHMEM_WAIT(var, value)                                          \
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

#define SHMEM_WAIT_UNTIL(var, cond, value)                              \
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

#endif

#endif
