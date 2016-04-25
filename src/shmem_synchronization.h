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


static inline void
shmem_internal_quiet(void)
{
    int ret;
 
    ret = shmem_transport_quiet();
    if (0 != ret) { RAISE_ERROR(ret); }

#ifdef USE_XPMEM
    ret = shmem_transport_xpmem_quiet();
    if (0 != ret) { RAISE_ERROR(ret); }
#endif
}
 
 
static inline void
shmem_internal_fence(void)
{
    int ret;
 
    ret = shmem_transport_fence();
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


#if defined(ENABLE_HARD_POLLING) || defined(USE_ON_NODE_COMMS)

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

#elif defined(USE_PORTALS4)

#define SHMEM_WAIT(var, value)                                          \
    do {                                                                \
        int ret;                                                        \
        ptl_ct_event_t ct;                                              \
                                                                        \
        while (*(var) == value) {                                       \
            ret = PtlCTGet(shmem_transport_portals4_target_ct_h, &ct);  \
            if (PTL_OK != ret) { RAISE_ERROR(ret); }                    \
            if (0 != ct.failure) { RAISE_ERROR_STR("Target CT failure"); } \
            COMPILER_FENCE();                                           \
            if (*(var) != value) break;                                 \
            ret = PtlCTWait(shmem_transport_portals4_target_ct_h,       \
                            ct.success + 1,                             \
                            &ct);                                       \
            if (PTL_OK != ret) { RAISE_ERROR(ret); }                    \
            if (0 != ct.failure) { RAISE_ERROR_STR("Target CT failure"); } \
        }                                                               \
    } while(0)

#define SHMEM_WAIT_UNTIL(var, cond, value)                              \
    do {                                                                \
        int ret, cmpret;                                                \
        ptl_ct_event_t ct;                                              \
                                                                        \
        COMP(cond, *(var), value, cmpret);                              \
        while (!cmpret) {                                               \
            ret = PtlCTGet(shmem_transport_portals4_target_ct_h, &ct);  \
            if (0 != ct.failure) { RAISE_ERROR_STR("Target CT failure"); } \
            if (PTL_OK != ret) { RAISE_ERROR(ret); }                    \
            COMPILER_FENCE();                                           \
            COMP(cond, *(var), value, cmpret);                          \
            if (cmpret) break;                                          \
            ret = PtlCTWait(shmem_transport_portals4_target_ct_h,       \
                            ct.success + 1,                             \
                            &ct);                                       \
            if (PTL_OK != ret) { RAISE_ERROR(ret); }                    \
            if (0 != ct.failure) { RAISE_ERROR_STR("Target CT failure"); } \
            COMP(cond, *(var), value, cmpret);                          \
        }                                                               \
    } while(0)
#elif defined(USE_OFI)

#if defined(ENABLE_HARD_POLLING) || defined(USE_ON_NODE_COMMS)

#define SHMEM_WAIT(var, value)                                          \
    do {                                                                \
        while (*(var) == value) {                                       \
            COMPILER_FENCE();                                           \
            if (*(var) != value) break;                                 \
        }                                                               \
    }   while(0)

#define SHMEM_WAIT_UNTIL(var, cond, value)                              \
    do {                                                                \
        int cmpret;                                                     \
        do {                                                            \
            COMP(cond, *(var), value, cmpret);                          \
        } while (!cmpret);                                              \
    } while(0)

#else

#define SHMEM_WAIT(var, value)                                          \
    do {                                                                \
        int ret;                                                        \
        uint64_t count;                                                 \
                                                                        \
        while (*(var) == value) {                                       \
            count = fi_cntr_read(shmem_transport_ofi_target_cntrfd);    \
            COMPILER_FENCE();                                           \
            if (*(var) != value) break;                                 \
            ret = fi_cntr_wait(shmem_transport_ofi_target_cntrfd,       \
                               (count + 1),-1);                         \
            if (ret) { RAISE_ERROR(ret); }                              \
        }                                                               \
    } while(0)

#define SHMEM_WAIT_UNTIL(var, cond, value)                              \
    do {                                                                \
        int ret, cmpret;                                                \
        uint64_t count;                                                 \
        COMP(cond, *(var), value, cmpret);                              \
        while(!cmpret) {                                                \
           count =  fi_cntr_read(shmem_transport_ofi_target_cntrfd);    \
           COMPILER_FENCE();                                            \
           COMP(cond, *(var), value, cmpret);                           \
           if (cmpret) break;                                           \
           ret = fi_cntr_wait(shmem_transport_ofi_target_cntrfd,        \
                              (count+1),-1);                            \
           if (ret) { RAISE_ERROR(ret); }                               \
           COMP(cond, *(var), value, cmpret);                           \
        }                                                               \
    } while(0)


#endif /* if defined(ENABLE_HARD_POLLING) || defined(USE_ON_NODE_COMMS) */

#else
#error "SHMEM_WAIT and SHMEM_WAIT_UNTIL not defined!"
#endif

#endif
