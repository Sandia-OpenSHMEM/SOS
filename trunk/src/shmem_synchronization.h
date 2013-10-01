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

#ifndef SHMEM_SYNCHRONIZATION_H
#define SHMEM_SYNCHRONIZATION_H

#include "shmem_comm.h"


static inline void
shmem_internal_quiet(void)
{
    int ret;
 
#ifdef USE_PORTALS4
    ret = shmem_transport_portals4_quiet();
    if (0 != ret) { RAISE_ERROR(ret); }
#endif
#ifdef USE_XPMEM
    ret = shmem_transport_xpmem_quiet();
    if (0 != ret) { RAISE_ERROR(ret); }
#endif
}
 
 
static inline void
shmem_internal_fence(void)
{
    int ret;
 
#ifdef USE_PORTALS4
    ret = shmem_transport_portals4_fence();
    if (0 != ret) { RAISE_ERROR(ret); }
#endif
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
        while (*var == value) { SPINLOCK_BODY(); }       \
    } while(0)

#define SHMEM_WAIT_UNTIL(var, cond, value)               \
    do {                                                 \
        int cmpret;                                      \
                                                         \
        COMP(cond, *var, value, cmpret);                 \
        while (!cmpret) {                                \
            SPINLOCK_BODY();                             \
            COMP(cond, *var, value, cmpret);             \
        }                                                \
    } while(0)

#elif defined(USE_PORTALS4)

#define SHMEM_WAIT(var, value)                                          \
    do {                                                                \
        int ret;                                                        \
        ptl_ct_event_t ct;                                              \
                                                                        \
        while (*var == value) {                                         \
            ret = PtlCTGet(shmem_transport_portals4_target_ct_h, &ct);  \
            if (PTL_OK != ret) { RAISE_ERROR(ret); }                    \
            if (*var != value) return;                                  \
            ret = PtlCTWait(shmem_transport_portals4_target_ct_h,       \
                            ct.success + ct.failure + 1,                \
                            &ct);                                       \
            if (PTL_OK != ret) { RAISE_ERROR(ret); }                    \
        }                                                               \
    } while(0)

#define SHMEM_WAIT_UNTIL(var, cond, value)                              \
    do {                                                                \
        int ret, cmpret;                                                \
        ptl_ct_event_t ct;                                              \
                                                                        \
        COMP(cond, *var, value, cmpret);                                \
        while (!cmpret) {                                               \
            ret = PtlCTGet(shmem_transport_portals4_target_ct_h, &ct);  \
            if (PTL_OK != ret) { RAISE_ERROR(ret); }                    \
            COMP(cond, *var, value, cmpret);                            \
            if (cmpret) return;                                         \
            ret = PtlCTWait(shmem_transport_portals4_target_ct_h,       \
                            ct.success + ct.failure + 1,                \
                            &ct);                                       \
            if (PTL_OK != ret) { RAISE_ERROR(ret); }                    \
            COMP(cond, *var, value, cmpret);                            \
        }                                                               \
    } while(0)
#else
#error "SHMEM_WAIT and SHMEM_WAIT_UNTIL not defined!"
#endif

#endif
