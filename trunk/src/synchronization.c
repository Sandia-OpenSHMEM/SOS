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

#include <portals4.h>
#include <stdlib.h>
#include <stdio.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"


void
shmem_quiet(void)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

#ifdef USE_PORTALS4
    ret = shmem_transport_portals4_quiet();
    if (0 != ret) { RAISE_ERROR(ret); }
#endif
#ifdef USE_XPMEM
    ret = shmem_transport_xpmem_quiet();
    if (0 != ret) { RAISE_ERROR(ret); }
#endif
}


void
shmem_fence(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    if (shmem_internal_fence_is_quiet == 1) {
        shmem_quiet();
    }
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

#if defined(__i386__) || defined(__x86_64__)
# define SPINLOCK_BODY() do { __asm__ __volatile__ ("pause" ::: "memory"); } while (0)
#else
# define SPINLOCK_BODY() do { __asm__ __volatile__ (::: "memory"); } while (0)
#endif


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

void
shmem_short_wait(volatile short *var, short value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT(var, value);
}


void
shmem_short_wait_until(volatile short *var, int cond, short value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_int_wait(volatile int *var, int value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT(var, value);
}


void
shmem_int_wait_until(volatile int *var, int cond, int value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_long_wait(volatile long *var, long value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT(var, value);
}


void
shmem_long_wait_until(volatile long *var, int cond, long value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_longlong_wait(volatile long long *var, long long value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT(var, value);
}


void
shmem_longlong_wait_until(volatile long long *var, int cond, long long value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_wait(volatile long *ivar, long cmp_value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT(ivar, cmp_value);
}


void
shmem_wait_until(volatile long *ivar, int cmp, long value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT_UNTIL(ivar, cmp, value);
}
