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
    ptl_ct_event_t ct;

    /* wait for remote completion (acks) of all pending events */
    ret = PtlCTWait(put_ct_h, pending_put_counter, &ct);
    if (PTL_OK != ret) { RAISE_ERROR(ret); }
    if (ct.failure != 0) { RAISE_ERROR(ct.failure); }
}


void
shmem_fence(void)
{
    /* intentionally a no-op */
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


#define SHMEM_WAIT(var, value)                           \
    do {                                                 \
        int ret;                                         \
        ptl_ct_event_t ct;                               \
                                                         \
        while (*var == value) {                          \
            ret = PtlCTGet(target_ct_h, &ct);            \
            if (PTL_OK != ret) { RAISE_ERROR(ret); }     \
            if (*var != value) return;                   \
            ret = PtlCTWait(target_ct_h,                 \
                            ct.success + ct.failure + 1, \
                            &ct);                        \
            if (PTL_OK != ret) { RAISE_ERROR(ret); }     \
        }                                                \
    } while(0)


#define SHMEM_WAIT_UNTIL(var, cond, value)               \
    do {                                                 \
        int ret;                                         \
        ptl_ct_event_t ct;                               \
        int cmpval;                                      \
                                                         \
        COMP(cond, *var, value, cmpval);                 \
        while (!cmpval) {                                \
            ret = PtlCTGet(target_ct_h, &ct);            \
            if (PTL_OK != ret) { RAISE_ERROR(ret); }     \
            COMP(cond, *var, value, cmpval);             \
            if (cmpval) return;                          \
            ret = PtlCTWait(target_ct_h,                 \
                            ct.success + ct.failure + 1, \
                            &ct);                        \
            if (PTL_OK != ret) { RAISE_ERROR(ret); }     \
            COMP(cond, *var, value, cmpval);             \
        }                                                \
    } while(0)


void
shmem_short_wait(short *var, short value)
{
    SHMEM_WAIT(var, value);
}


void
shmem_short_wait_until(short *var, int cond, short value)
{
    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_int_wait(int *var, int value)
{
    SHMEM_WAIT(var, value);
}


void
shmem_int_wait_until(int *var, int cond, int value)
{
    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_long_wait(long *var, long value)
{
    SHMEM_WAIT(var, value);
}


void
shmem_long_wait_until(long *var, int cond, long value)
{
    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_longlong_wait(long long *var, long long value)
{
    SHMEM_WAIT(var, value);
}


void
shmem_longlong_wait_until(long long *var, int cond, long long value)
{
    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_wait(long *ivar, long cmp_value)
{
    SHMEM_WAIT(ivar, cmp_value);
}


void
shmem_wait_until(long *ivar, int cmp, long value)
{
    SHMEM_WAIT_UNTIL(ivar, cmp, value);
}
