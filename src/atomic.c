/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
 */

#include "config.h"

#include <portals4.h>
#include <stdio.h>
#include <stdlib.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"

float
shmem_float_swap(float *target, float value, int pe)
{
    int ret;
    float newval;
    ret = shmem_internal_swap(target, &value, &newval, sizeof(float), pe, PTL_FLOAT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


double
shmem_double_swap(double *target, double value, int pe)
{
    int ret;
    double newval;
    ret = shmem_internal_swap(target, &value, &newval, sizeof(double), pe, PTL_DOUBLE);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


short
shmem_short_swap(short *target, short value, int pe)
{
    int ret;
    short newval;
    ret = shmem_internal_swap(target, &value, &newval, sizeof(short), pe, PTL_SHORT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


int
shmem_int_swap(int *target, int value, int pe)
{
    int ret;
    int newval;
    ret = shmem_internal_swap(target, &value, &newval, sizeof(int), pe, PTL_INT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


long
shmem_long_swap(long *target, long value, int pe)
{
    int ret;
    long newval;
    ret = shmem_internal_swap(target, &value, &newval, sizeof(long), pe, PTL_LONG);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


long long
shmem_longlong_swap(long long *target, long long value, int pe)
{
    int ret;
    long long newval;
    /* BWB: FIX ME: need better type for long long */
    ret = shmem_internal_swap(target, &value, &newval, sizeof(long long), pe, PTL_LONG);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


long
shmem_swap(long *target, long value, int pe)
{
    int ret;
    long newval;
    ret = shmem_internal_swap(target, &value, &newval, sizeof(long), pe, PTL_LONG);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


short
shmem_short_cswap(short *target, short cond, short value, int pe)
{
    int ret;
    short newval;
    ret = shmem_internal_cswap(target, &value, &newval, &cond, sizeof(short), pe, PTL_SHORT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


int
shmem_int_cswap(int *target, int cond, int value, int pe)
{
    int ret;
    int newval;
    ret = shmem_internal_cswap(target, &value, &newval, &cond, sizeof(int), pe, PTL_SHORT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


long
shmem_long_cswap(long *target, long cond, long value, int pe)
{
    int ret;
    long newval;
    ret = shmem_internal_cswap(target, &value, &newval, &cond, sizeof(long), pe, PTL_SHORT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


long long
shmem_longlong_cswap(long long * target, long long cond, 
                     long long value, int pe)
{
    int ret;
    long long newval;
    /* BWB: FIX ME: need better type for long long */
    ret = shmem_internal_cswap(target, &value, &newval, &cond, sizeof(long long), pe, PTL_SHORT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


void
shmem_int_inc(int *target, int pe)
{
    int ret;
    int tmp = 1;
    ret = shmem_internal_atomic(target, &tmp, sizeof(int), pe, PTL_SUM, PTL_INT);
    shmem_internal_put_wait(ret);
}


void
shmem_long_inc(long *target, int pe)
{
    int ret;
    long tmp = 1;
    ret = shmem_internal_atomic(target, &tmp, sizeof(long), pe, PTL_SUM, PTL_LONG);
    shmem_internal_put_wait(ret);
}


void
shmem_longlong_inc(long long *target, int pe)
{
    int ret;
    long long tmp = 1;
    /* BWB: FIX ME: need better type for long long */
    ret = shmem_internal_atomic(target, &tmp, sizeof(long long), pe, PTL_SUM, PTL_LONG);
    shmem_internal_put_wait(ret);
}


int
shmem_int_finc(int *target, int pe)
{
    int ret;
    int oldval, tmp = 1;
    ret = shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(int), pe, PTL_SUM, PTL_INT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return oldval;
}


long
shmem_long_finc(long *target, int pe)
{
    int ret;
    long oldval, tmp = 1;
    ret = shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(long), pe, PTL_SUM, PTL_LONG);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return oldval;
}


long long
shmem_longlong_finc(long long *target, int pe)
{
    int ret;
    long long oldval, tmp = 1;
    /* BWB: FIX ME: need better type for long long */
    ret = shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(long long), pe, PTL_SUM, PTL_LONG);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return oldval;
}


void
shmem_int_add(int *target, int value, int pe)
{
    int ret;
    ret = shmem_internal_atomic(target, &value, sizeof(int), pe, PTL_SUM, PTL_INT);
    shmem_internal_put_wait(ret);
}


void
shmem_long_add(long *target, long value, int pe)
{
    int ret;
    ret = shmem_internal_atomic(target, &value, sizeof(long), pe, PTL_SUM, PTL_LONG);
    shmem_internal_put_wait(ret);
}


void
shmem_longlong_add(long long *target, long long value, int pe)
{
    int ret;
    /* BWB: FIX ME: need better type for long long */
    ret = shmem_internal_atomic(target, &value, sizeof(long long), pe, PTL_SUM, PTL_LONG);
    shmem_internal_put_wait(ret);
}


int
shmem_int_fadd(int *target, int value, int pe)
{
    int ret;
    int oldval;
    ret = shmem_internal_fetch_atomic(target, &value, &oldval, sizeof(int), pe, PTL_SUM, PTL_INT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return oldval;
}


long
shmem_long_fadd(long *target, long value, int pe)
{
    int ret;
    long oldval;
    ret = shmem_internal_fetch_atomic(target, &value, &oldval, sizeof(long), pe, PTL_SUM, PTL_LONG);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return oldval;
}


long long
shmem_longlong_fadd(long long *target, long long value,
                    int pe)
{
    int ret;
    long long oldval;
    /* BWB: FIX ME: need better type for long long */
    ret = shmem_internal_fetch_atomic(target, &value, &oldval, sizeof(long long), pe, PTL_SUM, PTL_LONG);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return oldval;
}


/*
 * Use basic MCS distributed lock algorithm for lock
 */
struct lock_t {
    short last; /* only has meaning on PE 0 */
    short next;
    short signal;
    short pad;
};
typedef struct lock_t lock_t;

void
shmem_clear_lock(long *lockp)
{
    lock_t *lock = (lock_t*) lockp;
    short curr;

    shmem_quiet();
    curr = shmem_short_cswap(&(lock->last), shmem_int_my_pe + 1, 0, 0);
    if (curr != shmem_int_my_pe + 1) {
        shmem_short_wait(&(lock->next), 0);
        shmem_short_p(&(lock->signal), 1, lock->next - 1);
    }
}


void
shmem_set_lock(long *lockp)
{
    lock_t *lock = (lock_t*) lockp;
    short curr;

    shmem_short_p(&(lock->next), 0, shmem_int_my_pe);
    shmem_short_p(&(lock->signal), 0, shmem_int_my_pe);
    shmem_quiet();
    curr = shmem_short_swap(&(lock->last), shmem_int_my_pe + 1, 0);
    if (0 != curr) {
        shmem_short_p(&(lock->next), shmem_int_my_pe + 1, curr - 1);
        shmem_short_wait(&(lock->signal), 0);
    }
}


int
shmem_test_lock(long *lockp)
{
    lock_t *lock = (lock_t*) lockp;
    short curr;

    shmem_short_p(&(lock->next), 0, shmem_int_my_pe);
    shmem_short_p(&(lock->signal), 0, shmem_int_my_pe);
    shmem_quiet();
    curr = shmem_short_cswap(&(lock->last), 0, shmem_int_my_pe + 1, 0);
    if (0 == curr) {
        return 0;
    }
    return 1;
}
