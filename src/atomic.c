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


int
shmem_int_swap(int *target, int value, int pe)
{
    int ret;
    int newval;
    ret = shmem_internal_swap(target, &value, &newval, sizeof(int), pe, DTYPE_INT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


long
shmem_long_swap(long *target, long value, int pe)
{
    int ret;
    long newval;
    ret = shmem_internal_swap(target, &value, &newval, sizeof(long), pe, DTYPE_LONG);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


long long
shmem_longlong_swap(long long *target, long long value, int pe)
{
    int ret;
    long long newval;
    ret = shmem_internal_swap(target, &value, &newval, sizeof(long long), pe, DTYPE_LONG_LONG);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


long
shmem_swap(long *target, long value, int pe)
{
    int ret;
    long newval;
    ret = shmem_internal_swap(target, &value, &newval, sizeof(long), pe, DTYPE_LONG);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


int
shmem_int_cswap(int *target, int cond, int value, int pe)
{
    int ret;
    int newval;
    ret = shmem_internal_cswap(target, &value, &newval, &cond, sizeof(int), pe, DTYPE_INT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


long
shmem_long_cswap(long *target, long cond, long value, int pe)
{
    int ret;
    long newval;
    ret = shmem_internal_cswap(target, &value, &newval, &cond, sizeof(long), pe, DTYPE_LONG);
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
    ret = shmem_internal_cswap(target, &value, &newval, &cond, sizeof(long long), pe, DTYPE_LONG_LONG);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return newval;
}


void
shmem_int_inc(int *target, int pe)
{
    int ret;
    int tmp = 1;
    ret = shmem_internal_atomic(target, &tmp, sizeof(int), pe, PTL_SUM, DTYPE_INT);
    shmem_internal_put_wait(ret);
}


void
shmem_long_inc(long *target, int pe)
{
    int ret;
    long tmp = 1;
    ret = shmem_internal_atomic(target, &tmp, sizeof(long), pe, PTL_SUM, DTYPE_LONG);
    shmem_internal_put_wait(ret);
}


void
shmem_longlong_inc(long long *target, int pe)
{
    int ret;
    long long tmp = 1;
    ret = shmem_internal_atomic(target, &tmp, sizeof(long long), pe, PTL_SUM, DTYPE_LONG_LONG);
    shmem_internal_put_wait(ret);
}


int
shmem_int_finc(int *target, int pe)
{
    int ret;
    int oldval, tmp = 1;
    ret = shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(int), pe, PTL_SUM, DTYPE_INT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return oldval;
}


long
shmem_long_finc(long *target, int pe)
{
    int ret;
    long oldval, tmp = 1;
    ret = shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(long), pe, PTL_SUM, DTYPE_LONG);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return oldval;
}


long long
shmem_longlong_finc(long long *target, int pe)
{
    int ret;
    long long oldval, tmp = 1;
    ret = shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(long long), pe, PTL_SUM, DTYPE_LONG_LONG);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return oldval;
}


void
shmem_int_add(int *target, int value, int pe)
{
    int ret;
    ret = shmem_internal_atomic(target, &value, sizeof(int), pe, PTL_SUM, DTYPE_INT);
    shmem_internal_put_wait(ret);
}


void
shmem_long_add(long *target, long value, int pe)
{
    int ret;
    ret = shmem_internal_atomic(target, &value, sizeof(long), pe, PTL_SUM, DTYPE_LONG);
    shmem_internal_put_wait(ret);
}


void
shmem_longlong_add(long long *target, long long value, int pe)
{
    int ret;
    ret = shmem_internal_atomic(target, &value, sizeof(long long), pe, PTL_SUM, DTYPE_LONG_LONG);
    shmem_internal_put_wait(ret);
}


int
shmem_int_fadd(int *target, int value, int pe)
{
    int ret;
    int oldval;
    ret = shmem_internal_fetch_atomic(target, &value, &oldval, sizeof(int), pe, PTL_SUM, DTYPE_INT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return oldval;
}


long
shmem_long_fadd(long *target, long value, int pe)
{
    int ret;
    long oldval;
    ret = shmem_internal_fetch_atomic(target, &value, &oldval, sizeof(long), pe, PTL_SUM, DTYPE_LONG);
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
    ret = shmem_internal_fetch_atomic(target, &value, &oldval, sizeof(long long), pe, PTL_SUM, DTYPE_LONG_LONG);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    return oldval;
}


/*
 * Use basic MCS distributed lock algorithm for lock
 */
struct lock_t {
    int last; /* has meaning only on PE 0 */
    int data; /* has meaning on all PEs */
};
typedef struct lock_t lock_t;

#define NEXT_MASK   0x7FFFFFFFU
#define SIGNAL_MASK 0x80000000U
#define NEXT(A)   (A & NEXT_MASK)
#define SIGNAL(A) (A & SIGNAL_MASK)


void
shmem_clear_lock(long *lockp)
{
    lock_t *lock = (lock_t*) lockp;
    int ret, curr, cond, zero = 0, sig = SIGNAL_MASK;

    shmem_quiet();

    /* release the lock if I'm the last to try to obtain it */
    cond = shmem_int_my_pe + 1;
    ret = shmem_internal_cswap(&(lock->last), &zero, &curr, &cond, sizeof(int), 0, DTYPE_INT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    /* if local PE was not the last to hold the lock, have to look for the next in line */
    if (curr != shmem_int_my_pe + 1) {
        /* wait for next part of the data block to be non-zero */
        while (NEXT(lock->data) == 0) {
            shmem_int_wait(&(lock->data), SIGNAL(lock->data));
        }
        /* set the signal bit on new lock holder */
        ret = shmem_internal_mswap(&(lock->data), &sig, &curr, &sig, sizeof(int), NEXT(lock->data) - 1, DTYPE_INT);
        shmem_internal_put_wait(ret);
        shmem_internal_get_wait();
    }
}


void
shmem_set_lock(long *lockp)
{
    lock_t *lock = (lock_t*) lockp;
    int ret, curr, zero = 0, me = shmem_int_my_pe + 1, next_mask = NEXT_MASK;

    /* initialize my elements to zero */
    ret = shmem_internal_put(&(lock->data), &zero, sizeof(zero), shmem_int_my_pe);
    shmem_internal_put_wait(ret);
    shmem_quiet();

    /* update last with my value to add me to the queue */
    ret = shmem_internal_swap(&(lock->last), &me, &curr, sizeof(int), 0, DTYPE_INT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    /* If I wasn't the first, need to add myself to the previous last's next */
    if (0 != curr) {
        ret = shmem_internal_mswap(&(lock->data), &me, &curr, &next_mask, sizeof(int), curr - 1, DTYPE_INT);
        shmem_internal_put_wait(ret);
        shmem_internal_get_wait();
        /* now wait for the signal part of data to be non-zero */
        while (SIGNAL(lock->data) == 0) {
            shmem_int_wait(&(lock->data), NEXT(lock->data));
        }
    }
}


int
shmem_test_lock(long *lockp)
{
    lock_t *lock = (lock_t*) lockp;
    int ret, curr, me = shmem_int_my_pe + 1, zero = 0;

    /* initialize my elements to zero */
    ret = shmem_internal_put(&(lock->data), &zero, sizeof(zero), shmem_int_my_pe);
    shmem_internal_put_wait(ret);
    shmem_quiet();

    /* add self to last if and only if the lock is zero (ie, no one has the lock) */
    ret = shmem_internal_cswap(&(lock->last), &me, &curr, &zero, sizeof(int), 0, DTYPE_INT);
    shmem_internal_put_wait(ret);
    shmem_internal_get_wait();
    if (0 == curr) {
        return 0;
    }
    return 1;
}
