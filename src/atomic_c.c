/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 * 
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"

#ifdef ENABLE_PROFILING
#include "pshmem.h"

#pragma weak shmem_float_swap = pshmem_float_swap
#define shmem_float_swap pshmem_float_swap

#pragma weak shmem_double_swap = pshmem_double_swap
#define shmem_double_swap pshmem_double_swap

#pragma weak shmem_int_swap = pshmem_int_swap
#define shmem_int_swap pshmem_int_swap

#pragma weak shmem_long_swap = pshmem_long_swap
#define shmem_long_swap pshmem_long_swap

#pragma weak shmem_longlong_swap = pshmem_longlong_swap
#define shmem_longlong_swap pshmem_longlong_swap

#pragma weak shmem_swap = pshmem_swap
#define shmem_swap pshmem_swap

#pragma weak shmem_int_cswap = pshmem_int_cswap
#define shmem_int_cswap pshmem_int_cswap

#pragma weak shmem_long_cswap = pshmem_long_cswap
#define shmem_long_cswap pshmem_long_cswap

#pragma weak shmem_longlong_cswap = pshmem_longlong_cswap
#define shmem_longlong_cswap pshmem_longlong_cswap

#pragma weak shmem_int_inc = pshmem_int_inc
#define shmem_int_inc pshmem_int_inc

#pragma weak shmem_long_inc = pshmem_long_inc
#define shmem_long_inc pshmem_long_inc

#pragma weak shmem_longlong_inc = pshmem_longlong_inc
#define shmem_longlong_inc pshmem_longlong_inc

#pragma weak shmem_int_finc = pshmem_int_finc
#define shmem_int_finc pshmem_int_finc

#pragma weak shmem_long_finc = pshmem_long_finc
#define shmem_long_finc pshmem_long_finc

#pragma weak shmem_longlong_finc = pshmem_longlong_finc
#define shmem_longlong_finc pshmem_longlong_finc

#pragma weak shmem_int_add = pshmem_int_add
#define shmem_int_add pshmem_int_add

#pragma weak shmem_long_add = pshmem_long_add
#define shmem_long_add pshmem_long_add

#pragma weak shmem_longlong_add = pshmem_longlong_add
#define shmem_longlong_add pshmem_longlong_add

#pragma weak shmem_int_fadd = pshmem_int_fadd
#define shmem_int_fadd pshmem_int_fadd

#pragma weak shmem_long_fadd = pshmem_long_fadd
#define shmem_long_fadd pshmem_long_fadd

#pragma weak shmem_longlong_fadd = pshmem_longlong_fadd
#define shmem_longlong_fadd pshmem_longlong_fadd

#pragma weak shmem_int_fetch = pshmem_int_fetch
#define shmem_int_fetch pshmem_int_fetch

#pragma weak shmem_long_fetch = pshmem_long_fetch
#define shmem_long_fetch pshmem_long_fetch

#pragma weak shmem_longlong_fetch = pshmem_longlong_fetch
#define shmem_longlong_fetch pshmem_longlong_fetch

#pragma weak shmem_float_fetch = pshmem_float_fetch
#define shmem_float_fetch pshmem_float_fetch

#pragma weak shmem_double_fetch = pshmem_double_fetch
#define shmem_double_fetch pshmem_double_fetch

#pragma weak shmem_int_set = pshmem_int_set
#define shmem_int_set pshmem_int_set

#pragma weak shmem_long_set = pshmem_long_set
#define shmem_long_set pshmem_long_set

#pragma weak shmem_longlong_set = pshmem_longlong_set
#define shmem_longlong_set pshmem_longlong_set

#pragma weak shmem_float_set = pshmem_float_set
#define shmem_float_set pshmem_float_set

#pragma weak shmem_double_set = pshmem_double_set
#define shmem_double_set pshmem_double_set

#endif /* ENABLE_PROFILING */


float
shmem_float_swap(float *target, float value, int pe)
{
    float newval;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_swap(target, &value, &newval, sizeof(float), pe, SHM_INTERNAL_FLOAT);
    shmem_internal_get_wait();
    return newval;
}


double
shmem_double_swap(double *target, double value, int pe)
{
    double newval;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_swap(target, &value, &newval, sizeof(double), pe, SHM_INTERNAL_DOUBLE);
    shmem_internal_get_wait();
    return newval;
}


int
shmem_int_swap(int *target, int value, int pe)
{
    int newval;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_swap(target, &value, &newval, sizeof(int), pe, DTYPE_INT);
    shmem_internal_get_wait();
    return newval;
}


long
shmem_long_swap(long *target, long value, int pe)
{
    long newval;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_swap(target, &value, &newval, sizeof(long), pe, DTYPE_LONG);
    shmem_internal_get_wait();
    return newval;
}


long long
shmem_longlong_swap(long long *target, long long value, int pe)
{
    long long newval;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_swap(target, &value, &newval, sizeof(long long), pe, DTYPE_LONG_LONG);
    shmem_internal_get_wait();
    return newval;
}


long
shmem_swap(long *target, long value, int pe)
{
    long newval;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_swap(target, &value, &newval, sizeof(long), pe, DTYPE_LONG);
    shmem_internal_get_wait();
    return newval;
}


int
shmem_int_cswap(int *target, int cond, int value, int pe)
{
    int newval;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_cswap(target, &value, &newval, &cond, sizeof(int), pe, DTYPE_INT);
    shmem_internal_get_wait();
    return newval;
}


long
shmem_long_cswap(long *target, long cond, long value, int pe)
{
    long newval;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_cswap(target, &value, &newval, &cond, sizeof(long), pe, DTYPE_LONG);
    shmem_internal_get_wait();
    return newval;
}


long long
shmem_longlong_cswap(long long * target, long long cond, 
                     long long value, int pe)
{
    long long newval;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_cswap(target, &value, &newval, &cond, sizeof(long long), pe, DTYPE_LONG_LONG);
    shmem_internal_get_wait();
    return newval;
}


void
shmem_int_inc(int *target, int pe)
{
    int tmp = 1;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_atomic_small(target, &tmp, sizeof(int), pe, SHM_INTERNAL_SUM, DTYPE_INT);
}


void
shmem_long_inc(long *target, int pe)
{
    long tmp = 1;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_atomic_small(target, &tmp, sizeof(long), pe, SHM_INTERNAL_SUM, DTYPE_LONG);
}


void
shmem_longlong_inc(long long *target, int pe)
{
    long long tmp = 1;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_atomic_small(target, &tmp, sizeof(long long), pe, SHM_INTERNAL_SUM, DTYPE_LONG_LONG);
}


int
shmem_int_finc(int *target, int pe)
{
    int oldval, tmp = 1;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(int), pe, SHM_INTERNAL_SUM, DTYPE_INT);
    shmem_internal_get_wait();
    return oldval;
}


long
shmem_long_finc(long *target, int pe)
{
    long oldval, tmp = 1;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(long), pe, SHM_INTERNAL_SUM, DTYPE_LONG);
    shmem_internal_get_wait();
    return oldval;
}


long long
shmem_longlong_finc(long long *target, int pe)
{
    long long oldval, tmp = 1;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(long long), pe, SHM_INTERNAL_SUM, DTYPE_LONG_LONG);
    shmem_internal_get_wait();
    return oldval;
}


void
shmem_int_add(int *target, int value, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_atomic_small(target, &value, sizeof(int), pe, SHM_INTERNAL_SUM, DTYPE_INT);
}


void
shmem_long_add(long *target, long value, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_atomic_small(target, &value, sizeof(long), pe, SHM_INTERNAL_SUM, DTYPE_LONG);
}


void
shmem_longlong_add(long long *target, long long value, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_atomic_small(target, &value, sizeof(long long), pe, SHM_INTERNAL_SUM, DTYPE_LONG_LONG);
}


int
shmem_int_fadd(int *target, int value, int pe)
{
    int oldval;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_fetch_atomic(target, &value, &oldval, sizeof(int), pe, SHM_INTERNAL_SUM, DTYPE_INT);
    shmem_internal_get_wait();
    return oldval;
}


long
shmem_long_fadd(long *target, long value, int pe)
{
    long oldval;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_fetch_atomic(target, &value, &oldval, sizeof(long), pe, SHM_INTERNAL_SUM, DTYPE_LONG);
    shmem_internal_get_wait();
    return oldval;
}


long long
shmem_longlong_fadd(long long *target, long long value,
                    int pe)
{
    long long oldval;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_fetch_atomic(target, &value, &oldval, sizeof(long long), pe, SHM_INTERNAL_SUM, DTYPE_LONG_LONG);
    shmem_internal_get_wait();
    return oldval;
}


/** Generate atomic fetch C bindings */
#define SHMEM_CAPI_ATOMIC_FETCH(type_name, c_type, dtype)                       \
c_type                                                                          \
shmem_##type_name##_fetch(const c_type *source, int pe)                         \
{                                                                               \
    c_type val;                                                                 \
                                                                                \
    SHMEM_ERR_CHECK_INITIALIZED();                                              \
                                                                                \
    shmem_internal_atomic_fetch(&val, (void *) source, sizeof(c_type), pe,      \
                                dtype);                                         \
    shmem_internal_get_wait();                                                  \
    return val;                                                                 \
}

SHMEM_CAPI_ATOMIC_FETCH(int, int, DTYPE_INT)
SHMEM_CAPI_ATOMIC_FETCH(long, long, DTYPE_LONG)
SHMEM_CAPI_ATOMIC_FETCH(longlong, long long, DTYPE_LONG_LONG)
SHMEM_CAPI_ATOMIC_FETCH(float, float, SHM_INTERNAL_FLOAT)
SHMEM_CAPI_ATOMIC_FETCH(double, double, SHM_INTERNAL_DOUBLE)

#undef SHMEM_CAPI_ATOMIC_FETCH


/* Generate atomic set C bindings
 *
 * Note: value is passed to the transport from the stack.  Transport must be
 * finished with value argument when shmem_internal_atomic_set returns.
 */
#define SHMEM_CAPI_ATOMIC_SET(type_name, c_type, dtype)                         \
void                                                                            \
shmem_##type_name##_set(const c_type *dest, c_type value, int pe)               \
{                                                                               \
    SHMEM_ERR_CHECK_INITIALIZED();                                              \
                                                                                \
    shmem_internal_atomic_set((void *) dest, &value, sizeof(c_type), pe, dtype);\
}

SHMEM_CAPI_ATOMIC_SET(int, int, DTYPE_INT)
SHMEM_CAPI_ATOMIC_SET(long, long, DTYPE_LONG)
SHMEM_CAPI_ATOMIC_SET(longlong, long long, DTYPE_LONG_LONG)
SHMEM_CAPI_ATOMIC_SET(float, float, SHM_INTERNAL_FLOAT)
SHMEM_CAPI_ATOMIC_SET(double, double, SHM_INTERNAL_DOUBLE)

#undef SHMEM_CAPI_ATOMIC_SET
