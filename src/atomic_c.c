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

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"

#ifdef ENABLE_PROFILING

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

#endif /* ENABLE_PROFILING */


float
shmem_float_swap(float *target, float value, int pe)
{
    float newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, &value, &newval, sizeof(float), pe, PTL_FLOAT);
    shmem_internal_get_wait();
    return newval;
}


double
shmem_double_swap(double *target, double value, int pe)
{
    double newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, &value, &newval, sizeof(double), pe, PTL_DOUBLE);
    shmem_internal_get_wait();
    return newval;
}


int
shmem_int_swap(int *target, int value, int pe)
{
    int newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, &value, &newval, sizeof(int), pe, DTYPE_INT);
    shmem_internal_get_wait();
    return newval;
}


long
shmem_long_swap(long *target, long value, int pe)
{
    long newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, &value, &newval, sizeof(long), pe, DTYPE_LONG);
    shmem_internal_get_wait();
    return newval;
}


long long
shmem_longlong_swap(long long *target, long long value, int pe)
{
    long long newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, &value, &newval, sizeof(long long), pe, DTYPE_LONG_LONG);
    shmem_internal_get_wait();
    return newval;
}


long
shmem_swap(long *target, long value, int pe)
{
    long newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, &value, &newval, sizeof(long), pe, DTYPE_LONG);
    shmem_internal_get_wait();
    return newval;
}


int
shmem_int_cswap(int *target, int cond, int value, int pe)
{
    int newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_cswap(target, &value, &newval, &cond, sizeof(int), pe, DTYPE_INT);
    shmem_internal_get_wait();
    return newval;
}


long
shmem_long_cswap(long *target, long cond, long value, int pe)
{
    long newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_cswap(target, &value, &newval, &cond, sizeof(long), pe, DTYPE_LONG);
    shmem_internal_get_wait();
    return newval;
}


long long
shmem_longlong_cswap(long long * target, long long cond, 
                     long long value, int pe)
{
    long long newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_cswap(target, &value, &newval, &cond, sizeof(long long), pe, DTYPE_LONG_LONG);
    shmem_internal_get_wait();
    return newval;
}


void
shmem_int_inc(int *target, int pe)
{
    int tmp = 1;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_atomic_small(target, &tmp, sizeof(int), pe, PTL_SUM, DTYPE_INT);
}


void
shmem_long_inc(long *target, int pe)
{
    long tmp = 1;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_atomic_small(target, &tmp, sizeof(long), pe, PTL_SUM, DTYPE_LONG);
}


void
shmem_longlong_inc(long long *target, int pe)
{
    long long tmp = 1;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_atomic_small(target, &tmp, sizeof(long long), pe, PTL_SUM, DTYPE_LONG_LONG);
}


int
shmem_int_finc(int *target, int pe)
{
    int oldval, tmp = 1;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(int), pe, PTL_SUM, DTYPE_INT);
    shmem_internal_get_wait();
    return oldval;
}


long
shmem_long_finc(long *target, int pe)
{
    long oldval, tmp = 1;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(long), pe, PTL_SUM, DTYPE_LONG);
    shmem_internal_get_wait();
    return oldval;
}


long long
shmem_longlong_finc(long long *target, int pe)
{
    long long oldval, tmp = 1;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(long long), pe, PTL_SUM, DTYPE_LONG_LONG);
    shmem_internal_get_wait();
    return oldval;
}


void
shmem_int_add(int *target, int value, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_atomic_small(target, &value, sizeof(int), pe, PTL_SUM, DTYPE_INT);
}


void
shmem_long_add(long *target, long value, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_atomic_small(target, &value, sizeof(long), pe, PTL_SUM, DTYPE_LONG);
}


void
shmem_longlong_add(long long *target, long long value, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_atomic_small(target, &value, sizeof(long long), pe, PTL_SUM, DTYPE_LONG_LONG);
}


int
shmem_int_fadd(int *target, int value, int pe)
{
    int oldval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fetch_atomic(target, &value, &oldval, sizeof(int), pe, PTL_SUM, DTYPE_INT);
    shmem_internal_get_wait();
    return oldval;
}


long
shmem_long_fadd(long *target, long value, int pe)
{
    long oldval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fetch_atomic(target, &value, &oldval, sizeof(long), pe, PTL_SUM, DTYPE_LONG);
    shmem_internal_get_wait();
    return oldval;
}


long long
shmem_longlong_fadd(long long *target, long long value,
                    int pe)
{
    long long oldval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fetch_atomic(target, &value, &oldval, sizeof(long long), pe, PTL_SUM, DTYPE_LONG_LONG);
    shmem_internal_get_wait();
    return oldval;
}
