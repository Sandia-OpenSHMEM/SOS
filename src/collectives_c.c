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
#include <strings.h>
#include <string.h>

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"
#include "shmem_collectives.h"

#ifdef ENABLE_PROFILING

#pragma weak shmem_barrier_all = pshmem_barrier_all
#define shmem_barrier_all pshmem_barrier_all

#pragma weak shmem_barrier = pshmem_barrier
#define shmem_barrier pshmem_barrier

#pragma weak shmem_int_and_to_all = pshmem_int_and_to_all
#define shmem_int_and_to_all pshmem_int_and_to_all

#pragma weak shmem_long_and_to_all = pshmem_long_and_to_all
#define shmem_long_and_to_all pshmem_long_and_to_all

#pragma weak shmem_longlong_and_to_all = pshmem_longlong_and_to_all
#define shmem_longlong_and_to_all pshmem_longlong_and_to_all

#pragma weak shmem_short_and_to_all = pshmem_short_and_to_all
#define shmem_short_and_to_all pshmem_short_and_to_all

#pragma weak shmem_int_or_to_all = pshmem_int_or_to_all
#define shmem_int_or_to_all pshmem_int_or_to_all

#pragma weak shmem_long_or_to_all = pshmem_long_or_to_all
#define shmem_long_or_to_all pshmem_long_or_to_all

#pragma weak shmem_longlong_or_to_all = pshmem_longlong_or_to_all
#define shmem_longlong_or_to_all pshmem_longlong_or_to_all

#pragma weak shmem_short_or_to_all = pshmem_short_or_to_all
#define shmem_short_or_to_all pshmem_short_or_to_all

#pragma weak shmem_int_xor_to_all = pshmem_int_xor_to_all
#define shmem_int_xor_to_all pshmem_int_xor_to_all

#pragma weak shmem_long_xor_to_all = pshmem_long_xor_to_all
#define shmem_long_xor_to_all pshmem_long_xor_to_all

#pragma weak shmem_longlong_xor_to_all = pshmem_longlong_xor_to_all
#define shmem_longlong_xor_to_all pshmem_longlong_xor_to_all

#pragma weak shmem_short_xor_to_all = pshmem_short_xor_to_all
#define shmem_short_xor_to_all pshmem_short_xor_to_all

#pragma weak shmem_int_min_to_all = pshmem_int_min_to_all
#define shmem_int_min_to_all pshmem_int_min_to_all

#pragma weak shmem_long_min_to_all = pshmem_long_min_to_all
#define shmem_long_min_to_all pshmem_long_min_to_all

#pragma weak shmem_longlong_min_to_all = pshmem_longlong_min_to_all
#define shmem_longlong_min_to_all pshmem_longlong_min_to_all

#pragma weak shmem_short_min_to_all = pshmem_short_min_to_all
#define shmem_short_min_to_all pshmem_short_min_to_all

#pragma weak shmem_longdouble_min_to_all = pshmem_longdouble_min_to_all
#define shmem_longdouble_min_to_all pshmem_longdouble_min_to_all

#pragma weak shmem_float_min_to_all = pshmem_float_min_to_all
#define shmem_float_min_to_all pshmem_float_min_to_all

#pragma weak shmem_double_min_to_all = pshmem_double_min_to_all
#define shmem_double_min_to_all pshmem_double_min_to_all

#pragma weak shmem_int_max_to_all = pshmem_int_max_to_all
#define shmem_int_max_to_all pshmem_int_max_to_all

#pragma weak shmem_long_max_to_all = pshmem_long_max_to_all
#define shmem_long_max_to_all pshmem_long_max_to_all

#pragma weak shmem_longlong_max_to_all = pshmem_longlong_max_to_all
#define shmem_longlong_max_to_all pshmem_longlong_max_to_all

#pragma weak shmem_short_max_to_all = pshmem_short_max_to_all
#define shmem_short_max_to_all pshmem_short_max_to_all

#pragma weak shmem_longdouble_max_to_all = pshmem_longdouble_max_to_all
#define shmem_longdouble_max_to_all pshmem_longdouble_max_to_all

#pragma weak shmem_float_max_to_all = pshmem_float_max_to_all
#define shmem_float_max_to_all pshmem_float_max_to_all

#pragma weak shmem_double_max_to_all = pshmem_double_max_to_all
#define shmem_double_max_to_all pshmem_double_max_to_all

#pragma weak shmem_complexd_sum_to_all = pshmem_complexd_sum_to_all
#define shmem_complexd_sum_to_all pshmem_complexd_sum_to_all

#pragma weak shmem_complexf_sum_to_all = pshmem_complexf_sum_to_all
#define shmem_complexf_sum_to_all pshmem_complexf_sum_to_all

#pragma weak shmem_double_sum_to_all = pshmem_double_sum_to_all
#define shmem_double_sum_to_all pshmem_double_sum_to_all

#pragma weak shmem_float_sum_to_all = pshmem_float_sum_to_all
#define shmem_float_sum_to_all pshmem_float_sum_to_all

#pragma weak shmem_int_sum_to_all = pshmem_int_sum_to_all
#define shmem_int_sum_to_all pshmem_int_sum_to_all

#pragma weak shmem_long_sum_to_all = pshmem_long_sum_to_all
#define shmem_long_sum_to_all pshmem_long_sum_to_all

#pragma weak shmem_longdouble_sum_to_all = pshmem_longdouble_sum_to_all
#define shmem_longdouble_sum_to_all pshmem_longdouble_sum_to_all

#pragma weak shmem_longlong_sum_to_all = pshmem_longlong_sum_to_all
#define shmem_longlong_sum_to_all pshmem_longlong_sum_to_all

#pragma weak shmem_short_sum_to_all = pshmem_short_sum_to_all
#define shmem_short_sum_to_all pshmem_short_sum_to_all

#pragma weak shmem_complexd_prod_to_all = pshmem_complexd_prod_to_all
#define shmem_complexd_prod_to_all pshmem_complexd_prod_to_all

#pragma weak shmem_complexf_prod_to_all = pshmem_complexf_prod_to_all
#define shmem_complexf_prod_to_all pshmem_complexf_prod_to_all

#pragma weak shmem_double_prod_to_all = pshmem_double_prod_to_all
#define shmem_double_prod_to_all pshmem_double_prod_to_all

#pragma weak shmem_float_prod_to_all = pshmem_float_prod_to_all
#define shmem_float_prod_to_all pshmem_float_prod_to_all

#pragma weak shmem_int_prod_to_all = pshmem_int_prod_to_all
#define shmem_int_prod_to_all pshmem_int_prod_to_all

#pragma weak shmem_long_prod_to_all = pshmem_long_prod_to_all
#define shmem_long_prod_to_all pshmem_long_prod_to_all

#pragma weak shmem_longdouble_prod_to_all = pshmem_longdouble_prod_to_all
#define shmem_longdouble_prod_to_all pshmem_longdouble_prod_to_all

#pragma weak shmem_longlong_prod_to_all = pshmem_longlong_prod_to_all
#define shmem_longlong_prod_to_all pshmem_longlong_prod_to_all

#pragma weak shmem_short_prod_to_all = pshmem_short_prod_to_all
#define shmem_short_prod_to_all pshmem_short_prod_to_all

#pragma weak shmem_broadcast32 = pshmem_broadcast32
#define shmem_broadcast32 pshmem_broadcast32

#pragma weak shmem_broadcast64 = pshmem_broadcast64
#define shmem_broadcast64 pshmem_broadcast64

#pragma weak shmem_collect32 = pshmem_collect32
#define shmem_collect32 pshmem_collect32

#pragma weak shmem_collect64 = pshmem_collect64
#define shmem_collect64 pshmem_collect64

#pragma weak shmem_fcollect32 = pshmem_fcollect32
#define shmem_fcollect32 pshmem_fcollect32

#pragma weak shmem_fcollect64 = pshmem_fcollect64
#define shmem_fcollect64 pshmem_fcollect64

#endif /* ENABLE_PROFILING */


void
shmem_barrier_all(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_barrier_all();
}


void
shmem_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_barrier(PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_short_and_to_all(short *target, short *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size,
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BAND, DTYPE_SHORT);
}


void
shmem_int_and_to_all(int *target, int *source, int nreduce,
                     int PE_start, int logPE_stride, int PE_size,
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BAND, DTYPE_INT);
}


void
shmem_long_and_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size,
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BAND, DTYPE_LONG);
}


void
shmem_longlong_and_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size,
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BAND, DTYPE_LONG);
}


void
shmem_short_or_to_all(short *target, short *source, int nreduce, 
                      int PE_start, int logPE_stride, int PE_size, 
                      short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BOR, DTYPE_SHORT);
}


void
shmem_int_or_to_all(int *target, int *source, int nreduce, 
                    int PE_start, int logPE_stride, int PE_size, 
                    int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BOR, DTYPE_INT);
}


void
shmem_long_or_to_all(long *target, long *source, int nreduce,
                     int PE_start, int logPE_stride, int PE_size, 
                     long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BOR, DTYPE_LONG);
}


void
shmem_longlong_or_to_all(long long *target, long long *source, int nreduce,
                         int PE_start, int logPE_stride, int PE_size, 
                         long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BOR, DTYPE_LONG);
}


void
shmem_short_xor_to_all(short *target, short *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size, 
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BXOR, DTYPE_SHORT);
}

void
shmem_int_xor_to_all(int *target, int *source, int nreduce, 
                     int PE_start, int logPE_stride, int PE_size, 
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BXOR, DTYPE_INT);
}


void
shmem_long_xor_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BXOR, DTYPE_LONG);
}


void
shmem_longlong_xor_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size, 
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BXOR, DTYPE_LONG);
}


void
shmem_float_min_to_all(float *target, float *source, int nreduce, 
                       int PE_start, int logPE_stride, int PE_size, 
                       float *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_FLOAT);
}


void
shmem_double_min_to_all(double *target, double *source, int nreduce,
                        int PE_start, int logPE_stride, int PE_size, 
                        double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_DOUBLE);
}


void
shmem_longdouble_min_to_all(long double *target, long double *source, int nreduce, 
                            int PE_start, int logPE_stride, int PE_size,
                            long double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_LONG_DOUBLE);
}


void
shmem_short_min_to_all(short *target, short *source, int nreduce, 
                       int PE_start, int logPE_stride, int PE_size, 
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, DTYPE_SHORT);
}


void
shmem_int_min_to_all(int *target, int *source, int nreduce, 
                     int PE_start, int logPE_stride, int PE_size, 
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, DTYPE_INT);
}


void
shmem_long_min_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, DTYPE_LONG);
}


void
shmem_longlong_min_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size,
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, DTYPE_LONG);
}


void
shmem_float_max_to_all(float *target, float *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size,
                       float *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_FLOAT);
}


void
shmem_double_max_to_all(double *target, double *source, int nreduce,
                        int PE_start, int logPE_stride, int PE_size,
                        double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_DOUBLE);
}


void
shmem_longdouble_max_to_all(long double *target, long double *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size,
                            long double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_LONG_DOUBLE);
}


void
shmem_short_max_to_all(short *target, short *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size,
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, DTYPE_SHORT);
}


void
shmem_int_max_to_all(int *target, int *source, int nreduce,
                     int PE_start, int logPE_stride, int PE_size,
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, DTYPE_INT);
}


void
shmem_long_max_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size,
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, DTYPE_LONG);
}


void 
shmem_longlong_max_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size,
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, DTYPE_LONG);
}


void
shmem_float_sum_to_all(float *target, float *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size, 
                       float *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_FLOAT);
}


void
shmem_double_sum_to_all(double *target, double *source, int nreduce, 
                        int PE_start, int logPE_stride, int PE_size, 
                        double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_DOUBLE);
}


void
shmem_longdouble_sum_to_all(long double *target, long double *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size,
                            long double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_LONG_DOUBLE);
}


void
shmem_complexf_sum_to_all(float complex *target, float complex *source, int nreduce, 
                          int PE_start, int logPE_stride, int PE_size,
                          float complex *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float complex),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_FLOAT_COMPLEX);
}


void 
shmem_complexd_sum_to_all(double complex *target, double complex *source, int nreduce, 
                          int PE_start, int logPE_stride, int PE_size, 
                          double complex *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double complex),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_DOUBLE_COMPLEX);
}


void
shmem_short_sum_to_all(short *target, short *source, int nreduce, 
                       int PE_start, int logPE_stride, int PE_size, 
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, DTYPE_SHORT);
}


void
shmem_int_sum_to_all(int *target, int *source, int nreduce, 
                     int PE_start, int logPE_stride, int PE_size, 
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, DTYPE_INT);
}


void
shmem_long_sum_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, DTYPE_LONG);
}


void
shmem_longlong_sum_to_all(long long *target, long long *source, int nreduce, 
                          int PE_start, int logPE_stride, int PE_size, 
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, DTYPE_LONG);
}


void
shmem_float_prod_to_all(float *target, float *source, int nreduce, 
                        int PE_start, int logPE_stride, int PE_size, 
                        float *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_FLOAT);
}


void
shmem_double_prod_to_all(double *target, double *source, int nreduce, 
                         int PE_start, int logPE_stride, int PE_size,
                         double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_DOUBLE);
}


void
shmem_longdouble_prod_to_all(long double *target, long double *source, int nreduce, 
                             int PE_start, int logPE_stride, int PE_size,
                             long double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_LONG_DOUBLE);
}


void
shmem_complexf_prod_to_all(float complex *target, float complex *source, int nreduce, 
                           int PE_start, int logPE_stride, int PE_size,
                           float complex *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float complex),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_FLOAT_COMPLEX);
}


void
shmem_complexd_prod_to_all(double complex *target, double complex *source, int nreduce, 
                           int PE_start, int logPE_stride, int PE_size, 
                           double complex *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double complex),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_DOUBLE_COMPLEX);
}


void
shmem_short_prod_to_all(short *target, short *source, int nreduce, 
                        int PE_start, int logPE_stride, int PE_size, 
                        short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, DTYPE_SHORT);
}


void
shmem_int_prod_to_all(int *target, int *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, DTYPE_INT);
}


void
shmem_long_prod_to_all(long *target, long *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size, 
                       long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, DTYPE_LONG);
}


void
shmem_longlong_prod_to_all(long long *target, long long *source, int nreduce, 
                           int PE_start, int logPE_stride, int PE_size,
                           long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, DTYPE_LONG);
}


void
shmem_broadcast32(void *target, const void *source, size_t nlong, 
                  int PE_root, int PE_start, int logPE_stride, int PE_size,
                  long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_bcast(target, source, nlong * 4,
                         PE_root, PE_start, logPE_stride, PE_size,
                         pSync, 1);
}


void
shmem_broadcast64(void *target, const void *source, size_t nlong,
                  int PE_root, int PE_start, int logPE_stride, int PE_size,
                  long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_bcast(target, source, nlong * 8,
                         PE_root, PE_start, logPE_stride, PE_size,
                         pSync, 1);
}


void
shmem_collect32(void *target, const void *source, size_t nlong,
                int PE_start, int logPE_stride, int PE_size, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_collect(target, source, nlong * 4,
                      PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_collect64(void *target, const void *source, size_t nlong,
                int PE_start, int logPE_stride, int PE_size, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_collect(target, source, nlong * 8,
                      PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_fcollect32(void *target, const void *source, size_t nlong,
                 int PE_start, int logPE_stride, int PE_size, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fcollect(target, source, nlong * 4,
                       PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_fcollect64(void *target, const void *source, size_t nlong,
                 int PE_start, int logPE_stride, int PE_size, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fcollect(target, source, nlong * 8,
                       PE_start, logPE_stride, PE_size, pSync);
}

