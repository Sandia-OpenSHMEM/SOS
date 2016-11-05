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

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"
#include "shmem_collectives.h"

#ifdef ENABLE_PROFILING
#include "pshmem.h"

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

#pragma weak shmem_alltoall32 = pshmem_alltoall32
#define shmem_alltoall32 pshmem_alltoall32

#pragma weak shmem_alltoall64 = pshmem_alltoall64
#define shmem_alltoall64 pshmem_alltoall64

#pragma weak shmem_alltoalls32 = pshmem_alltoalls32
#define shmem_alltoalls32 pshmem_alltoalls32

#pragma weak shmem_alltoalls64 = pshmem_alltoalls64
#define shmem_alltoalls64 pshmem_alltoalls64

#endif /* ENABLE_PROFILING */


void
shmem_barrier_all(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_barrier_all();
}


void
shmem_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);

    shmem_internal_barrier(PE_start, logPE_stride, PE_size, pSync);
}


#define SHMEM_DEF_TO_ALL(STYPE,TYPE,ITYPE,SOP,IOP)                      \
    void shmem_##STYPE##_##SOP##_to_all(TYPE *target,                   \
                                       const TYPE *source, int nreduce, \
                                       int PE_start, int logPE_stride,  \
                                       int PE_size, TYPE *pWrk,         \
                                       long *pSync)                     \
    {                                                                   \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);    \
                                                                        \
        shmem_internal_op_to_all(target, source, nreduce, sizeof(TYPE), \
                                 PE_start, logPE_stride, PE_size,       \
                                 pWrk, pSync, IOP, ITYPE);              \
    }

SHMEM_BIND_C_INTS_OP(SHMEM_DEF_TO_ALL, and, SHM_INTERNAL_BAND)
SHMEM_BIND_C_INTS_OP(SHMEM_DEF_TO_ALL, or, SHM_INTERNAL_BOR)
SHMEM_BIND_C_INTS_OP(SHMEM_DEF_TO_ALL, xor, SHM_INTERNAL_BXOR)

SHMEM_BIND_C_INTS_OP(SHMEM_DEF_TO_ALL, min, SHM_INTERNAL_MIN)
SHMEM_BIND_C_FLOATS_OP(SHMEM_DEF_TO_ALL, min, SHM_INTERNAL_MIN)
SHMEM_BIND_C_INTS_OP(SHMEM_DEF_TO_ALL, max, SHM_INTERNAL_MAX)
SHMEM_BIND_C_FLOATS_OP(SHMEM_DEF_TO_ALL, max, SHM_INTERNAL_MAX)

SHMEM_BIND_C_INTS_OP(SHMEM_DEF_TO_ALL, sum, SHM_INTERNAL_SUM)
SHMEM_BIND_C_FLOATS_OP(SHMEM_DEF_TO_ALL, sum, SHM_INTERNAL_SUM)
SHMEM_BIND_C_CMPLX_OP(SHMEM_DEF_TO_ALL, sum, SHM_INTERNAL_SUM)
SHMEM_BIND_C_INTS_OP(SHMEM_DEF_TO_ALL, prod, SHM_INTERNAL_PROD)
SHMEM_BIND_C_FLOATS_OP(SHMEM_DEF_TO_ALL, prod, SHM_INTERNAL_PROD)
SHMEM_BIND_C_CMPLX_OP(SHMEM_DEF_TO_ALL, prod, SHM_INTERNAL_PROD)

void
shmem_broadcast32(void *target, const void *source, size_t nlong, 
                  int PE_root, int PE_start, int logPE_stride, int PE_size,
                  long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(PE_root);
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);

    shmem_internal_bcast(target, source, nlong * 4,
                         PE_root, PE_start, logPE_stride, PE_size,
                         pSync, 1);
}


void
shmem_broadcast64(void *target, const void *source, size_t nlong,
                  int PE_root, int PE_start, int logPE_stride, int PE_size,
                  long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(PE_root);
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);

    shmem_internal_bcast(target, source, nlong * 8,
                         PE_root, PE_start, logPE_stride, PE_size,
                         pSync, 1);
}


void
shmem_collect32(void *target, const void *source, size_t nlong,
                int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);

    shmem_internal_collect(target, source, nlong * 4,
                      PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_collect64(void *target, const void *source, size_t nlong,
                int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);

    shmem_internal_collect(target, source, nlong * 8,
                      PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_fcollect32(void *target, const void *source, size_t nlong,
                 int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);

    shmem_internal_fcollect(target, source, nlong * 4,
                       PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_fcollect64(void *target, const void *source, size_t nlong,
                 int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);

    shmem_internal_fcollect(target, source, nlong * 8,
                       PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_alltoall32(void *dest, const void *source, size_t nelems, int PE_start,
                 int logPE_stride, int PE_size, long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);

    shmem_internal_alltoall(dest, source, nelems * 4,
                            PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_alltoall64(void *dest, const void *source, size_t nelems, int PE_start,
                 int logPE_stride, int PE_size, long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);

    shmem_internal_alltoall(dest, source, nelems * 8,
                            PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_alltoalls32(void *dest, const void *source, ptrdiff_t dst, ptrdiff_t sst,
                  size_t nelems, int PE_start, int logPE_stride, int PE_size,
                  long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ARG_POSITIVE(sst);
    SHMEM_ERR_CHECK_ARG_POSITIVE(dst);
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);

    shmem_internal_alltoalls(dest, source, dst, sst, 4, nelems, PE_start,
                             logPE_stride, PE_size, pSync);
}


void
shmem_alltoalls64(void *dest, const void *source, ptrdiff_t dst, ptrdiff_t sst,
                  size_t nelems, int PE_start, int logPE_stride, int PE_size,
                  long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ARG_POSITIVE(sst);
    SHMEM_ERR_CHECK_ARG_POSITIVE(dst);
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);

    shmem_internal_alltoalls(dest, source, dst, sst, 8, nelems, PE_start,
                             logPE_stride, PE_size, pSync);
}
