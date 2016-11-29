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
#include "collectives_c_pshmem.h"

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
    SHMEM_ERR_CHECK_SYMMETRIC(pSync, sizeof(long)*SHMEM_BARRIER_SYNC_SIZE);

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
        SHMEM_ERR_CHECK_NON_NEGATIVE(nreduce);                          \
        SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE)*nreduce);        \
        SHMEM_ERR_CHECK_SYMMETRIC(source, sizeof(TYPE)*nreduce);        \
        SHMEM_ERR_CHECK_SYMMETRIC(pWrk, sizeof(TYPE) *                  \
                                  MAX(nreduce/2 + 1, SHMEM_REDUCE_MIN_WRKDATA_SIZE)); \
        SHMEM_ERR_CHECK_SYMMETRIC(pSync, sizeof(long)*SHMEM_REDUCE_SYNC_SIZE); \
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
    SHMEM_ERR_CHECK_NON_NEGATIVE(nlong);
    SHMEM_ERR_CHECK_SYMMETRIC(target, nlong * 4);
    SHMEM_ERR_CHECK_SYMMETRIC(source, nlong * 4);
    SHMEM_ERR_CHECK_SYMMETRIC(pSync, sizeof(long)*SHMEM_BCAST_SYNC_SIZE);

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
    SHMEM_ERR_CHECK_NON_NEGATIVE(nlong);
    SHMEM_ERR_CHECK_SYMMETRIC(target, nlong * 8);
    SHMEM_ERR_CHECK_SYMMETRIC(source, nlong * 8);
    SHMEM_ERR_CHECK_SYMMETRIC(pSync, sizeof(long)*SHMEM_BCAST_SYNC_SIZE);

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
    SHMEM_ERR_CHECK_NON_NEGATIVE(nlong);
    SHMEM_ERR_CHECK_SYMMETRIC(target, nlong * 4);
    SHMEM_ERR_CHECK_SYMMETRIC(source, nlong * 4);
    SHMEM_ERR_CHECK_SYMMETRIC(pSync, sizeof(long) * SHMEM_COLLECT_SYNC_SIZE);

    shmem_internal_collect(target, source, nlong * 4,
                      PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_collect64(void *target, const void *source, size_t nlong,
                int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);
    SHMEM_ERR_CHECK_NON_NEGATIVE(nlong);
    SHMEM_ERR_CHECK_SYMMETRIC(target, nlong * 8);
    SHMEM_ERR_CHECK_SYMMETRIC(source, nlong * 8);
    SHMEM_ERR_CHECK_SYMMETRIC(pSync, sizeof(long) * SHMEM_COLLECT_SYNC_SIZE);

    shmem_internal_collect(target, source, nlong * 8,
                      PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_fcollect32(void *target, const void *source, size_t nlong,
                 int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);
    SHMEM_ERR_CHECK_NON_NEGATIVE(nlong);
    SHMEM_ERR_CHECK_SYMMETRIC(target, nlong * 4);
    SHMEM_ERR_CHECK_SYMMETRIC(source, nlong * 4);
    SHMEM_ERR_CHECK_SYMMETRIC(pSync, sizeof(long) * SHMEM_COLLECT_SYNC_SIZE);

    shmem_internal_fcollect(target, source, nlong * 4,
                       PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_fcollect64(void *target, const void *source, size_t nlong,
                 int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);
    SHMEM_ERR_CHECK_NON_NEGATIVE(nlong);
    SHMEM_ERR_CHECK_SYMMETRIC(target, nlong * 8);
    SHMEM_ERR_CHECK_SYMMETRIC(source, nlong * 8);
    SHMEM_ERR_CHECK_SYMMETRIC(pSync, sizeof(long) * SHMEM_COLLECT_SYNC_SIZE);

    shmem_internal_fcollect(target, source, nlong * 8,
                       PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_alltoall32(void *dest, const void *source, size_t nelems, int PE_start,
                 int logPE_stride, int PE_size, long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);
    SHMEM_ERR_CHECK_SYMMETRIC(dest, nelems * 4);
    SHMEM_ERR_CHECK_SYMMETRIC(source, nelems * 4);
    SHMEM_ERR_CHECK_SYMMETRIC(pSync, sizeof(long) * SHMEM_ALLTOALL_SYNC_SIZE);

    shmem_internal_alltoall(dest, source, nelems * 4,
                            PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_alltoall64(void *dest, const void *source, size_t nelems, int PE_start,
                 int logPE_stride, int PE_size, long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);
    SHMEM_ERR_CHECK_SYMMETRIC(dest, nelems * 8);
    SHMEM_ERR_CHECK_SYMMETRIC(source, nelems * 8);
    SHMEM_ERR_CHECK_SYMMETRIC(pSync, sizeof(long) * SHMEM_ALLTOALL_SYNC_SIZE);

    shmem_internal_alltoall(dest, source, nelems * 8,
                            PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_alltoalls32(void *dest, const void *source, ptrdiff_t dst, ptrdiff_t sst,
                  size_t nelems, int PE_start, int logPE_stride, int PE_size,
                  long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_POSITIVE(sst);
    SHMEM_ERR_CHECK_POSITIVE(dst);
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);
    SHMEM_ERR_CHECK_SYMMETRIC(dest, 4 * ((nelems-1) * dst + 1));
    SHMEM_ERR_CHECK_SYMMETRIC(source, 4 * ((nelems-1) * sst + 1));
    SHMEM_ERR_CHECK_SYMMETRIC(pSync, sizeof(long) * SHMEM_ALLTOALL_SYNC_SIZE);

    shmem_internal_alltoalls(dest, source, dst, sst, 4, nelems, PE_start,
                             logPE_stride, PE_size, pSync);
}


void
shmem_alltoalls64(void *dest, const void *source, ptrdiff_t dst, ptrdiff_t sst,
                  size_t nelems, int PE_start, int logPE_stride, int PE_size,
                  long *pSync)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_POSITIVE(sst);
    SHMEM_ERR_CHECK_POSITIVE(dst);
    SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size);
    SHMEM_ERR_CHECK_SYMMETRIC(dest, 8 * ((nelems-1) * dst + 1));
    SHMEM_ERR_CHECK_SYMMETRIC(source, 8 * ((nelems-1) * sst + 1));
    SHMEM_ERR_CHECK_SYMMETRIC(pSync, sizeof(long) * SHMEM_ALLTOALL_SYNC_SIZE);
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);

    shmem_internal_alltoalls(dest, source, dst, sst, 8, nelems, PE_start,
                             logPE_stride, PE_size, pSync);
}
