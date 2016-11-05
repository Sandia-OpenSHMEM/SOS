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
#include <strings.h>
#include <string.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"
#include "shmem_collectives.h"
#include "shmem_synchronization.h"


#define FC_SHMEM_BARRIER_ALL FC_FUNC_(shmem_barrier_all, SHMEM_BARRIER_ALL)
void FC_SHMEM_BARRIER_ALL(void);
void
FC_SHMEM_BARRIER_ALL(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_barrier_all();
}


#define FC_SHMEM_BARRIER FC_FUNC_(shmem_barrier, SHMEM_BARRIER)
void FC_SHMEM_BARRIER(fortran_integer_t *PE_start,
                      fortran_integer_t *logPE_stride,
                      fortran_integer_t *PE_size,
                      fortran_integer_t *pSync);
void
FC_SHMEM_BARRIER(fortran_integer_t *PE_start,
                 fortran_integer_t *logPE_stride,
                 fortran_integer_t *PE_size,
                 fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_BARRIER_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_barrier(*PE_start, *logPE_stride, *PE_size, pSync_c);
}


#define FC_SHMEM_INT4_AND_TO_ALL FC_FUNC_(shmem_int4_and_to_all, SHMEM_INT4_AND_TO_ALL)
void FC_SHMEM_INT4_AND_TO_ALL(int32_t *target, 
                              int32_t *source,
                              fortran_integer_t *nreduce,
                              fortran_integer_t *PE_start,
                              fortran_integer_t *logPE_stride,
                              fortran_integer_t *PE_size,
                              int32_t *pWrk,
                              fortran_integer_t *pSync);
void
FC_SHMEM_INT4_AND_TO_ALL(int32_t *target, 
                         int32_t *source,
                         fortran_integer_t *nreduce,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         int32_t *pWrk,
                         fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 4,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_BAND, SHM_INTERNAL_INT32);
}


#define FC_SHMEM_INT8_AND_TO_ALL FC_FUNC_(shmem_int8_and_to_all, SHMEM_INT8_AND_TO_ALL)
void FC_SHMEM_INT8_AND_TO_ALL(int64_t *target, 
                              int64_t *source,
                              fortran_integer_t *nreduce,
                              fortran_integer_t *PE_start,
                              fortran_integer_t *logPE_stride,
                              fortran_integer_t *PE_size,
                              int64_t *pWrk,
                              fortran_integer_t *pSync);
void
FC_SHMEM_INT8_AND_TO_ALL(int64_t *target, 
                         int64_t *source,
                         fortran_integer_t *nreduce,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         int64_t *pWrk,
                         fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8, 
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_BAND, SHM_INTERNAL_INT64);
}


#define FC_SHMEM_INT4_OR_TO_ALL FC_FUNC_(shmem_int4_or_to_all, SHMEM_INT4_OR_TO_ALL)
void FC_SHMEM_INT4_OR_TO_ALL(int32_t *target, 
                              int32_t *source,
                              fortran_integer_t *nreduce,
                              fortran_integer_t *PE_start,
                              fortran_integer_t *logPE_stride,
                              fortran_integer_t *PE_size,
                              int32_t *pWrk,
                              fortran_integer_t *pSync);
void
FC_SHMEM_INT4_OR_TO_ALL(int32_t *target, 
                         int32_t *source,
                         fortran_integer_t *nreduce,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         int32_t *pWrk,
                         fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 4,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_BOR, SHM_INTERNAL_INT32);
}


#define FC_SHMEM_INT8_OR_TO_ALL FC_FUNC_(shmem_int8_or_to_all, SHMEM_INT8_OR_TO_ALL)
void FC_SHMEM_INT8_OR_TO_ALL(int64_t *target, 
                              int64_t *source,
                              fortran_integer_t *nreduce,
                              fortran_integer_t *PE_start,
                              fortran_integer_t *logPE_stride,
                              fortran_integer_t *PE_size,
                              int64_t *pWrk,
                              fortran_integer_t *pSync);
void
FC_SHMEM_INT8_OR_TO_ALL(int64_t *target, 
                         int64_t *source,
                         fortran_integer_t *nreduce,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         int64_t *pWrk,
                         fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_BOR, SHM_INTERNAL_INT64);
}


#define FC_SHMEM_INT4_XOR_TO_ALL FC_FUNC_(shmem_int4_xor_to_all, SHMEM_INT4_XOR_TO_ALL)
void FC_SHMEM_INT4_XOR_TO_ALL(int32_t *target, 
                              int32_t *source,
                              fortran_integer_t *nreduce,
                              fortran_integer_t *PE_start,
                              fortran_integer_t *logPE_stride,
                              fortran_integer_t *PE_size,
                              int32_t *pWrk,
                              fortran_integer_t *pSync);
void
FC_SHMEM_INT4_XOR_TO_ALL(int32_t *target, 
                         int32_t *source,
                         fortran_integer_t *nreduce,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         int32_t *pWrk,
                         fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 4,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_BXOR, SHM_INTERNAL_INT32);
}


#define FC_SHMEM_INT8_XOR_TO_ALL FC_FUNC_(shmem_int8_xor_to_all, SHMEM_INT8_XOR_TO_ALL)
void FC_SHMEM_INT8_XOR_TO_ALL(int64_t *target, 
                              int64_t *source,
                              fortran_integer_t *nreduce,
                              fortran_integer_t *PE_start,
                              fortran_integer_t *logPE_stride,
                              fortran_integer_t *PE_size,
                              int64_t *pWrk,
                              fortran_integer_t *pSync);
void
FC_SHMEM_INT8_XOR_TO_ALL(int64_t *target, 
                         int64_t *source,
                         fortran_integer_t *nreduce,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         int64_t *pWrk,
                         fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_BXOR, SHM_INTERNAL_INT64);
}


/* BWB: FIX ME: For xor_to_all, OpenSHMEM 1.0 references COMP4 and
   COMP8 and the SGI man pages also references REAL4 and REAL8.  Cray
   provides neither.  I don't know what an XOR of a floating point
   means, so I'm not adding them. */


#define FC_SHMEM_INT4_MAX_TO_ALL FC_FUNC_(shmem_int4_max_to_all, SHMEM_INT4_MAX_TO_ALL)
void FC_SHMEM_INT4_MAX_TO_ALL(int32_t *target, 
                              int32_t *source,
                              fortran_integer_t *nreduce,
                              fortran_integer_t *PE_start,
                              fortran_integer_t *logPE_stride,
                              fortran_integer_t *PE_size,
                              int32_t *pWrk,
                              fortran_integer_t *pSync);
void
FC_SHMEM_INT4_MAX_TO_ALL(int32_t *target, 
                         int32_t *source,
                         fortran_integer_t *nreduce,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         int32_t *pWrk,
                         fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 4,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_MAX, SHM_INTERNAL_INT32);
}


#define FC_SHMEM_INT8_MAX_TO_ALL FC_FUNC_(shmem_int8_max_to_all, SHMEM_INT8_MAX_TO_ALL)
void FC_SHMEM_INT8_MAX_TO_ALL(int64_t *target, 
                              int64_t *source,
                              fortran_integer_t *nreduce,
                              fortran_integer_t *PE_start,
                              fortran_integer_t *logPE_stride,
                              fortran_integer_t *PE_size,
                              int64_t *pWrk,
                              fortran_integer_t *pSync);
void
FC_SHMEM_INT8_MAX_TO_ALL(int64_t *target, 
                         int64_t *source,
                         fortran_integer_t *nreduce,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         int64_t *pWrk,
                         fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_MAX, SHM_INTERNAL_INT64);
}


#define FC_SHMEM_REAL4_MAX_TO_ALL FC_FUNC_(shmem_real4_max_to_all, SHMEM_REAL4_MAX_TO_ALL)
void FC_SHMEM_REAL4_MAX_TO_ALL(float *target, 
                               float *source,
                               fortran_integer_t *nreduce,
                               fortran_integer_t *PE_start,
                               fortran_integer_t *logPE_stride,
                               fortran_integer_t *PE_size,
                               float *pWrk,
                               fortran_integer_t *pSync);
void
FC_SHMEM_REAL4_MAX_TO_ALL(float *target, 
                          float *source,
                          fortran_integer_t *nreduce,
                          fortran_integer_t *PE_start,
                          fortran_integer_t *logPE_stride,
                          fortran_integer_t *PE_size,
                          float *pWrk,
                          fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    shmem_internal_assert(sizeof(float) == 4);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 4,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_MAX, SHM_INTERNAL_FLOAT);
}


#define FC_SHMEM_REAL8_MAX_TO_ALL FC_FUNC_(shmem_real8_max_to_all, SHMEM_REAL8_MAX_TO_ALL)
void FC_SHMEM_REAL8_MAX_TO_ALL(double *target, 
                               double *source,
                               fortran_integer_t *nreduce,
                               fortran_integer_t *PE_start,
                               fortran_integer_t *logPE_stride,
                               fortran_integer_t *PE_size,
                               double *pWrk,
                               fortran_integer_t *pSync);
void
FC_SHMEM_REAL8_MAX_TO_ALL(double *target, 
                          double *source,
                          fortran_integer_t *nreduce,
                          fortran_integer_t *PE_start,
                          fortran_integer_t *logPE_stride,
                          fortran_integer_t *PE_size,
                          double *pWrk,
                          fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    shmem_internal_assert(sizeof(double) == 8);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_MAX, SHM_INTERNAL_DOUBLE);
}


#if SIZEOF_LONG_DOUBLE == 16
#define FC_SHMEM_REAL16_MAX_TO_ALL FC_FUNC_(shmem_real16_max_to_all, SHMEM_REAL16_MAX_TO_ALL)
void FC_SHMEM_REAL16_MAX_TO_ALL(long double *target, 
                                long double *source,
                                fortran_integer_t *nreduce,
                                fortran_integer_t *PE_start,
                                fortran_integer_t *logPE_stride,
                                fortran_integer_t *PE_size,
                                long double *pWrk,
                                fortran_integer_t *pSync);
void
FC_SHMEM_REAL16_MAX_TO_ALL(long double *target, 
                           long double *source,
                           fortran_integer_t *nreduce,
                           fortran_integer_t *PE_start,
                           fortran_integer_t *logPE_stride,
                           fortran_integer_t *PE_size,
                           long double *pWrk,
                           fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_MAX, SHM_INTERNAL_LONG_DOUBLE);
}
#endif


#define FC_SHMEM_INT4_MIN_TO_ALL FC_FUNC_(shmem_int4_min_to_all, SHMEM_INT4_MIN_TO_ALL)
void FC_SHMEM_INT4_MIN_TO_ALL(int32_t *target, 
                              int32_t *source,
                              fortran_integer_t *nreduce,
                              fortran_integer_t *PE_start,
                              fortran_integer_t *logPE_stride,
                              fortran_integer_t *PE_size,
                              int32_t *pWrk,
                              fortran_integer_t *pSync);
void
FC_SHMEM_INT4_MIN_TO_ALL(int32_t *target, 
                         int32_t *source,
                         fortran_integer_t *nreduce,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         int32_t *pWrk,
                         fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 4,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_MIN, SHM_INTERNAL_INT32);
}


#define FC_SHMEM_INT8_MIN_TO_ALL FC_FUNC_(shmem_int8_min_to_all, SHMEM_INT8_MIN_TO_ALL)
void FC_SHMEM_INT8_MIN_TO_ALL(int64_t *target, 
                              int64_t *source,
                              fortran_integer_t *nreduce,
                              fortran_integer_t *PE_start,
                              fortran_integer_t *logPE_stride,
                              fortran_integer_t *PE_size,
                              int64_t *pWrk,
                              fortran_integer_t *pSync);
void
FC_SHMEM_INT8_MIN_TO_ALL(int64_t *target, 
                         int64_t *source,
                         fortran_integer_t *nreduce,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         int64_t *pWrk,
                         fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_MIN, SHM_INTERNAL_INT64);
}


#define FC_SHMEM_REAL4_MIN_TO_ALL FC_FUNC_(shmem_real4_min_to_all, SHMEM_REAL4_MIN_TO_ALL)
void FC_SHMEM_REAL4_MIN_TO_ALL(float *target, 
                               float *source,
                               fortran_integer_t *nreduce,
                               fortran_integer_t *PE_start,
                               fortran_integer_t *logPE_stride,
                               fortran_integer_t *PE_size,
                               float *pWrk,
                               fortran_integer_t *pSync);
void
FC_SHMEM_REAL4_MIN_TO_ALL(float *target, 
                          float *source,
                          fortran_integer_t *nreduce,
                          fortran_integer_t *PE_start,
                          fortran_integer_t *logPE_stride,
                          fortran_integer_t *PE_size,
                          float *pWrk,
                          fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    shmem_internal_assert(sizeof(float) == 4);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 4,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_MIN, SHM_INTERNAL_FLOAT);
}


#define FC_SHMEM_REAL8_MIN_TO_ALL FC_FUNC_(shmem_real8_min_to_all, SHMEM_REAL8_MIN_TO_ALL)
void FC_SHMEM_REAL8_MIN_TO_ALL(double *target, 
                               double *source,
                               fortran_integer_t *nreduce,
                               fortran_integer_t *PE_start,
                               fortran_integer_t *logPE_stride,
                               fortran_integer_t *PE_size,
                               double *pWrk,
                               fortran_integer_t *pSync);
void
FC_SHMEM_REAL8_MIN_TO_ALL(double *target, 
                          double *source,
                          fortran_integer_t *nreduce,
                          fortran_integer_t *PE_start,
                          fortran_integer_t *logPE_stride,
                          fortran_integer_t *PE_size,
                          double *pWrk,
                          fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    shmem_internal_assert(sizeof(double) == 8);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_MIN, SHM_INTERNAL_DOUBLE);
}


#if SIZEOF_LONG_DOUBLE == 16
#define FC_SHMEM_REAL16_MIN_TO_ALL FC_FUNC_(shmem_real16_min_to_all, SHMEM_REAL16_MIN_TO_ALL)
void FC_SHMEM_REAL16_MIN_TO_ALL(long double *target, 
                                long double *source,
                                fortran_integer_t *nreduce,
                                fortran_integer_t *PE_start,
                                fortran_integer_t *logPE_stride,
                                fortran_integer_t *PE_size,
                                long double *pWrk,
                                fortran_integer_t *pSync);
void
FC_SHMEM_REAL16_MIN_TO_ALL(long double *target, 
                           long double *source,
                           fortran_integer_t *nreduce,
                           fortran_integer_t *PE_start,
                           fortran_integer_t *logPE_stride,
                           fortran_integer_t *PE_size,
                           long double *pWrk,
                           fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_MIN, SHM_INTERNAL_LONG_DOUBLE);
}
#endif


#define FC_SHMEM_INT4_SUM_TO_ALL FC_FUNC_(shmem_int4_sum_to_all, SHMEM_INT4_SUM_TO_ALL)
void FC_SHMEM_INT4_SUM_TO_ALL(int32_t *target, 
                              int32_t *source,
                              fortran_integer_t *nreduce,
                              fortran_integer_t *PE_start,
                              fortran_integer_t *logPE_stride,
                              fortran_integer_t *PE_size,
                              int32_t *pWrk,
                              fortran_integer_t *pSync);
void
FC_SHMEM_INT4_SUM_TO_ALL(int32_t *target, 
                         int32_t *source,
                         fortran_integer_t *nreduce,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         int32_t *pWrk,
                         fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 4,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_SUM, SHM_INTERNAL_INT32);
}


#define FC_SHMEM_INT8_SUM_TO_ALL FC_FUNC_(shmem_int8_sum_to_all, SHMEM_INT8_SUM_TO_ALL)
void FC_SHMEM_INT8_SUM_TO_ALL(int64_t *target, 
                              int64_t *source,
                              fortran_integer_t *nreduce,
                              fortran_integer_t *PE_start,
                              fortran_integer_t *logPE_stride,
                              fortran_integer_t *PE_size,
                              int64_t *pWrk,
                              fortran_integer_t *pSync);
void
FC_SHMEM_INT8_SUM_TO_ALL(int64_t *target, 
                         int64_t *source,
                         fortran_integer_t *nreduce,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         int64_t *pWrk,
                         fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_SUM, SHM_INTERNAL_INT64);
}


#define FC_SHMEM_REAL4_SUM_TO_ALL FC_FUNC_(shmem_real4_sum_to_all, SHMEM_REAL4_SUM_TO_ALL)
void FC_SHMEM_REAL4_SUM_TO_ALL(float *target, 
                               float *source,
                               fortran_integer_t *nreduce,
                               fortran_integer_t *PE_start,
                               fortran_integer_t *logPE_stride,
                               fortran_integer_t *PE_size,
                               float *pWrk,
                               fortran_integer_t *pSync);
void
FC_SHMEM_REAL4_SUM_TO_ALL(float *target, 
                          float *source,
                          fortran_integer_t *nreduce,
                          fortran_integer_t *PE_start,
                          fortran_integer_t *logPE_stride,
                          fortran_integer_t *PE_size,
                          float *pWrk,
                          fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    shmem_internal_assert(sizeof(float) == 4);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 4,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_SUM, SHM_INTERNAL_FLOAT);
}


#define FC_SHMEM_REAL8_SUM_TO_ALL FC_FUNC_(shmem_real8_sum_to_all, SHMEM_REAL8_SUM_TO_ALL)
void FC_SHMEM_REAL8_SUM_TO_ALL(double *target, 
                               double *source,
                               fortran_integer_t *nreduce,
                               fortran_integer_t *PE_start,
                               fortran_integer_t *logPE_stride,
                               fortran_integer_t *PE_size,
                               double *pWrk,
                               fortran_integer_t *pSync);
void
FC_SHMEM_REAL8_SUM_TO_ALL(double *target, 
                          double *source,
                          fortran_integer_t *nreduce,
                          fortran_integer_t *PE_start,
                          fortran_integer_t *logPE_stride,
                          fortran_integer_t *PE_size,
                          double *pWrk,
                          fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    shmem_internal_assert(sizeof(double) == 8);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_SUM, SHM_INTERNAL_DOUBLE);
}


#if SIZEOF_LONG_DOUBLE == 16
#define FC_SHMEM_REAL16_SUM_TO_ALL FC_FUNC_(shmem_real16_sum_to_all, SHMEM_REAL16_SUM_TO_ALL)
void FC_SHMEM_REAL16_SUM_TO_ALL(long double *target, 
                                long double *source,
                                fortran_integer_t *nreduce,
                                fortran_integer_t *PE_start,
                                fortran_integer_t *logPE_stride,
                                fortran_integer_t *PE_size,
                                long double *pWrk,
                                fortran_integer_t *pSync);
void
FC_SHMEM_REAL16_SUM_TO_ALL(long double *target, 
                           long double *source,
                           fortran_integer_t *nreduce,
                           fortran_integer_t *PE_start,
                           fortran_integer_t *logPE_stride,
                           fortran_integer_t *PE_size,
                           long double *pWrk,
                           fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_SUM, SHM_INTERNAL_LONG_DOUBLE);
}
#endif


#define FC_SHMEM_INT4_PROD_TO_ALL FC_FUNC_(shmem_int4_prod_to_all, SHMEM_INT4_PROD_TO_ALL)
void FC_SHMEM_INT4_PROD_TO_ALL(int32_t *target, 
                              int32_t *source,
                              fortran_integer_t *nreduce,
                              fortran_integer_t *PE_start,
                              fortran_integer_t *logPE_stride,
                              fortran_integer_t *PE_size,
                              int32_t *pWrk,
                              fortran_integer_t *pSync);
void
FC_SHMEM_INT4_PROD_TO_ALL(int32_t *target, 
                         int32_t *source,
                         fortran_integer_t *nreduce,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         int32_t *pWrk,
                         fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 4,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_PROD, SHM_INTERNAL_INT32);
}


#define FC_SHMEM_INT8_PROD_TO_ALL FC_FUNC_(shmem_int8_prod_to_all, SHMEM_INT8_PROD_TO_ALL)
void FC_SHMEM_INT8_PROD_TO_ALL(int64_t *target, 
                              int64_t *source,
                              fortran_integer_t *nreduce,
                              fortran_integer_t *PE_start,
                              fortran_integer_t *logPE_stride,
                              fortran_integer_t *PE_size,
                              int64_t *pWrk,
                              fortran_integer_t *pSync);
void
FC_SHMEM_INT8_PROD_TO_ALL(int64_t *target, 
                         int64_t *source,
                         fortran_integer_t *nreduce,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         int64_t *pWrk,
                         fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_PROD, SHM_INTERNAL_INT64);
}


#define FC_SHMEM_REAL4_PROD_TO_ALL FC_FUNC_(shmem_real4_prod_to_all, SHMEM_REAL4_PROD_TO_ALL)
void FC_SHMEM_REAL4_PROD_TO_ALL(float *target, 
                               float *source,
                               fortran_integer_t *nreduce,
                               fortran_integer_t *PE_start,
                               fortran_integer_t *logPE_stride,
                               fortran_integer_t *PE_size,
                               float *pWrk,
                               fortran_integer_t *pSync);
void
FC_SHMEM_REAL4_PROD_TO_ALL(float *target, 
                          float *source,
                          fortran_integer_t *nreduce,
                          fortran_integer_t *PE_start,
                          fortran_integer_t *logPE_stride,
                          fortran_integer_t *PE_size,
                          float *pWrk,
                          fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    shmem_internal_assert(sizeof(float) == 4);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 4,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_PROD, SHM_INTERNAL_FLOAT);
}


#define FC_SHMEM_REAL8_PROD_TO_ALL FC_FUNC_(shmem_real8_prod_to_all, SHMEM_REAL8_PROD_TO_ALL)
void FC_SHMEM_REAL8_PROD_TO_ALL(double *target, 
                               double *source,
                               fortran_integer_t *nreduce,
                               fortran_integer_t *PE_start,
                               fortran_integer_t *logPE_stride,
                               fortran_integer_t *PE_size,
                               double *pWrk,
                               fortran_integer_t *pSync);
void
FC_SHMEM_REAL8_PROD_TO_ALL(double *target, 
                          double *source,
                          fortran_integer_t *nreduce,
                          fortran_integer_t *PE_start,
                          fortran_integer_t *logPE_stride,
                          fortran_integer_t *PE_size,
                          double *pWrk,
                          fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    shmem_internal_assert(sizeof(double) == 8);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_PROD, SHM_INTERNAL_DOUBLE);
}


#if SIZEOF_LONG_DOUBLE == 16
#define FC_SHMEM_REAL16_PROD_TO_ALL FC_FUNC_(shmem_real16_prod_to_all, SHMEM_REAL16_PROD_TO_ALL)
void FC_SHMEM_REAL16_PROD_TO_ALL(long double *target, 
                                long double *source,
                                fortran_integer_t *nreduce,
                                fortran_integer_t *PE_start,
                                fortran_integer_t *logPE_stride,
                                fortran_integer_t *PE_size,
                                long double *pWrk,
                                fortran_integer_t *pSync);
void
FC_SHMEM_REAL16_PROD_TO_ALL(long double *target, 
                           long double *source,
                           fortran_integer_t *nreduce,
                           fortran_integer_t *PE_start,
                           fortran_integer_t *logPE_stride,
                           fortran_integer_t *PE_size,
                           long double *pWrk,
                           fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_op_to_all(target, source, *nreduce, 8,
                             *PE_start, *logPE_stride, *PE_size,
                             pWrk, pSync_c, SHM_INTERNAL_PROD, SHM_INTERNAL_LONG_DOUBLE);
}
#endif


#define FC_SHMEM_COLLECT4 FC_FUNC_(shmem_collect4, SHMEM_COLLECT4)
void FC_SHMEM_COLLECT4(void *target, 
                       void *source,
                       fortran_integer_t *nelems,
                       fortran_integer_t *PE_start,
                       fortran_integer_t *logPE_stride,
                       fortran_integer_t *PE_size,
                       fortran_integer_t *pSync);
void
FC_SHMEM_COLLECT4(void *target, 
                  void *source,
                  fortran_integer_t *nelems,
                  fortran_integer_t *PE_start,
                  fortran_integer_t *logPE_stride,
                  fortran_integer_t *PE_size,
                  fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_collect(target, source, *nelems * 4,
                           *PE_start, *logPE_stride, *PE_size, pSync_c);
}


#define FC_SHMEM_COLLECT8 FC_FUNC_(shmem_collect8, SHMEM_COLLECT8)
void FC_SHMEM_COLLECT8(void *target, 
                       void *source,
                       fortran_integer_t *nelems,
                       fortran_integer_t *PE_start,
                       fortran_integer_t *logPE_stride,
                       fortran_integer_t *PE_size,
                       fortran_integer_t *pSync);
void
FC_SHMEM_COLLECT8(void *target, 
                  void *source,
                  fortran_integer_t *nelems,
                  fortran_integer_t *PE_start,
                  fortran_integer_t *logPE_stride,
                  fortran_integer_t *PE_size,
                  fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_collect(target, source, *nelems * 8,
                           *PE_start, *logPE_stride, *PE_size, pSync_c);
}


#define FC_SHMEM_COLLECT32 FC_FUNC_(shmem_collect32, SHMEM_COLLECT32)
void FC_SHMEM_COLLECT32(void *target, 
                       void *source,
                       fortran_integer_t *nelems,
                       fortran_integer_t *PE_start,
                       fortran_integer_t *logPE_stride,
                       fortran_integer_t *PE_size,
                       fortran_integer_t *pSync);
void
FC_SHMEM_COLLECT32(void *target, 
                  void *source,
                  fortran_integer_t *nelems,
                  fortran_integer_t *PE_start,
                  fortran_integer_t *logPE_stride,
                  fortran_integer_t *PE_size,
                  fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_collect(target, source, *nelems * 4,
                           *PE_start, *logPE_stride, *PE_size, pSync_c);
}


#define FC_SHMEM_COLLECT64 FC_FUNC_(shmem_collect64, SHMEM_COLLECT64)
void FC_SHMEM_COLLECT64(void *target, 
                       void *source,
                       fortran_integer_t *nelems,
                       fortran_integer_t *PE_start,
                       fortran_integer_t *logPE_stride,
                       fortran_integer_t *PE_size,
                       fortran_integer_t *pSync);
void
FC_SHMEM_COLLECT64(void *target, 
                  void *source,
                  fortran_integer_t *nelems,
                  fortran_integer_t *PE_start,
                  fortran_integer_t *logPE_stride,
                  fortran_integer_t *PE_size,
                  fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_collect(target, source, *nelems * 8,
                           *PE_start, *logPE_stride, *PE_size, pSync_c);
}


#define FC_SHMEM_FCOLLECT4 FC_FUNC_(shmem_fcollect4, SHMEM_FCOLLECT4)
void FC_SHMEM_FCOLLECT4(void *target, 
                       void *source,
                       fortran_integer_t *nelems,
                       fortran_integer_t *PE_start,
                       fortran_integer_t *logPE_stride,
                       fortran_integer_t *PE_size,
                       fortran_integer_t *pSync);
void
FC_SHMEM_FCOLLECT4(void *target, 
                  void *source,
                  fortran_integer_t *nelems,
                  fortran_integer_t *PE_start,
                  fortran_integer_t *logPE_stride,
                  fortran_integer_t *PE_size,
                  fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_fcollect(target, source, *nelems * 4,
                           *PE_start, *logPE_stride, *PE_size, pSync_c);
}


#define FC_SHMEM_FCOLLECT8 FC_FUNC_(shmem_fcollect8, SHMEM_FCOLLECT8)
void FC_SHMEM_FCOLLECT8(void *target, 
                       void *source,
                       fortran_integer_t *nelems,
                       fortran_integer_t *PE_start,
                       fortran_integer_t *logPE_stride,
                       fortran_integer_t *PE_size,
                       fortran_integer_t *pSync);
void
FC_SHMEM_FCOLLECT8(void *target, 
                  void *source,
                  fortran_integer_t *nelems,
                  fortran_integer_t *PE_start,
                  fortran_integer_t *logPE_stride,
                  fortran_integer_t *PE_size,
                  fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_fcollect(target, source, *nelems * 8,
                           *PE_start, *logPE_stride, *PE_size, pSync_c);
}


#define FC_SHMEM_FCOLLECT32 FC_FUNC_(shmem_fcollect32, SHMEM_FCOLLECT32)
void FC_SHMEM_FCOLLECT32(void *target, 
			 void *source,
			 fortran_integer_t *nelems,
			 fortran_integer_t *PE_start,
			 fortran_integer_t *logPE_stride,
			 fortran_integer_t *PE_size,
			 fortran_integer_t *pSync);
void
FC_SHMEM_FCOLLECT32(void *target, 
		    void *source,
		    fortran_integer_t *nelems,
		    fortran_integer_t *PE_start,
		    fortran_integer_t *logPE_stride,
		    fortran_integer_t *PE_size,
		    fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_fcollect(target, source, *nelems * 4,
                           *PE_start, *logPE_stride, *PE_size, pSync_c);
}


#define FC_SHMEM_FCOLLECT64 FC_FUNC_(shmem_fcollect64, SHMEM_FCOLLECT64)
void FC_SHMEM_FCOLLECT64(void *target, 
			 void *source,
			 fortran_integer_t *nelems,
			 fortran_integer_t *PE_start,
			 fortran_integer_t *logPE_stride,
			 fortran_integer_t *PE_size,
			 fortran_integer_t *pSync);
void
FC_SHMEM_FCOLLECT64(void *target, 
		    void *source,
		    fortran_integer_t *nelems,
		    fortran_integer_t *PE_start,
		    fortran_integer_t *logPE_stride,
		    fortran_integer_t *PE_size,
		    fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_fcollect(target, source, *nelems * 8,
                           *PE_start, *logPE_stride, *PE_size, pSync_c);
}


#define FC_SHMEM_BROADCAST4 FC_FUNC_(shmem_broadcast4, SHMEM_BROADCAST4)
void FC_SHMEM_BROADCAST4(void *target, 
                         void *source,
                         fortran_integer_t *nelems,
                         fortran_integer_t *PE_root,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         fortran_integer_t *pSync);
void
FC_SHMEM_BROADCAST4(void *target, 
                    void *source,
                    fortran_integer_t *nelems,
                    fortran_integer_t *PE_root,
                    fortran_integer_t *PE_start,
                    fortran_integer_t *logPE_stride,
                    fortran_integer_t *PE_size,
                    fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*PE_root);
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_bcast(target, source, *nelems * 4,
                     *PE_root, *PE_start, *logPE_stride, *PE_size,
                     pSync_c, 1);
}


#define FC_SHMEM_BROADCAST8 FC_FUNC_(shmem_broadcast8, SHMEM_BROADCAST8)
void FC_SHMEM_BROADCAST8(void *target, 
                         void *source,
                         fortran_integer_t *nelems,
                         fortran_integer_t *PE_root,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         fortran_integer_t *pSync);
void
FC_SHMEM_BROADCAST8(void *target, 
                    void *source,
                    fortran_integer_t *nelems,
                    fortran_integer_t *PE_root,
                    fortran_integer_t *PE_start,
                    fortran_integer_t *logPE_stride,
                    fortran_integer_t *PE_size,
                    fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*PE_root);
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_bcast(target, source, *nelems * 8,
                         *PE_root, *PE_start, *logPE_stride, *PE_size,
                         pSync_c, 1);
}


#define FC_SHMEM_BROADCAST32 FC_FUNC_(shmem_broadcast32, SHMEM_BROADCAST32)
void FC_SHMEM_BROADCAST32(void *target, 
                         void *source,
                         fortran_integer_t *nelems,
                          fortran_integer_t *PE_root,
                         fortran_integer_t *PE_start,
                         fortran_integer_t *logPE_stride,
                         fortran_integer_t *PE_size,
                         fortran_integer_t *pSync);
void
FC_SHMEM_BROADCAST32(void *target, 
                    void *source,
                    fortran_integer_t *nelems,
                    fortran_integer_t *PE_root,
                    fortran_integer_t *PE_start,
                    fortran_integer_t *logPE_stride,
                    fortran_integer_t *PE_size,
                    fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*PE_root);
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_bcast(target, source, *nelems * 4,
                         *PE_root, *PE_start, *logPE_stride, *PE_size,
                         pSync_c, 1);
}


#define FC_SHMEM_BROADCAST64 FC_FUNC_(shmem_broadcast64, SHMEM_BROADCAST64)
void FC_SHMEM_BROADCAST64(void *target, 
                          void *source,
                          fortran_integer_t *nelems,
                          fortran_integer_t *PE_root,
                          fortran_integer_t *PE_start,
                          fortran_integer_t *logPE_stride,
                          fortran_integer_t *PE_size,
                          fortran_integer_t *pSync);
void
FC_SHMEM_BROADCAST64(void *target, 
                    void *source,
                    fortran_integer_t *nelems,
                    fortran_integer_t *PE_root,
                    fortran_integer_t *PE_start,
                    fortran_integer_t *logPE_stride,
                    fortran_integer_t *PE_size,
                    fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*PE_root);
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_bcast(target, source, *nelems * 8,
                         *PE_root, *PE_start, *logPE_stride, *PE_size,
                         pSync_c, 1);
}


#define FC_SHMEM_ALLTOALL32 FC_FUNC_(shmem_alltoall32, SHMEM_ALLTOALL32)
void FC_SHMEM_ALLTOALL32(void *dest,
                          void *source,
                          fortran_integer_t *nelems,
                          fortran_integer_t *PE_start,
                          fortran_integer_t *logPE_stride,
                          fortran_integer_t *PE_size,
                          fortran_integer_t *pSync);
void
FC_SHMEM_ALLTOALL32(void *target,
                    void *source,
                    fortran_integer_t *nelems,
                    fortran_integer_t *PE_start,
                    fortran_integer_t *logPE_stride,
                    fortran_integer_t *PE_size,
                    fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_ALLTOALL_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_alltoall(target, source, *nelems * 4, *PE_start,
                            *logPE_stride, *PE_size, pSync_c);
}


#define FC_SHMEM_ALLTOALL64 FC_FUNC_(shmem_alltoall64, SHMEM_ALLTOALL64)
void FC_SHMEM_ALLTOALL64(void *dest,
                          void *source,
                          fortran_integer_t *nelems,
                          fortran_integer_t *PE_start,
                          fortran_integer_t *logPE_stride,
                          fortran_integer_t *PE_size,
                          fortran_integer_t *pSync);
void
FC_SHMEM_ALLTOALL64(void *target,
                    void *source,
                    fortran_integer_t *nelems,
                    fortran_integer_t *PE_start,
                    fortran_integer_t *logPE_stride,
                    fortran_integer_t *PE_size,
                    fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_ALLTOALL_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_alltoall(target, source, *nelems * 8, *PE_start,
                            *logPE_stride, *PE_size, pSync_c);
}


#define FC_SHMEM_ALLTOALLS32 FC_FUNC_(shmem_alltoalls32, SHMEM_ALLTOALLS32)
void FC_SHMEM_ALLTOALLS32(void *dest,
                          void *source,
                          fortran_integer_t *dst,
                          fortran_integer_t *sst,
                          fortran_integer_t *nelems,
                          fortran_integer_t *PE_start,
                          fortran_integer_t *logPE_stride,
                          fortran_integer_t *PE_size,
                          fortran_integer_t *pSync);
void
FC_SHMEM_ALLTOALLS32(void *target,
                     void *source,
                     fortran_integer_t *dst,
                     fortran_integer_t *sst,
                     fortran_integer_t *nelems,
                     fortran_integer_t *PE_start,
                     fortran_integer_t *logPE_stride,
                     fortran_integer_t *PE_size,
                     fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ARG_POSITIVE(*sst);
    SHMEM_ERR_CHECK_ARG_POSITIVE(*dst);
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_ALLTOALL_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_alltoalls(target, source, *dst, *sst, 4, *nelems,
                             *PE_start, *logPE_stride, *PE_size, pSync_c);
}


#define FC_SHMEM_ALLTOALLS64 FC_FUNC_(shmem_alltoalls64, SHMEM_ALLTOALLS64)
void FC_SHMEM_ALLTOALLS64(void *dest,
                          void *source,
                          fortran_integer_t *dst,
                          fortran_integer_t *sst,
                          fortran_integer_t *nelems,
                          fortran_integer_t *PE_start,
                          fortran_integer_t *logPE_stride,
                          fortran_integer_t *PE_size,
                          fortran_integer_t *pSync);
void
FC_SHMEM_ALLTOALLS64(void *target,
                     void *source,
                     fortran_integer_t *dst,
                     fortran_integer_t *sst,
                     fortran_integer_t *nelems,
                     fortran_integer_t *PE_start,
                     fortran_integer_t *logPE_stride,
                     fortran_integer_t *PE_size,
                     fortran_integer_t *pSync)
{
    long *pSync_c;
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_ARG_POSITIVE(*sst);
    SHMEM_ERR_CHECK_ARG_POSITIVE(*dst);
    SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size);

    /* SHMEM_ALLTOALL_SYNC_SIZE is defined to allow this cast */
    pSync_c = (long*) pSync;

    shmem_internal_alltoalls(target, source, *dst, *sst, 8, *nelems,
                             *PE_start, *logPE_stride, *PE_size, pSync_c);
}
