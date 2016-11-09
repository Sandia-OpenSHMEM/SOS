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


#define SHMEM_STAMP_TO_ALL(FUNCNAME,TYPE,ITYPE,SIZE,IOP)                \
    void FUNCNAME(TYPE *target, TYPE *source,                           \
                  fortran_integer_t *nreduce,                           \
                  fortran_integer_t *PE_start,                          \
                  fortran_integer_t *logPE_stride,                      \
                  fortran_integer_t *PE_size,                           \
                  int32_t *pWrk,                                        \
                  fortran_integer_t *pSync);                            \
    void FUNCNAME(TYPE *target, TYPE *source,                           \
                  fortran_integer_t *nreduce,                           \
                  fortran_integer_t *PE_start,                          \
                  fortran_integer_t *logPE_stride,                      \
                  fortran_integer_t *PE_size,                           \
                  int32_t *pWrk,                                        \
                  fortran_integer_t *pSync)                             \
    {                                                                   \
        long *pSync_c;                                                  \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size); \
                                                                        \
        shmem_internal_assert(sizeof(TYPE) == SIZE);                    \
                                                                        \
        /* SHMEM_REDUCE_SYNC_SIZE is defined to allow this cast */      \
        pSync_c = (long*) pSync;                                        \
                                                                        \
        shmem_internal_op_to_all(target, source, *nreduce, SIZE,        \
                                 *PE_start, *logPE_stride, *PE_size,    \
                                 pWrk, pSync_c, IOP, ITYPE);            \
    }

#define SHMEM_DEF_TO_ALL(stype,STYPE,TYPE,ITYPE,op,OP,IOP,SIZE)         \
    SHMEM_STAMP_TO_ALL(FC_FUNC_(shmem_##stype##_##op##_to_all,          \
                                SHMEM_##STYPE##_##OP##_TO_ALL),         \
                       TYPE, ITYPE, SIZE, IOP)

SHMEM_EVAL_FC_MACRO_FOR_INTS_WITH_ITYPE_OP(SHMEM_DEF_TO_ALL,and,AND,SHM_INTERNAL_BAND,)
SHMEM_EVAL_FC_MACRO_FOR_INTS_WITH_ITYPE_OP(SHMEM_DEF_TO_ALL,or,OR,SHM_INTERNAL_BOR,)
SHMEM_EVAL_FC_MACRO_FOR_INTS_WITH_ITYPE_OP(SHMEM_DEF_TO_ALL,xor,XOR,SHM_INTERNAL_BXOR,)

SHMEM_EVAL_FC_MACRO_FOR_INTS_WITH_ITYPE_OP(SHMEM_DEF_TO_ALL,min,MIN,SHM_INTERNAL_MIN,)
SHMEM_EVAL_FC_MACRO_FOR_FLOATS_WITH_ITYPE_OP(SHMEM_DEF_TO_ALL,min,MIN,SHM_INTERNAL_MIN,)
SHMEM_EVAL_FC_MACRO_FOR_INTS_WITH_ITYPE_OP(SHMEM_DEF_TO_ALL,max,MAX,SHM_INTERNAL_MAX,)
SHMEM_EVAL_FC_MACRO_FOR_FLOATS_WITH_ITYPE_OP(SHMEM_DEF_TO_ALL,max,MAX,SHM_INTERNAL_MAX,)

SHMEM_EVAL_FC_MACRO_FOR_INTS_WITH_ITYPE_OP(SHMEM_DEF_TO_ALL,sum,SUM,SHM_INTERNAL_SUM,)
SHMEM_EVAL_FC_MACRO_FOR_FLOATS_WITH_ITYPE_OP(SHMEM_DEF_TO_ALL,sum,SUM,SHM_INTERNAL_SUM,)
SHMEM_EVAL_FC_MACRO_FOR_INTS_WITH_ITYPE_OP(SHMEM_DEF_TO_ALL,prod,PROD,SHM_INTERNAL_PROD,)
SHMEM_EVAL_FC_MACRO_FOR_FLOATS_WITH_ITYPE_OP(SHMEM_DEF_TO_ALL,prod,PROD,SHM_INTERNAL_PROD,)


#define SHMEM_STAMP_COLLECT(FUNCNAME, SIZE)                             \
    void FUNCNAME(void *target, void *source,                           \
                  fortran_integer_t *nelems,                            \
                  fortran_integer_t *PE_start,                          \
                  fortran_integer_t *logPE_stride,                      \
                  fortran_integer_t *PE_size,                           \
                  fortran_integer_t *pSync);                            \
    void FUNCNAME(void *target, void *source,                           \
                  fortran_integer_t *nelems,                            \
                  fortran_integer_t *PE_start,                          \
                  fortran_integer_t *logPE_stride,                      \
                  fortran_integer_t *PE_size,                           \
                  fortran_integer_t *pSync)                             \
    {                                                                   \
        long *pSync_c;                                                  \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size); \
                                                                        \
        /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */     \
        pSync_c = (long*) pSync;                                        \
                                                                        \
        shmem_internal_collect(target, source, *nelems * SIZE, *PE_start, \
                               *logPE_stride, *PE_size, pSync_c);       \
    }

#define SHMEM_DEF_COLLECT(STYPE,SIZE)                                   \
    SHMEM_STAMP_COLLECT(FC_FUNC_(shmem_collect##STYPE,                  \
                                 SHMEM_COLLECT##STYPE), SIZE)

SHMEM_EVAL_FC_MACRO_FOR_COLL_SIZES(SHMEM_DEF_COLLECT,)


#define SHMEM_STAMP_FCOLLECT(FUNCNAME, SIZE)                            \
    void FUNCNAME(void *target, void *source,                           \
                  fortran_integer_t *nelems,                            \
                  fortran_integer_t *PE_start,                          \
                  fortran_integer_t *logPE_stride,                      \
                  fortran_integer_t *PE_size,                           \
                  fortran_integer_t *pSync);                            \
    void FUNCNAME(void *target, void *source,                           \
                  fortran_integer_t *nelems,                            \
                  fortran_integer_t *PE_start,                          \
                  fortran_integer_t *logPE_stride,                      \
                  fortran_integer_t *PE_size,                           \
                  fortran_integer_t *pSync)                             \
    {                                                                   \
        long *pSync_c;                                                  \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size); \
                                                                        \
        /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */     \
        pSync_c = (long*) pSync;                                        \
                                                                        \
        shmem_internal_fcollect(target, source, *nelems * SIZE, *PE_start, \
                               *logPE_stride, *PE_size, pSync_c);       \
    }

#define SHMEM_DEF_FCOLLECT(STYPE,SIZE)                                  \
    SHMEM_STAMP_FCOLLECT(FC_FUNC_(shmem_fcollect##STYPE,                \
                                 SHMEM_FCOLLECT##STYPE), SIZE)

SHMEM_EVAL_FC_MACRO_FOR_COLL_SIZES(SHMEM_DEF_FCOLLECT,)


#define SHMEM_STAMP_BROADCAST(FUNCNAME, SIZE)                           \
    void FUNCNAME(void *target, void *source,                           \
                  fortran_integer_t *nelems,                            \
                  fortran_integer_t *PE_root,                           \
                  fortran_integer_t *PE_start,                          \
                  fortran_integer_t *logPE_stride,                      \
                  fortran_integer_t *PE_size,                           \
                  fortran_integer_t *pSync);                            \
    void FUNCNAME(void *target, void *source,                           \
                  fortran_integer_t *nelems,                            \
                  fortran_integer_t *PE_root,                           \
                  fortran_integer_t *PE_start,                          \
                  fortran_integer_t *logPE_stride,                      \
                  fortran_integer_t *PE_size,                           \
                  fortran_integer_t *pSync)                             \
    {                                                                   \
        long *pSync_c;                                                  \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_ACTIVE_SET(*PE_start, *logPE_stride, *PE_size); \
                                                                        \
        /* SHMEM_COLLECT_SYNC_SIZE is defined to allow this cast */     \
        pSync_c = (long*) pSync;                                        \
                                                                        \
        shmem_internal_bcast(target, source, *nelems * SIZE,            \
                         *PE_root, *PE_start, *logPE_stride, *PE_size,  \
                         pSync_c, 1);                                   \
    }

#define SHMEM_DEF_BROADCAST(STYPE,SIZE)                                 \
    SHMEM_STAMP_BROADCAST(FC_FUNC_(shmem_broadcast##STYPE,              \
                                 SHMEM_BROADCAST##STYPE), SIZE)

SHMEM_EVAL_FC_MACRO_FOR_COLL_SIZES(SHMEM_DEF_BROADCAST,)


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
