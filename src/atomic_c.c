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

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"
#include "atomic_c_pshmem.h"

#define SHMEM_DEF_SWAP(STYPE,TYPE,ITYPE) \
    TYPE shmem_##STYPE##_swap(TYPE *target, TYPE value, int pe) \
    {                                                           \
        TYPE newval;                                            \
        SHMEM_ERR_CHECK_INITIALIZED();                          \
        SHMEM_ERR_CHECK_PE(pe);                                 \
        SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE));        \
        shmem_internal_swap(target, &value, &newval,            \
                            sizeof(TYPE), pe, ITYPE);           \
        shmem_internal_get_wait();                              \
        return newval;                                          \
    }

SHMEM_BIND_C_EXTENDED_AMO(SHMEM_DEF_SWAP)


/* shmem_swap is a special case in the API */
long
shmem_swap(long *target, long value, int pe)
{
    long newval;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(long));

    shmem_internal_swap(target, &value, &newval, sizeof(long), pe, SHM_INTERNAL_LONG);
    shmem_internal_get_wait();
    return newval;
}


#define SHMEM_DEF_CSWAP(STYPE,TYPE,ITYPE) \
    TYPE shmem_##STYPE##_cswap(TYPE *target, TYPE cond, TYPE value,     \
                               int pe)                                  \
    {                                                                   \
        TYPE newval;                                                    \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_PE(pe);                                         \
        SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE));                \
        shmem_internal_cswap(target, &value, &newval, &cond,            \
                             sizeof(TYPE), pe, ITYPE);                  \
        shmem_internal_get_wait();                                      \
        return newval;                                                  \
    }

SHMEM_BIND_C_AMO(SHMEM_DEF_CSWAP)

#define SHMEM_DEF_INC(STYPE,TYPE,ITYPE)                                 \
    void shmem_##STYPE##_inc(TYPE *target, int pe)                      \
    {                                                                   \
        TYPE tmp = 1;                                                   \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_PE(pe);                                         \
        SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE));                \
        shmem_internal_atomic_small(target, &tmp, sizeof(TYPE), pe,     \
                                    SHM_INTERNAL_SUM, ITYPE);           \
    }

SHMEM_BIND_C_AMO(SHMEM_DEF_INC)

#define SHMEM_DEF_FINC(STYPE,TYPE,ITYPE)                                \
    TYPE shmem_##STYPE##_finc(TYPE *target, int pe)                     \
    {                                                                   \
        TYPE oldval, tmp = 1;                                           \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_PE(pe);                                         \
        SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE));                \
        shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(TYPE),\
                                    pe, SHM_INTERNAL_SUM, ITYPE);       \
        shmem_internal_get_wait();                                      \
        return oldval;                                                  \
    }

SHMEM_BIND_C_AMO(SHMEM_DEF_FINC)

#define SHMEM_DEF_ADD(STYPE,TYPE,ITYPE)                                 \
    void shmem_##STYPE##_add(TYPE *target, TYPE value, int pe)          \
    {                                                                   \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_PE(pe);                                         \
        SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE));                \
                                                                        \
        shmem_internal_atomic_small(target, &value, sizeof(TYPE), pe,   \
                                    SHM_INTERNAL_SUM, ITYPE);           \
    }


SHMEM_BIND_C_AMO(SHMEM_DEF_ADD)

#define SHMEM_DEF_FADD(STYPE,TYPE,ITYPE)                                \
    TYPE shmem_##STYPE##_fadd(TYPE *target, TYPE value, int pe)         \
    {                                                                   \
        int oldval;                                                     \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_PE(pe);                                         \
        SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE));                \
                                                                        \
        shmem_internal_fetch_atomic(target, &value, &oldval, sizeof(TYPE), \
                                    pe, SHM_INTERNAL_SUM, ITYPE);       \
        shmem_internal_get_wait();                                      \
        return oldval;                                                  \
    }

SHMEM_BIND_C_AMO(SHMEM_DEF_FADD)

#define SHMEM_DEF_FETCH(STYPE,TYPE,ITYPE)                               \
    TYPE shmem_##STYPE##_fetch(const TYPE *source, int pe)              \
    {                                                                   \
        TYPE val;                                                       \
                                                                        \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_PE(pe);                                         \
        SHMEM_ERR_CHECK_SYMMETRIC(source, sizeof(TYPE));                \
                                                                        \
        shmem_internal_atomic_fetch(&val, (void *) source, sizeof(TYPE),\
                                    pe, ITYPE);                         \
        shmem_internal_get_wait();                                      \
        return val;                                                     \
    }

SHMEM_BIND_C_EXTENDED_AMO(SHMEM_DEF_FETCH)


#define SHMEM_DEF_SET(STYPE,TYPE,ITYPE)                                 \
    void shmem_##STYPE##_set(TYPE *dest, TYPE value, int pe)            \
    {                                                                   \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_PE(pe);                                         \
        SHMEM_ERR_CHECK_SYMMETRIC(dest, sizeof(TYPE));                  \
                                                                        \
        shmem_internal_atomic_set((void *) dest, &value, sizeof(TYPE),  \
                                  pe, ITYPE);                           \
    }

SHMEM_BIND_C_EXTENDED_AMO(SHMEM_DEF_SET)
