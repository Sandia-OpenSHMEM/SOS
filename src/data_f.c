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
#include <unistd.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"


#define SHMEM_STAMP_FC_PUT(FUNCNAME, SIZE)                              \
    void FUNCNAME(void *target, void *source, fortran_integer_t *len,   \
                  fortran_integer_t *pe);                               \
    void FUNCNAME(void *target, void *source, fortran_integer_t *len,   \
                  fortran_integer_t *pe)                                \
    {                                                                   \
        long completion = 0;                                            \
                                                                        \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
                                                                        \
        shmem_internal_put_nb(target, source, SIZE * *len, *pe,         \
                              &completion);                             \
        shmem_internal_put_wait(&completion);                           \
    }

#define SHMEM_DEF_FC_PUT(stype,STYPE,SIZE) \
    SHMEM_STAMP_FC_PUT(FC_FUNC_(shmem_##stype##_put, SHMEM_##STYPE##_PUT), SIZE)

#define SHMEM_DEF_FC_PUT_SIZE(stype,STYPE,SIZE) \
    SHMEM_STAMP_FC_PUT(FC_FUNC_(shmem_put##stype, SHMEM_PUT##STYPE), SIZE)


SHMEM_EVAL_FC_MACRO_FOR_RMA(SHMEM_DEF_FC_PUT,)
SHMEM_EVAL_FC_MACRO_FOR_SIZES(SHMEM_DEF_FC_PUT_SIZE,)


#define SHMEM_STAMP_FC_PUT_NBI(FUNCNAME, SIZE)                          \
    void FUNCNAME(void *target, void *source, fortran_integer_t *nelems,\
                  fortran_integer_t *pe);                               \
    void FUNCNAME(void *target, void *source, fortran_integer_t *nelems,\
                  fortran_integer_t *pe)                                \
    {                                                                   \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
                                                                        \
        shmem_internal_put_nb(dest, source, SIZE * *nelems, *pe, NULL); \
    }

#define SHMEM_DEF_FC_PUT_NBI(stype,STYPE,SIZE) \
    SHMEM_STAMP_FC_PUT(FC_FUNC_(shmem_##stype##_put_nbi, SHMEM_##STYPE##_PUT_NBI), SIZE)

#define SHMEM_DEF_FC_PUT_NBI_SIZE(stype,STYPE,SIZE) \
    SHMEM_STAMP_FC_PUT(FC_FUNC_(shmem_put##stype##_nbi, SHMEM_PUT##STYPE##_NBI), SIZE)


SHMEM_EVAL_FC_MACRO_FOR_RMA(SHMEM_DEF_FC_PUT_NBI,)
SHMEM_EVAL_FC_MACRO_FOR_SIZES(SHMEM_DEF_FC_PUT_NBI_SIZE,)


#define SHMEM_STAMP_FC_IPUT(FUNCNAME, SIZE)                             \
    void FUNCNAME(void *targetp, void *sourcep,                         \
                  fortran_integer_t *tst, fortran_integer_t *sst,       \
                  fortran_integer_t *lenp, fortran_integer_t *pe);      \
    void FUNCNAME(void *targetp, void *sourcep,                         \
                  fortran_integer_t *tst, fortran_integer_t *sst,       \
                  fortran_integer_t *lenp, fortran_integer_t *pe)       \
    {                                                                   \
        fortran_integer_t len = *lenp;                                  \
        char *target = (char*) targetp;                                 \
        char *source = (char*) sourcep;                                 \
                                                                        \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
                                                                        \
        for ( ; len > 0 ; --len) {                                      \
            shmem_internal_put_small(target, source, SIZE, *pe);        \
            target += (*tst * SIZE);                                    \
            source += (*sst * SIZE);                                    \
        }                                                               \
    }


#define SHMEM_DEF_FC_IPUT(stype,STYPE,SIZE) \
    SHMEM_STAMP_FC_PUT(FC_FUNC_(shmem_##stype##_iput, SHMEM_##STYPE##_IPUT), SIZE)

#define SHMEM_DEF_FC_IPUT_SIZE(stype,STYPE,SIZE) \
    SHMEM_STAMP_FC_PUT(FC_FUNC_(shmem_iput##stype, SHMEM_IPUT##STYPE), SIZE)


SHMEM_EVAL_FC_MACRO_FOR_RMA(SHMEM_DEF_FC_IPUT,)
SHMEM_EVAL_FC_MACRO_FOR_SIZES(SHMEM_DEF_FC_IPUT_SIZE,)


#define SHMEM_STAMP_FC_GET(FUNCNAME, SIZE)                              \
    void FUNCNAME(void *target, void *source, fortran_integer_t *len,   \
                  fortran_integer_t *pe);                               \
    void FUNCNAME(void *target, void *source, fortran_integer_t *len,   \
                  fortran_integer_t *pe)                                \
    {                                                                   \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
                                                                        \
        shmem_internal_get(target, source, SIZE * *len, *pe);           \
        shmem_internal_get_wait();                                      \
    }

#define SHMEM_DEF_FC_GET(stype,STYPE,SIZE) \
    SHMEM_STAMP_FC_GET(FC_FUNC_(shmem_##stype##_get, SHMEM_##STYPE##_GET), SIZE)

#define SHMEM_DEF_FC_GET_SIZE(stype,STYPE,SIZE) \
    SHMEM_STAMP_FC_GET(FC_FUNC_(shmem_get##stype, SHMEM_GET##STYPE), SIZE)


SHMEM_EVAL_FC_MACRO_FOR_RMA(SHMEM_DEF_FC_GET,)
SHMEM_EVAL_FC_MACRO_FOR_SIZES(SHMEM_DEF_FC_GET_SIZE,)


#define SHMEM_STAMP_FC_GET_NBI(FUNCNAME, SIZE)                          \
    void FUNCNAME(void *target, void *source, fortran_integer_t *nelems,\
                  fortran_integer_t *pe);                               \
    void FUNCNAME(void *target, void *source, fortran_integer_t *nelems,\
                  fortran_integer_t *pe)                                \
    {                                                                   \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
                                                                        \
        shmem_internal_get(target, source, SIZE * *len, *pe);           \
    }

#define SHMEM_DEF_FC_GET_NBI(stype,STYPE,SIZE) \
    SHMEM_STAMP_FC_GET(FC_FUNC_(shmem_##stype##_get_nbi, SHMEM_##STYPE##_GET_NBI), SIZE)

#define SHMEM_DEF_FC_GET_NBI_SIZE(stype,STYPE,SIZE) \
    SHMEM_STAMP_FC_GET(FC_FUNC_(shmem_get##stype##_nbi, SHMEM_GET##STYPE##_NBI), SIZE)


SHMEM_EVAL_FC_MACRO_FOR_RMA(SHMEM_DEF_FC_GET_NBI,)
SHMEM_EVAL_FC_MACRO_FOR_SIZES(SHMEM_DEF_FC_GET_NBI_SIZE,)


#define SHMEM_STAMP_FC_IGET(FUNCNAME, SIZE)                             \
    void FUNCNAME(void *targetp, void *sourcep,                         \
                  fortran_integer_t *tst, fortran_integer_t *sst,       \
                  fortran_integer_t *lenp, fortran_integer_t *pe);      \
    void FUNCNAME(void *targetp, void *sourcep,                         \
                  fortran_integer_t *tst, fortran_integer_t *sst,       \
                  fortran_integer_t *lenp, fortran_integer_t *pe)       \
    {                                                                   \
        fortran_integer_t len = *lenp;                                  \
        char *target = (char*) targetp;                                 \
        char *source = (char*) sourcep;                                 \
                                                                        \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
                                                                        \
        for ( ; len > 0 ; --len ) {                                     \
            shmem_internal_get(target, source, SIZE, *pe);              \
            target += (*tst * SIZE);                                    \
            source += (*sst * SIZE);                                    \
        }                                                               \
        shmem_internal_get_wait();                                      \
    }

#define SHMEM_DEF_FC_IGET(stype,STYPE,SIZE) \
    SHMEM_STAMP_FC_GET(FC_FUNC_(shmem_##stype##_iget, SHMEM_##STYPE##_IGET), SIZE)

#define SHMEM_DEF_FC_IGET_SIZE(stype,STYPE,SIZE) \
    SHMEM_STAMP_FC_GET(FC_FUNC_(shmem_iget##stype##_nbi, SHMEM_IGET##STYPE##_NBI), SIZE)


SHMEM_EVAL_FC_MACRO_FOR_RMA(SHMEM_DEF_FC_IGET,)
SHMEM_EVAL_FC_MACRO_FOR_SIZES(SHMEM_DEF_FC_IGET_SIZE,)
