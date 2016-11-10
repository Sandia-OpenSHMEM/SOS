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
        SHMEM_ERR_CHECK_PE(*pe);                                        \
        SHMEM_ERR_CHECK_NON_NEGATIVE(*len);                             \
        SHMEM_ERR_CHECK_SYMMETRIC(target, SIZE * *len);                 \
                                                                        \
        shmem_internal_put_nb(target, source, SIZE * *len, *pe,         \
                              &completion);                             \
        shmem_internal_put_wait(&completion);                           \
    }

#define SHMEM_DEF_FC_PUT(STYPE_L,STYPE_U,SIZE) \
    SHMEM_STAMP_FC_PUT(FC_FUNC_(shmem_##STYPE_L##_put, SHMEM_##STYPE_U##_PUT), SIZE)

#define SHMEM_DEF_FC_PUT_SIZE(STYPE_L,STYPE_U,SIZE) \
    SHMEM_STAMP_FC_PUT(FC_FUNC_(shmem_put##STYPE_L, SHMEM_PUT##STYPE_U), SIZE)


SHMEM_BIND_F_RMA(SHMEM_DEF_FC_PUT)
SHMEM_BIND_F_SIZES(SHMEM_DEF_FC_PUT_SIZE)


#define SHMEM_STAMP_FC_PUT_NBI(FUNCNAME, SIZE)                          \
    void FUNCNAME(void *target, void *source, fortran_integer_t *nelems,\
                  fortran_integer_t *pe);                               \
    void FUNCNAME(void *target, void *source, fortran_integer_t *nelems,\
                  fortran_integer_t *pe)                                \
    {                                                                   \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_PE(*pe);                                        \
        SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);                          \
        SHMEM_ERR_CHECK_SYMMETRIC(target, SIZE * *nelems);              \
                                                                        \
        shmem_internal_put_nb(target, source, SIZE * *nelems, *pe, NULL); \
    }

#define SHMEM_DEF_FC_PUT_NBI(STYPE_L,STYPE_U,SIZE) \
    SHMEM_STAMP_FC_PUT_NBI(FC_FUNC_(shmem_##STYPE_L##_put_nbi, SHMEM_##STYPE_U##_PUT_NBI), SIZE)

#define SHMEM_DEF_FC_PUT_NBI_SIZE(STYPE_L,STYPE_U,SIZE) \
    SHMEM_STAMP_FC_PUT_NBI(FC_FUNC_(shmem_put##STYPE_L##_nbi, SHMEM_PUT##STYPE_U##_NBI), SIZE)


SHMEM_BIND_F_RMA(SHMEM_DEF_FC_PUT_NBI)
SHMEM_BIND_F_SIZES(SHMEM_DEF_FC_PUT_NBI_SIZE)


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
        SHMEM_ERR_CHECK_PE(*pe);                                        \
        SHMEM_ERR_CHECK_POSITIVE(*tst);                                 \
        SHMEM_ERR_CHECK_POSITIVE(*sst);                                 \
        SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);                            \
        SHMEM_ERR_CHECK_SYMMETRIC(target, SIZE*len + SIZE * *tst * (len-1)); \
                                                                        \
        for ( ; len > 0 ; --len) {                                      \
            shmem_internal_put_small(target, source, SIZE, *pe);        \
            target += (*tst * SIZE);                                    \
            source += (*sst * SIZE);                                    \
        }                                                               \
    }


#define SHMEM_DEF_FC_IPUT(STYPE_L,STYPE_U,SIZE) \
    SHMEM_STAMP_FC_IPUT(FC_FUNC_(shmem_##STYPE_L##_iput, SHMEM_##STYPE_U##_IPUT), SIZE)

#define SHMEM_DEF_FC_IPUT_SIZE(STYPE_L,STYPE_U,SIZE) \
    SHMEM_STAMP_FC_IPUT(FC_FUNC_(shmem_iput##STYPE_L, SHMEM_IPUT##STYPE_U), SIZE)


SHMEM_BIND_F_RMA(SHMEM_DEF_FC_IPUT)
SHMEM_BIND_F_SIZES(SHMEM_DEF_FC_IPUT_SIZE)


#define SHMEM_STAMP_FC_GET(FUNCNAME, SIZE)                              \
    void FUNCNAME(void *target, void *source, fortran_integer_t *len,   \
                  fortran_integer_t *pe);                               \
    void FUNCNAME(void *target, void *source, fortran_integer_t *len,   \
                  fortran_integer_t *pe)                                \
    {                                                                   \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_PE(*pe);                                        \
        SHMEM_ERR_CHECK_NON_NEGATIVE(*len);                             \
        SHMEM_ERR_CHECK_SYMMETRIC(source, SIZE * *len);                 \
                                                                        \
        shmem_internal_get(target, source, SIZE * *len, *pe);           \
        shmem_internal_get_wait();                                      \
    }

#define SHMEM_DEF_FC_GET(STYPE_L,STYPE_U,SIZE) \
    SHMEM_STAMP_FC_GET(FC_FUNC_(shmem_##STYPE_L##_get, SHMEM_##STYPE_U##_GET), SIZE)

#define SHMEM_DEF_FC_GET_SIZE(STYPE_L,STYPE_U,SIZE) \
    SHMEM_STAMP_FC_GET(FC_FUNC_(shmem_get##STYPE_L, SHMEM_GET##STYPE_U), SIZE)


SHMEM_BIND_F_RMA(SHMEM_DEF_FC_GET)
SHMEM_BIND_F_SIZES(SHMEM_DEF_FC_GET_SIZE)


#define SHMEM_STAMP_FC_GET_NBI(FUNCNAME, SIZE)                          \
    void FUNCNAME(void *target, void *source, fortran_integer_t *nelems,\
                  fortran_integer_t *pe);                               \
    void FUNCNAME(void *target, void *source, fortran_integer_t *nelems,\
                  fortran_integer_t *pe)                                \
    {                                                                   \
        SHMEM_ERR_CHECK_INITIALIZED();                                  \
        SHMEM_ERR_CHECK_PE(*pe);                                        \
        SHMEM_ERR_CHECK_SYMMETRIC(source, SIZE * *nelems);              \
                                                                        \
        shmem_internal_get(target, source, SIZE * *nelems, *pe);        \
    }

#define SHMEM_DEF_FC_GET_NBI(STYPE_L,STYPE_U,SIZE) \
    SHMEM_STAMP_FC_GET_NBI(FC_FUNC_(shmem_##STYPE_L##_get_nbi, SHMEM_##STYPE_U##_GET_NBI), SIZE)

#define SHMEM_DEF_FC_GET_NBI_SIZE(STYPE_L,STYPE_U,SIZE) \
    SHMEM_STAMP_FC_GET_NBI(FC_FUNC_(shmem_get##STYPE_L##_nbi, SHMEM_GET##STYPE_U##_NBI), SIZE)


SHMEM_BIND_F_RMA(SHMEM_DEF_FC_GET_NBI)
SHMEM_BIND_F_SIZES(SHMEM_DEF_FC_GET_NBI_SIZE)


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
        SHMEM_ERR_CHECK_PE(*pe);                                        \
        SHMEM_ERR_CHECK_POSITIVE(*tst);                                 \
        SHMEM_ERR_CHECK_POSITIVE(*sst);                                 \
        SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);                            \
        SHMEM_ERR_CHECK_SYMMETRIC(source, SIZE*len + SIZE * *sst * (len-1)); \
                                                                        \
        for ( ; len > 0 ; --len ) {                                     \
            shmem_internal_get(target, source, SIZE, *pe);              \
            target += (*tst * SIZE);                                    \
            source += (*sst * SIZE);                                    \
        }                                                               \
        shmem_internal_get_wait();                                      \
    }

#define SHMEM_DEF_FC_IGET(STYPE_L,STYPE_U,SIZE) \
    SHMEM_STAMP_FC_IGET(FC_FUNC_(shmem_##STYPE_L##_iget, SHMEM_##STYPE_U##_IGET), SIZE)

#define SHMEM_DEF_FC_IGET_SIZE(STYPE_L,STYPE_U,SIZE) \
    SHMEM_STAMP_FC_IGET(FC_FUNC_(shmem_iget##STYPE_L, SHMEM_IGET##STYPE_U), SIZE)


SHMEM_BIND_F_RMA(SHMEM_DEF_FC_IGET)
SHMEM_BIND_F_SIZES(SHMEM_DEF_FC_IGET_SIZE)
