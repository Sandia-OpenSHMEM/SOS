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
#include "shmemx.h"
#include "shmem_internal.h"
#include "shmem_comm.h"

#define SHMEM_DEFINE_FOR_FC_RMA(DECL) \
  DECL(complex, COMPLEX, SIZEOF_FORTRAN_COMPLEX) \
  DECL(double,  DOUBLE,  SIZEOF_FORTRAN_DOUBLE_PRECISION)  \
  DECL(integer, INTEGER, SIZEOF_FORTRAN_INTEGER) \
  DECL(real,    REAL,    SIZEOF_FORTRAN_REAL) \
  DECL(logical, LOGICAL, SIZEOF_FORTRAN_LOGICAL)

#define SHMEM_DEFINE_FOR_FC_EXTENDED_RMA(DECL) \
  SHMEM_DEFINE_FOR_FC_RMA(DECL) \
  DECL(character, CHARACTER, SIZEOF_FORTRAN_CHARACTER)

/* Yes, SHMEM_PUT8 means something different in C and fortran.
 * No, I don't think anyone's happy about it.
 * -jpdoyle
 */
#define SHMEM_DEFINE_FOR_FC_SIZES(DECL) \
  DECL(4,   4,    4*sizeof(uint8_t)) \
  DECL(8,   8,    8*sizeof(uint8_t)) \
  DECL(32,  32,   4*sizeof(uint8_t)) \
  DECL(64,  64,   8*sizeof(uint8_t)) \
  DECL(128, 128, 16*sizeof(uint8_t))

#define SHMEM_DEFINE_FOR_FC_EXTENDED_SIZES(DECL) \
  SHMEM_DEFINE_FOR_FC_SIZES(DECL) \
  DECL(mem, MEM, sizeof(uint8_t))

#define SHMEM_FC(FC_type,FC_TYPE,SHM_op,SHM_OP) \
  FC_FUNC_(shmem_##FC_type##_##SHM_op,SHMEM_##FC_TYPE##_##SHM_OP)

#define SHMEM_FC_N(SHM_op,SHM_OP) \
  FC_FUNC_(shmem_##SHM_op,SHMEM_##SHM_OP)

#define SHMEM_DEF_FC_PUT(FC_type,FC_TYPE,SIZE) \
  void SHMEM_FC(FC_type,FC_TYPE,put,PUT)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe);                \
  void SHMEM_FC(FC_type,FC_TYPE,put,PUT)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe) {               \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    shmem_internal_put(target, source,                               \
        SIZE * *len, *pe, SHMEMX_CTX_DEFAULT);                       \
  }

SHMEM_DEFINE_FOR_FC_EXTENDED_RMA(SHMEM_DEF_FC_PUT)

/* TODO: Is this part of the spec? -jpdoyle */
/* #define FC_SHMEM_PUT FC_FUNC_(shmem_put, SHMEM_PUT) */
/* void FC_SHMEM_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe); */
/* void */
/* FC_SHMEM_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe) */
/* { */
/*     long completion = 0; */

/*     SHMEM_ERR_CHECK_INITIALIZED(); */

/*     shmem_internal_put_nb(target, source, *len, *pe, &completion); */
/*     shmem_internal_put_wait(&completion); */
/* } */

#define SHMEM_DEF_FC_PUT_N(name,NAME,SIZE) \
  void SHMEM_FC_N(put##name,PUT##NAME)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe);                \
  void SHMEM_FC_N(put##name,PUT##NAME)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe) {               \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    shmem_internal_put(target, source,                            \
        SIZE * *len, *pe, SHMEMX_CTX_DEFAULT);   \
  }

SHMEM_DEFINE_FOR_FC_EXTENDED_SIZES(SHMEM_DEF_FC_PUT_N)


#define SHMEM_DEF_FC_PUT_NBI(name,NAME,SIZE) \
  void SHMEM_FC(name,NAME,put_nbi,PUT_NBI)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe);                \
  void SHMEM_FC(name,NAME,put_nbi,PUT_NBI)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe) {               \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    shmem_internal_put_nbi(target, source,                           \
        SIZE * *len, *pe, SHMEMX_CTX_DEFAULT);   \
  }
SHMEM_DEFINE_FOR_FC_EXTENDED_RMA(SHMEM_DEF_FC_PUT_NBI)


#define SHMEM_DEF_FC_PUT_N_NBI(name,NAME,SIZE) \
  void SHMEM_FC_N(put##name##_nbi,PUT##NAME##_NBI)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe);                \
  void SHMEM_FC_N(put##name##_nbi,PUT##NAME##_NBI)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe) {               \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    shmem_internal_put_nbi(target, source,                            \
        SIZE * *len, *pe, SHMEMX_CTX_DEFAULT);   \
  }

SHMEM_DEFINE_FOR_FC_EXTENDED_SIZES(SHMEM_DEF_FC_PUT_N_NBI)



#define SHMEM_DEF_FC_IPUT(name,NAME,SIZE) \
  void SHMEM_FC(name,NAME,iput,IPUT)(void *targetp, void *sourcep, \
      fortran_integer_t *tst, fortran_integer_t *sst,              \
      fortran_integer_t *lenp, fortran_integer_t *pe);             \
  void SHMEM_FC(name,NAME,iput,IPUT)(void *targetp, void *sourcep, \
      fortran_integer_t *tst, fortran_integer_t *sst,              \
      fortran_integer_t *lenp, fortran_integer_t *pe) {            \
                                                                     \
    fortran_integer_t len = *lenp;                                   \
    char *target = (char*) targetp;                                  \
    char *source = (char*) sourcep;                                  \
                                                                     \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    for ( ; len > 0 ; --len) {                                       \
      shmem_internal_put_small(target, source, SIZE, *pe,          \
          SHMEMX_CTX_DEFAULT);                                     \
      target += (*tst * SIZE);                                     \
      source += (*sst * SIZE);                                     \
    }                                                                \
  }
SHMEM_DEFINE_FOR_FC_RMA(SHMEM_DEF_FC_IPUT)

#define SHMEM_DEF_FC_IPUT_N(name,NAME,SIZE) \
  void SHMEM_FC_N(iput##name,IPUT##name)(void *targetp, void *sourcep, \
      fortran_integer_t *tst, fortran_integer_t *sst, \
      fortran_integer_t *lenp, fortran_integer_t *pe);                \
  void SHMEM_FC_N(iput##name,IPUT##name)(void *targetp, void *sourcep, \
      fortran_integer_t *tst, fortran_integer_t *sst, \
      fortran_integer_t *lenp, fortran_integer_t *pe) {               \
                                                                     \
    fortran_integer_t len = *lenp;                                   \
    char *target = (char*) targetp;                                  \
    char *source = (char*) sourcep;                                  \
                                                                     \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    for ( ; len > 0 ; --len) {                                       \
      shmem_internal_put_small(target, source, SIZE, *pe,          \
          SHMEMX_CTX_DEFAULT);                                     \
      target += (*tst * SIZE);                                     \
      source += (*sst * SIZE);                                     \
    }                                                                \
  }
SHMEM_DEFINE_FOR_FC_SIZES(SHMEM_DEF_FC_IPUT)


#define SHMEM_DEF_FC_GET(FC_type,FC_TYPE,SIZE) \
  void SHMEM_FC(FC_type,FC_TYPE,get,GET)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe);                \
  void SHMEM_FC(FC_type,FC_TYPE,get,GET)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe) {               \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    shmem_internal_put(target, source,                               \
        SIZE * *len, *pe, SHMEMX_CTX_DEFAULT);                       \
    shmemx_ctx_quiet(SHMEMX_CTX_DEFAULT);                            \
  }

SHMEM_DEFINE_FOR_FC_EXTENDED_RMA(SHMEM_DEF_FC_GET)

#define SHMEM_DEF_FC_GET_N(name,NAME,SIZE) \
  void SHMEM_FC_N(get##name,GET##NAME)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe);                \
  void SHMEM_FC_N(get##name,GET##NAME)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe) {               \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    shmem_internal_get(target, source,                            \
        SIZE * *len, *pe, SHMEMX_CTX_DEFAULT);   \
    shmemx_ctx_quiet(SHMEMX_CTX_DEFAULT);                            \
  }

SHMEM_DEFINE_FOR_FC_EXTENDED_SIZES(SHMEM_DEF_FC_GET_N)


#define SHMEM_DEF_FC_GET_NBI(name,NAME,SIZE) \
  void SHMEM_FC(name,NAME,get_nbi,GET_NBI)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe);                \
  void SHMEM_FC(name,NAME,get_nbi,GET_NBI)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe) {               \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    shmem_internal_get(target, source,                           \
        SIZE * *len, *pe, SHMEMX_CTX_DEFAULT);   \
  }

SHMEM_DEFINE_FOR_FC_EXTENDED_RMA(SHMEM_DEF_FC_GET_NBI)


#define SHMEM_DEF_FC_GET_N_NBI(name,NAME,SIZE) \
  void SHMEM_FC_N(get##name##_nbi,GET##NAME##_NBI)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe);                \
  void SHMEM_FC_N(get##name##_nbi,GET##NAME##_NBI)(void *target, void *source, \
      fortran_integer_t *len, fortran_integer_t *pe) {               \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    shmem_internal_get(target, source,                            \
        SIZE * *len, *pe, SHMEMX_CTX_DEFAULT);   \
  }

SHMEM_DEFINE_FOR_FC_EXTENDED_SIZES(SHMEM_DEF_FC_GET_N_NBI)

#define SHMEM_DEF_FC_IGET(name,NAME,SIZE) \
  void SHMEM_FC(name,NAME,iget,IGET)(void *targetp, void *sourcep, \
      fortran_integer_t *tst, fortran_integer_t *sst, \
      fortran_integer_t *lenp, fortran_integer_t *pe);                \
  void SHMEM_FC(name,NAME,iget,IGET)(void *targetp, void *sourcep, \
      fortran_integer_t *tst, fortran_integer_t *sst, \
      fortran_integer_t *lenp, fortran_integer_t *pe) {               \
                                                                     \
    fortran_integer_t len = *lenp;                                   \
    char *target = (char*) targetp;                                  \
    char *source = (char*) sourcep;                                  \
                                                                     \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    for ( ; len > 0 ; --len) {                                       \
      shmem_internal_get(target, source, SIZE, *pe,          \
          SHMEMX_CTX_DEFAULT);                                     \
      target += (*tst * SIZE);                                     \
      source += (*sst * SIZE);                                     \
    }                                                                \
    shmemx_ctx_quiet(SHMEMX_CTX_DEFAULT);                            \
  }
SHMEM_DEFINE_FOR_FC_RMA(SHMEM_DEF_FC_IGET)

#define SHMEM_DEF_FC_IGET_N(name,NAME,SIZE) \
  void SHMEM_FC_N(iget##name,IGET##name)(void *targetp, void *sourcep, \
      fortran_integer_t *tst, fortran_integer_t *sst, \
      fortran_integer_t *lenp, fortran_integer_t *pe);                \
  void SHMEM_FC_N(iget##name,IGET##name)(void *targetp, void *sourcep, \
      fortran_integer_t *tst, fortran_integer_t *sst, \
      fortran_integer_t *lenp, fortran_integer_t *pe) {               \
                                                                     \
    fortran_integer_t len = *lenp;                                   \
    char *target = (char*) targetp;                                  \
    char *source = (char*) sourcep;                                  \
                                                                     \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    for ( ; len > 0 ; --len) {                                       \
      shmem_internal_get(target, source, SIZE, *pe,          \
          SHMEMX_CTX_DEFAULT);                                     \
      target += (*tst * SIZE);                                     \
      source += (*sst * SIZE);                                     \
    }                                                                \
    shmemx_ctx_quiet(SHMEMX_CTX_DEFAULT);                            \
  }
SHMEM_DEFINE_FOR_FC_SIZES(SHMEM_DEF_FC_IGET)

