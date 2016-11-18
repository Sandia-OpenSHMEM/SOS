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
#include "data_c_pshmem.h"

#define SHMEM_DEF_P(STYPE,TYPE) \
  void shmem_##STYPE##_p(TYPE *addr, TYPE value, int pe)       \
  {                                                            \
    SHMEM_ERR_CHECK_INITIALIZED();                             \
    SHMEM_ERR_CHECK_PE(pe);                                    \
    SHMEM_ERR_CHECK_SYMMETRIC(addr, sizeof(TYPE));             \
    shmem_internal_put_small(addr, &value, sizeof(TYPE), pe);  \
  }

SHMEM_DEFINE_FOR_RMA(SHMEM_DEF_P)

#define SHMEM_DEF_G(STYPE,TYPE) \
  TYPE shmem_##STYPE##_g(const TYPE *addr, int pe)   \
  {                                                  \
    TYPE tmp;                                        \
    SHMEM_ERR_CHECK_INITIALIZED();                   \
    SHMEM_ERR_CHECK_PE(pe);                          \
    SHMEM_ERR_CHECK_SYMMETRIC(addr, sizeof(TYPE));   \
    shmem_internal_get(&tmp, addr, sizeof(TYPE), pe);\
    shmem_internal_get_wait();                       \
    return tmp;                                      \
  }

SHMEM_DEFINE_FOR_RMA(SHMEM_DEF_G)

#define SHMEM_DEF_PUT(STYPE,TYPE) \
  void shmem_##STYPE##_put(TYPE *target, const TYPE *source,     \
                           size_t nelems, int pe)                \
  {                                                              \
    long completion = 0;                                         \
    SHMEM_ERR_CHECK_INITIALIZED();                               \
    SHMEM_ERR_CHECK_PE(pe);                                      \
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);                        \
    SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE) * nelems);    \
    SHMEM_ERR_CHECK_NULL(source, nelems);                        \
    shmem_internal_put_nb(target, source, sizeof(TYPE) * nelems, \
                          pe, &completion);                      \
    shmem_internal_put_wait(&completion);                        \
  }

SHMEM_DEFINE_FOR_RMA(SHMEM_DEF_PUT)

#define SHMEM_DEF_PUT_N(NAME,SIZE) \
  void shmem_put##NAME(void *target, const void *source,       \
                       size_t nelems, int pe)                  \
  {                                                            \
    long completion = 0;                                       \
    SHMEM_ERR_CHECK_INITIALIZED();                             \
    SHMEM_ERR_CHECK_PE(pe);                                    \
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);                      \
    SHMEM_ERR_CHECK_SYMMETRIC(target, (SIZE) * nelems);        \
    SHMEM_ERR_CHECK_NULL(source, nelems);                      \
    shmem_internal_put_nb(target, source, (SIZE) * nelems, pe, \
                          &completion);                        \
    shmem_internal_put_wait(&completion);                      \
  }

SHMEM_DEFINE_FOR_SIZES(SHMEM_DEF_PUT_N)
SHMEM_DEF_PUT_N(mem,1)

#define SHMEM_DEF_PUT_NBI(STYPE,TYPE) \
  void shmem_##STYPE##_put_nbi(TYPE *target, const TYPE *source, \
                               size_t nelems, int pe)            \
  {                                                              \
    SHMEM_ERR_CHECK_INITIALIZED();                               \
    SHMEM_ERR_CHECK_PE(pe);                                      \
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);                        \
    SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE) * nelems);    \
    SHMEM_ERR_CHECK_NULL(source, nelems);                        \
    shmem_internal_put_nbi(target, source, sizeof(TYPE)*nelems,  \
        pe);                                                     \
  }

SHMEM_DEFINE_FOR_RMA(SHMEM_DEF_PUT_NBI)

#define SHMEM_DEF_PUT_N_NBI(NAME,SIZE) \
  void shmem_put##NAME##_nbi(void *target, const void *source, \
                             size_t nelems, int pe)            \
  {                                                            \
    SHMEM_ERR_CHECK_INITIALIZED();                             \
    SHMEM_ERR_CHECK_PE(pe);                                    \
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);                      \
    SHMEM_ERR_CHECK_SYMMETRIC(target, (SIZE) * nelems);        \
    SHMEM_ERR_CHECK_NULL(source, nelems);                      \
    shmem_internal_put_nbi(target, source, (SIZE)*nelems, pe); \
  }

SHMEM_DEFINE_FOR_SIZES(SHMEM_DEF_PUT_N_NBI)
SHMEM_DEF_PUT_N_NBI(mem,1)

#define SHMEM_DEF_GET(STYPE,TYPE) \
  void shmem_##STYPE##_get(TYPE *target,const TYPE *source,   \
                           size_t nelems, int pe)             \
  {                                                           \
    SHMEM_ERR_CHECK_INITIALIZED();                            \
    SHMEM_ERR_CHECK_PE(pe);                                   \
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);                     \
    SHMEM_ERR_CHECK_SYMMETRIC(source, sizeof(TYPE) * nelems); \
    SHMEM_ERR_CHECK_NULL(target, nelems);                     \
    shmem_internal_get(target, source, sizeof(TYPE) * nelems, \
        pe);                                                  \
    shmem_internal_get_wait();                                \
  }

SHMEM_DEFINE_FOR_RMA(SHMEM_DEF_GET)

#define SHMEM_DEF_GET_N(NAME,SIZE) \
  void shmem_get##NAME(void *target, const void *source,   \
                       size_t nelems, int pe)              \
  {                                                        \
    SHMEM_ERR_CHECK_INITIALIZED();                         \
    SHMEM_ERR_CHECK_PE(pe);                                \
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);                  \
    SHMEM_ERR_CHECK_SYMMETRIC(source, (SIZE) * nelems);    \
    SHMEM_ERR_CHECK_NULL(target, nelems);                  \
    shmem_internal_get(target, source, (SIZE)*nelems, pe); \
    shmem_internal_get_wait();                             \
  }

SHMEM_DEFINE_FOR_SIZES(SHMEM_DEF_GET_N)
SHMEM_DEF_GET_N(mem,1)

#define SHMEM_DEF_GET_NBI(STYPE,TYPE) \
  void shmem_##STYPE##_get_nbi(TYPE *target, const TYPE *source, \
                              size_t nelems, int pe)             \
  {                                                              \
    SHMEM_ERR_CHECK_INITIALIZED();                               \
    SHMEM_ERR_CHECK_PE(pe);                                      \
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);                        \
    SHMEM_ERR_CHECK_SYMMETRIC(source, sizeof(TYPE) * nelems);    \
    SHMEM_ERR_CHECK_NULL(target, nelems);                        \
    shmem_internal_get(target, source, sizeof(TYPE)*nelems, pe); \
  }

SHMEM_DEFINE_FOR_RMA(SHMEM_DEF_GET_NBI)

#define SHMEM_DEF_GET_N_NBI(NAME,SIZE) \
  void shmem_get##NAME##_nbi(void *target, const void *source, \
                             size_t nelems, int pe)            \
  {                                                            \
    SHMEM_ERR_CHECK_INITIALIZED();                             \
    SHMEM_ERR_CHECK_PE(pe);                                    \
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);                      \
    SHMEM_ERR_CHECK_SYMMETRIC(source, (SIZE) * nelems);        \
    SHMEM_ERR_CHECK_NULL(target, nelems);                      \
    shmem_internal_get(target, source, (SIZE)*nelems, pe);     \
  }

SHMEM_DEFINE_FOR_SIZES(SHMEM_DEF_GET_N_NBI)
SHMEM_DEF_GET_N_NBI(mem,1)

#define SHMEM_DEF_IPUT(STYPE,TYPE) \
  void shmem_##STYPE##_iput(TYPE *target, const TYPE *source, \
                            ptrdiff_t tst, ptrdiff_t sst,     \
                            size_t nelems, int pe)            \
  {                                                           \
    SHMEM_ERR_CHECK_INITIALIZED();                            \
    SHMEM_ERR_CHECK_PE(pe);                                   \
    SHMEM_ERR_CHECK_POSITIVE(tst);                            \
    SHMEM_ERR_CHECK_POSITIVE(sst);                            \
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);                     \
    SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE) * ((nelems-1) * tst + 1)); \
    SHMEM_ERR_CHECK_NULL(source, nelems);                     \
    for ( ; nelems > 0 ; --nelems) {                          \
      shmem_internal_put_small(target, source, sizeof(TYPE),  \
          pe);                                                \
      target += tst;                                          \
      source += sst;                                          \
    }                                                         \
  }

SHMEM_DEFINE_FOR_RMA(SHMEM_DEF_IPUT)

#define SHMEM_DEF_IPUT_N(NAME,SIZE) \
  void shmem_iput##NAME(void *target, const void *source,    \
                        ptrdiff_t tst, ptrdiff_t sst,        \
                        size_t nelems, int pe)               \
  {                                                          \
    SHMEM_ERR_CHECK_INITIALIZED();                           \
    SHMEM_ERR_CHECK_PE(pe);                                  \
    SHMEM_ERR_CHECK_POSITIVE(tst);                           \
    SHMEM_ERR_CHECK_POSITIVE(sst);                           \
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);                    \
    SHMEM_ERR_CHECK_SYMMETRIC(target, SIZE * ((nelems-1) * tst + 1)); \
    SHMEM_ERR_CHECK_NULL(source, nelems);                    \
    for ( ; nelems > 0 ; --nelems) {                         \
      shmem_internal_put_small(target, source, (SIZE), pe);  \
      target = (uint8_t*)target + tst*(SIZE);                \
      source = (uint8_t*)source + sst*(SIZE);                \
    }                                                        \
  }

SHMEM_DEFINE_FOR_SIZES(SHMEM_DEF_IPUT_N)

#define SHMEM_DEF_IGET(STYPE,TYPE) \
  void shmem_##STYPE##_iget(TYPE *target, const TYPE *source, \
                            ptrdiff_t tst, ptrdiff_t sst,     \
                            size_t nelems, int pe)            \
  {                                                           \
    SHMEM_ERR_CHECK_INITIALIZED();                            \
    SHMEM_ERR_CHECK_PE(pe);                                   \
    SHMEM_ERR_CHECK_POSITIVE(tst);                            \
    SHMEM_ERR_CHECK_POSITIVE(sst);                            \
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);                     \
    SHMEM_ERR_CHECK_SYMMETRIC(source, sizeof(TYPE) * ((nelems-1) * sst + 1)); \
    SHMEM_ERR_CHECK_NULL(target, nelems);                     \
    for ( ; nelems > 0 ; --nelems) {                          \
      shmem_internal_get(target, source, sizeof(TYPE), pe);   \
      target += tst;                                          \
      source += sst;                                          \
    }                                                         \
    shmem_internal_get_wait();                                \
  }

SHMEM_DEFINE_FOR_RMA(SHMEM_DEF_IGET)

#define SHMEM_DEF_IGET_N(NAME,SIZE) \
  void shmem_iget##NAME(void *target, const void *source, \
                        ptrdiff_t tst, ptrdiff_t sst,     \
                        size_t nelems, int pe)            \
  {                                                       \
    SHMEM_ERR_CHECK_INITIALIZED();                        \
    SHMEM_ERR_CHECK_PE(pe);                               \
    SHMEM_ERR_CHECK_POSITIVE(tst);                        \
    SHMEM_ERR_CHECK_POSITIVE(sst);                        \
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);                 \
    SHMEM_ERR_CHECK_SYMMETRIC(source, SIZE * ((nelems-1) * sst + 1)); \
    SHMEM_ERR_CHECK_NULL(target, nelems);                 \
    for ( ; nelems > 0 ; --nelems) {                      \
      shmem_internal_get(target, source, (SIZE), pe);     \
      target = (uint8_t*)target + tst*(SIZE);             \
      source = (uint8_t*)source + sst*(SIZE);             \
    }                                                     \
    shmem_internal_get_wait();                            \
  }

SHMEM_DEFINE_FOR_SIZES(SHMEM_DEF_IGET_N)

void
shmemx_getmem_ct(shmemx_ct_t ct, void *target, const void *source, size_t nelems, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(pe);
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);
    SHMEM_ERR_CHECK_SYMMETRIC(source, nelems);
    SHMEM_ERR_CHECK_NULL(target, nelems);

    shmem_internal_get_ct(ct, target, source, nelems, pe);
    shmem_internal_get_wait();
}

void shmemx_putmem_ct(shmemx_ct_t ct, void *target, const void *source,
                     size_t nelems, int pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(pe);
    SHMEM_ERR_CHECK_NON_NEGATIVE(nelems);
    SHMEM_ERR_CHECK_SYMMETRIC(target, nelems);
    SHMEM_ERR_CHECK_NULL(source, nelems);

    shmem_internal_put_ct_nb(ct, target, source, nelems, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void shmemx_ct_create(shmemx_ct_t *ct)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_ct_create(ct);
}


void shmemx_ct_free(shmemx_ct_t *ct)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_ct_free(ct);
}


long shmemx_ct_get(shmemx_ct_t ct)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    return shmem_internal_ct_get(ct);
}


void shmemx_ct_set(shmemx_ct_t ct, long value)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_ct_set(ct, value);
}


void shmemx_ct_wait(shmemx_ct_t ct, long wait_for)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_ct_wait(ct, wait_for);
}
