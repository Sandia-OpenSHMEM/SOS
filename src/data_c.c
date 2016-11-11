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

#ifdef ENABLE_PROFILING
#include "pshmem.h"

#pragma weak shmem_float_p = pshmem_float_p
#define shmem_float_p pshmem_float_p

#pragma weak shmem_double_p = pshmem_double_p
#define shmem_double_p pshmem_double_p

#pragma weak shmem_longdouble_p = pshmem_longdouble_p
#define shmem_longdouble_p pshmem_longdouble_p

#pragma weak shmem_char_p = pshmem_char_p
#define shmem_char_p pshmem_char_p

#pragma weak shmem_short_p = pshmem_short_p
#define shmem_short_p pshmem_short_p

#pragma weak shmem_int_p = pshmem_int_p
#define shmem_int_p pshmem_int_p

#pragma weak shmem_long_p = pshmem_long_p
#define shmem_long_p pshmem_long_p

#pragma weak shmem_longlong_p = pshmem_longlong_p
#define shmem_longlong_p pshmem_longlong_p

#pragma weak shmem_float_g = pshmem_float_g
#define shmem_float_g pshmem_float_g

#pragma weak shmem_double_g = pshmem_double_g
#define shmem_double_g pshmem_double_g

#pragma weak shmem_char_g = pshmem_char_g
#define shmem_char_g pshmem_char_g

#pragma weak shmem_short_g = pshmem_short_g
#define shmem_short_g pshmem_short_g

#pragma weak shmem_int_g = pshmem_int_g
#define shmem_int_g pshmem_int_g

#pragma weak shmem_long_g = pshmem_long_g
#define shmem_long_g pshmem_long_g

#pragma weak shmem_longdouble_g = pshmem_longdouble_g
#define shmem_longdouble_g pshmem_longdouble_g

#pragma weak shmem_longlong_g = pshmem_longlong_g
#define shmem_longlong_g pshmem_longlong_g

#pragma weak shmem_float_put = pshmem_float_put
#define shmem_float_put pshmem_float_put

#pragma weak shmem_double_put = pshmem_double_put
#define shmem_double_put pshmem_double_put

#pragma weak shmem_longdouble_put = pshmem_longdouble_put
#define shmem_longdouble_put pshmem_longdouble_put

#pragma weak shmem_char_put = pshmem_char_put
#define shmem_char_put pshmem_char_put

#pragma weak shmem_short_put = pshmem_short_put
#define shmem_short_put pshmem_short_put

#pragma weak shmem_int_put = pshmem_int_put
#define shmem_int_put pshmem_int_put

#pragma weak shmem_long_put = pshmem_long_put
#define shmem_long_put pshmem_long_put

#pragma weak shmem_longlong_put = pshmem_longlong_put
#define shmem_longlong_put pshmem_longlong_put

#pragma weak shmem_put8 = pshmem_put8
#define shmem_put8 pshmem_put8

#pragma weak shmem_put16 = pshmem_put16
#define shmem_put16 pshmem_put16

#pragma weak shmem_put32 = pshmem_put32
#define shmem_put32 pshmem_put32

#pragma weak shmem_put64 = pshmem_put64
#define shmem_put64 pshmem_put64

#pragma weak shmem_put128 = pshmem_put128
#define shmem_put128 pshmem_put128

#pragma weak shmem_putmem = pshmem_putmem
#define shmem_putmem pshmem_putmem

#pragma weak shmem_float_get = pshmem_float_get
#define shmem_float_get pshmem_float_get

#pragma weak shmem_double_get = pshmem_double_get
#define shmem_double_get pshmem_double_get

#pragma weak shmem_longdouble_get = pshmem_longdouble_get
#define shmem_longdouble_get pshmem_longdouble_get

#pragma weak shmem_char_get = pshmem_char_get
#define shmem_char_get pshmem_char_get

#pragma weak shmem_short_get = pshmem_short_get
#define shmem_short_get pshmem_short_get

#pragma weak shmem_int_get = pshmem_int_get
#define shmem_int_get pshmem_int_get

#pragma weak shmem_long_get = pshmem_long_get
#define shmem_long_get pshmem_long_get

#pragma weak shmem_longlong_get = pshmem_longlong_get
#define shmem_longlong_get pshmem_longlong_get

#pragma weak shmem_get8 = pshmem_get8
#define shmem_get8 pshmem_get8

#pragma weak shmem_get16 = pshmem_get16
#define shmem_get16 pshmem_get16

#pragma weak shmem_get32 = pshmem_get32
#define shmem_get32 pshmem_get32

#pragma weak shmem_get64 = pshmem_get64
#define shmem_get64 pshmem_get64

#pragma weak shmem_get128 = pshmem_get128
#define shmem_get128 pshmem_get128

#pragma weak shmem_getmem = pshmem_getmem
#define shmem_getmem pshmem_getmem

#pragma weak shmem_float_iput = pshmem_float_iput
#define shmem_float_iput pshmem_float_iput

#pragma weak shmem_double_iput = pshmem_double_iput
#define shmem_double_iput pshmem_double_iput

#pragma weak shmem_longdouble_iput = pshmem_longdouble_iput
#define shmem_longdouble_iput pshmem_longdouble_iput

#pragma weak shmem_char_iput = pshmem_char_iput
#define shmem_char_iput pshmem_char_iput

#pragma weak shmem_short_iput = pshmem_short_iput
#define shmem_short_iput pshmem_short_iput

#pragma weak shmem_int_iput = pshmem_int_iput
#define shmem_int_iput pshmem_int_iput

#pragma weak shmem_long_iput = pshmem_long_iput
#define shmem_long_iput pshmem_long_iput

#pragma weak shmem_longlong_iput = pshmem_longlong_iput
#define shmem_longlong_iput pshmem_longlong_iput

#pragma weak shmem_iput32 = pshmem_iput32
#define shmem_iput32 pshmem_iput32

#pragma weak shmem_iput64 = pshmem_iput64
#define shmem_iput64 pshmem_iput64

#pragma weak shmem_iput128 = pshmem_iput128
#define shmem_iput128 pshmem_iput128

#pragma weak shmem_float_iget = pshmem_float_iget
#define shmem_float_iget pshmem_float_iget

#pragma weak shmem_double_iget = pshmem_double_iget
#define shmem_double_iget pshmem_double_iget

#pragma weak shmem_longdouble_iget = pshmem_longdouble_iget
#define shmem_longdouble_iget pshmem_longdouble_iget

#pragma weak shmem_char_iget = pshmem_char_iget
#define shmem_char_iget pshmem_char_iget

#pragma weak shmem_short_iget = pshmem_short_iget
#define shmem_short_iget pshmem_short_iget

#pragma weak shmem_int_iget = pshmem_int_iget
#define shmem_int_iget pshmem_int_iget

#pragma weak shmem_long_iget = pshmem_long_iget
#define shmem_long_iget pshmem_long_iget

#pragma weak shmem_longlong_iget = pshmem_longlong_iget
#define shmem_longlong_iget pshmem_longlong_iget

#pragma weak shmem_iget32 = pshmem_iget32
#define shmem_iget32 pshmem_iget32

#pragma weak shmem_iget64 = pshmem_iget64
#define shmem_iget64 pshmem_iget64

#pragma weak shmem_iget128 = pshmem_iget128
#define shmem_iget128 pshmem_iget128

#pragma weak shmem_float_put_nbi = pshmem_float_put_nbi
#define shmem_float_put_nbi pshmem_float_put_nbi

#pragma weak shmem_double_put_nbi = pshmem_double_put_nbi
#define shmem_double_put_nbi pshmem_double_put_nbi

#pragma weak shmem_longdouble_put_nbi = pshmem_longdouble_put_nbi
#define shmem_longdouble_put_nbi pshmem_longdouble_put_nbi

#pragma weak shmem_char_put_nbi = pshmem_char_put_nbi
#define shmem_char_put_nbi pshmem_char_put_nbi

#pragma weak shmem_putmem_nbi = pshmem_putmem_nbi
#define shmem_putmem_nbi pshmem_putmem_nbi

#pragma weak shmem_short_put_nbi = pshmem_short_put_nbi
#define shmem_short_put_nbi pshmem_short_put_nbi

#pragma weak shmem_int_put_nbi = pshmem_int_put_nbi
#define shmem_int_put_nbi pshmem_int_put_nbi

#pragma weak shmem_long_put_nbi = pshmem_long_put_nbi
#define shmem_long_put_nbi pshmem_long_put_nbi

#pragma weak shmem_longlong_put_nbi = pshmem_longlong_put_nbi
#define shmem_longlong_put_nbi pshmem_longlong_put_nbi

#pragma weak shmem_put8_nbi = pshmem_put8_nbi
#define shmem_put8_nbi pshmem_put8_nbi

#pragma weak shmem_put16_nbi = pshmem_put16_nbi
#define shmem_put16_nbi pshmem_put16_nbi

#pragma weak shmem_put32_nbi = pshmem_put32_nbi
#define shmem_put32_nbi pshmem_put32_nbi

#pragma weak shmem_put64_nbi = pshmem_put64_nbi
#define shmem_put64_nbi pshmem_put64_nbi

#pragma weak shmem_put128_nbi = pshmem_put128_nbi
#define shmem_put128_nbi pshmem_put128_nbi

#pragma weak shmem_float_get_nbi = pshmem_float_get_nbi
#define shmem_float_get_nbi pshmem_float_get_nbi

#pragma weak shmem_double_get_nbi = pshmem_double_get_nbi
#define shmem_double_get_nbi pshmem_double_get_nbi

#pragma weak shmem_longdouble_get_nbi = pshmem_longdouble_get_nbi
#define shmem_longdouble_get_nbi pshmem_longdouble_get_nbi

#pragma weak shmem_char_get_nbi = pshmem_char_get_nbi
#define shmem_char_get_nbi pshmem_char_get_nbi

#pragma weak shmem_getmem_nbi = pshmem_getmem_nbi
#define shmem_getmem_nbi pshmem_getmem_nbi

#pragma weak shmem_short_get_nbi = pshmem_short_get_nbi
#define shmem_short_get_nbi pshmem_short_get_nbi

#pragma weak shmem_int_get_nbi = pshmem_int_get_nbi
#define shmem_int_get_nbi pshmem_int_get_nbi

#pragma weak shmem_long_get_nbi = pshmem_long_get_nbi
#define shmem_long_get_nbi pshmem_long_get_nbi

#pragma weak shmem_longlong_get_nbi = pshmem_longlong_get_nbi
#define shmem_longlong_get_nbi pshmem_longlong_get_nbi

#pragma weak shmem_get8_nbi = pshmem_get8_nbi
#define shmem_get8_nbi pshmem_get8_nbi

#pragma weak shmem_get16_nbi = pshmem_get16_nbi
#define shmem_get16_nbi pshmem_get16_nbi

#pragma weak shmem_get32_nbi = pshmem_get32_nbi
#define shmem_get32_nbi pshmem_get32_nbi

#pragma weak shmem_get64_nbi = pshmem_get64_nbi
#define shmem_get64_nbi pshmem_get64_nbi

#pragma weak shmem_get128_nbi = pshmem_get128_nbi
#define shmem_get128_nbi pshmem_get128_nbi

#pragma weak shmemx_putmem_ct = pshmemx_putmem_ct
#define shmemx_putmem_ct pshmemx_putmem_ct

#pragma weak shmemx_getmem_ct = pshmemx_getmem_ct
#define shmemx_getmem_ct pshmemx_getmem_ct

#pragma weak shmemx_ct_create = pshmemx_ct_create
#define shmemx_ct_create pshmemx_ct_create

#pragma weak shmemx_ct_free = pshmemx_ct_free
#define shmemx_ct_free pshmemx_ct_free

#pragma weak shmemx_ct_get = pshmemx_ct_get
#define shmemx_ct_get pshmemx_ct_get

#pragma weak shmemx_ct_set = pshmemx_ct_set
#define shmemx_ct_set pshmemx_ct_set

#pragma weak shmemx_ct_wait = pshmemx_ct_wait
#define shmemx_ct_wait pshmemx_ct_wait

#endif /* ENABLE_PROFILING */

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
