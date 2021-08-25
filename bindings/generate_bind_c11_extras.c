/* -*- C -*-
 *
 * Copyright (c) 2017 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>
#include "config.h"

#if OPAL_HAVE_ATTRIBUTE_UNUSED
#define VAR_IS_UNUSED __attribute__((unused))
#else
#define VAR_IS_UNUSED
#endif

#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L)

/* Note: Mirrors SHMEM_BIND_C11_RMA */
#define NEED_RMA_ASSOC(VAL)       \
  _Generic((VAL),                 \
           char: 0,               \
           signed char: 0,        \
           short: 0,              \
           int: 0,                \
           long: 0,               \
           long long: 0,          \
           unsigned char: 0,      \
           unsigned short: 0,     \
           unsigned int: 0,       \
           unsigned long: 0,      \
           unsigned long long: 0, \
           float: 0,              \
           double: 0,             \
           long double: 0,        \
           default: 1)

/* Note: Mirrors SHMEM_BIND_C11_AMO */
#define NEED_AMO_ASSOC(VAL)       \
  _Generic((VAL),                 \
           int: 0,                \
           long: 0,               \
           long long: 0,          \
           unsigned int: 0,       \
           unsigned long: 0,      \
           unsigned long long: 0, \
           default: 1)

/* Note: Mirrors SHMEM_BIND_C11_EXTENDED_AMO */
#define NEED_EXTENDED_AMO_ASSOC(VAL) \
  _Generic((VAL),                 \
           int: 0,                \
           long: 0,               \
           long long: 0,          \
           unsigned int: 0,       \
           unsigned long: 0,      \
           unsigned long long: 0, \
           float: 0,              \
           double: 0,             \
           default: 1)

/* Note: Mirrors SHMEM_BIND_C11_BITWISE_AMO */
#define NEED_BITWISE_AMO_ASSOC(VAL) \
  _Generic((VAL),                 \
           int32_t: 0,            \
           int64_t: 0,            \
           unsigned int: 0,       \
           unsigned long: 0,      \
           unsigned long long: 0, \
           default: 1)

/* Note: Mirrors SHMEM_BIND_C11_SYNC */
#define NEED_SYNC_ASSOC(VAL)      \
  _Generic((VAL),                 \
           short: 0,              \
           int: 0,                \
           long: 0,               \
           long long: 0,          \
           unsigned short: 0,     \
           unsigned int: 0,       \
           unsigned long: 0,      \
           unsigned long long: 0, \
           default: 1)

/* Note: Mirrors SHMEM_BIND_C11_COLL_AND_OR_XOR */
#define NEED_BITWISE_REDUCE_ASSOC(VAL) \
  _Generic((VAL),                      \
           unsigned char: 0,           \
           unsigned short: 0,          \
           unsigned int: 0,            \
           unsigned long: 0,           \
           unsigned long long: 0,      \
           default: 1)

/* Note: Mirrors SHMEM_BIND_C11_COLL_MIN_MAX */
#define NEED_MIN_MAX_REDUCE_ASSOC(VAL) \
  _Generic((VAL),                      \
           char: 0,                    \
           signed char: 0,             \
           short: 0,                   \
           int: 0,                     \
           long: 0,                    \
           long long: 0,               \
           unsigned char: 0,           \
           unsigned short: 0,          \
           unsigned int: 0,            \
           unsigned long: 0,           \
           unsigned long long: 0,      \
           float: 0,                   \
           double: 0,                  \
           long double: 0,             \
           default: 1)

/* Note: Mirrors SHMEM_BIND_C11_COLL_SUM_PROD */
#define NEED_SUM_PROD_REDUCE_ASSOC(VAL) \
  _Generic((VAL),                       \
           char: 0,                     \
           signed char: 0,              \
           short: 0,                    \
           int: 0,                      \
           long: 0,                     \
           long long: 0,                \
           unsigned char: 0,            \
           unsigned short: 0,           \
           unsigned int: 0,             \
           unsigned long: 0,            \
           unsigned long long: 0,       \
           float: 0,                   \
           double: 0,                  \
           long double: 0,             \
           default: 1)

#else
/* Compiler does not support C11 _Generic */
#define NEED_RMA_ASSOC(VAL) 0
#define NEED_AMO_ASSOC(VAL) 0
#define NEED_EXTENDED_AMO_ASSOC(VAL) 0
#define NEED_BITWISE_AMO_ASSOC(VAL) 0
#define NEED_SYNC_ASSOC(VAL) 0
#define NEED_BITWISE_REDUCE_ASSOC(VAL) 0
#define NEED_MIN_MAX_REDUCE_ASSOC(VAL) 0
#define NEED_SUM_PROD_REDUCE_ASSOC(VAL) 0

#endif

#define GEN_ASSOC(TYPENAME, CTYPE, TYPE_CLASS)                  \
  do {                                                          \
    CTYPE val VAR_IS_UNUSED;                                    \
    if (NEED_##TYPE_CLASS##_ASSOC(val))                         \
      printf("$1(%s, %s)$2\n", #TYPENAME, #CTYPE);              \
  }                                                             \
  while (0)

#define GEN_AMO_ASSOC(TYPENAME, CTYPE, SHMTYPE, TYPE_CLASS)     \
  do {                                                          \
    CTYPE val VAR_IS_UNUSED;                                    \
    if (NEED_##TYPE_CLASS##_ASSOC(val))                         \
      printf("$1(%s, %s, %s)$2\n", #TYPENAME, #CTYPE, #SHMTYPE);\
  }                                                             \
  while (0)

#define GEN_REDUCE_ASSOC(TYPENAME, CTYPE, SHMTYPE, TYPE_CLASS)  \
  do {                                                          \
    CTYPE val VAR_IS_UNUSED;                                    \
    if (NEED_##TYPE_CLASS##_ASSOC(val))                         \
      printf("$1(%s, %s, %s)$2\n", #TYPENAME, #CTYPE, #SHMTYPE);\
  }                                                             \
  while (0)

int main(int argc, char **argv)
{
    char timestr[100];
    time_t curtime = time(NULL);
    struct tm *t = localtime(&curtime);

    strftime(timestr, sizeof(timestr)-1, "%m-%d-%Y %H:%M", t);

    printf("dnl Generated by %s, %s\n", argv[0], timestr);
    printf("define(`SHMEM_BIND_C11_RMA_EXTRAS',\n`");
    GEN_ASSOC(int8,       int8_t, RMA);
    GEN_ASSOC(int16,     int16_t, RMA);
    GEN_ASSOC(int32,     int32_t, RMA);
    GEN_ASSOC(int64,     int64_t, RMA);
    GEN_ASSOC(uint8,     uint8_t, RMA);
    GEN_ASSOC(uint16,   uint16_t, RMA);
    GEN_ASSOC(uint32,   uint32_t, RMA);
    GEN_ASSOC(uint64,   uint64_t, RMA);
    GEN_ASSOC(size,       size_t, RMA);
    GEN_ASSOC(ptrdiff, ptrdiff_t, RMA);
    printf("')dnl\n");

    printf("define(`SHMEM_BIND_C11_AMO_EXTRAS',\n`");
    GEN_AMO_ASSOC(int32,     int32_t,     SHM_INTERNAL_INT32, AMO);
    GEN_AMO_ASSOC(int64,     int64_t,     SHM_INTERNAL_INT64, AMO);
    GEN_AMO_ASSOC(uint32,   uint32_t,    SHM_INTERNAL_UINT32, AMO);
    GEN_AMO_ASSOC(uint64,   uint64_t,    SHM_INTERNAL_UINT64, AMO);
    GEN_AMO_ASSOC(size,       size_t,    SHM_INTERNAL_SIZE_T, AMO);
    GEN_AMO_ASSOC(ptrdiff, ptrdiff_t, SHM_INTERNAL_PTRDIFF_T, AMO);
    printf("')dnl\n");

    printf("define(`SHMEM_BIND_C11_EXTENDED_AMO_EXTRAS',\n`");
    GEN_AMO_ASSOC(int32,     int32_t,     SHM_INTERNAL_INT32, EXTENDED_AMO);
    GEN_AMO_ASSOC(int64,     int64_t,     SHM_INTERNAL_INT64, EXTENDED_AMO);
    GEN_AMO_ASSOC(uint32,   uint32_t,    SHM_INTERNAL_UINT32, EXTENDED_AMO);
    GEN_AMO_ASSOC(uint64,   uint64_t,    SHM_INTERNAL_UINT64, EXTENDED_AMO);
    GEN_AMO_ASSOC(size,       size_t,    SHM_INTERNAL_SIZE_T, EXTENDED_AMO);
    GEN_AMO_ASSOC(ptrdiff, ptrdiff_t, SHM_INTERNAL_PTRDIFF_T, EXTENDED_AMO);
    printf("')dnl\n");

    printf("define(`SHMEM_BIND_C11_BITWISE_AMO_EXTRAS',\n`");
    GEN_AMO_ASSOC(uint32,   uint32_t,    SHM_INTERNAL_UINT32, BITWISE_AMO);
    GEN_AMO_ASSOC(uint64,   uint64_t,    SHM_INTERNAL_UINT64, BITWISE_AMO);
    printf("')dnl\n");

    printf("define(`SHMEM_BIND_C11_SYNC_EXTRAS',\n`");
    GEN_ASSOC(int32,     int32_t, SYNC);
    GEN_ASSOC(int64,     int64_t, SYNC);
    GEN_ASSOC(uint32,   uint32_t, SYNC);
    GEN_ASSOC(uint64,   uint64_t, SYNC);
    GEN_ASSOC(size,       size_t, SYNC);
    GEN_ASSOC(ptrdiff, ptrdiff_t, SYNC);
    printf("')dnl\n");

    printf("define(`SHMEM_BIND_C11_COLL_AND_OR_XOR_EXTRAS',\n`");
    GEN_REDUCE_ASSOC(int8,       int8_t, SHM_INTERNAL_INT8,   BITWISE_REDUCE);
    GEN_REDUCE_ASSOC(int16,     int16_t, SHM_INTERNAL_INT16,  BITWISE_REDUCE);
    GEN_REDUCE_ASSOC(int32,     int32_t, SHM_INTERNAL_INT32,  BITWISE_REDUCE);
    GEN_REDUCE_ASSOC(int64,     int64_t, SHM_INTERNAL_INT64,  BITWISE_REDUCE);
    GEN_REDUCE_ASSOC(uint32,    uint8_t, SHM_INTERNAL_UINT8,  BITWISE_REDUCE);
    GEN_REDUCE_ASSOC(uint64,   uint16_t, SHM_INTERNAL_UINT16, BITWISE_REDUCE);
    GEN_REDUCE_ASSOC(uint32,   uint32_t, SHM_INTERNAL_UINT32, BITWISE_REDUCE);
    GEN_REDUCE_ASSOC(uint64,   uint64_t, SHM_INTERNAL_UINT64, BITWISE_REDUCE);
    GEN_REDUCE_ASSOC(size,       size_t, SHM_INTERNAL_SIZE_T, BITWISE_REDUCE);
    printf("')dnl\n");

    printf("define(`SHMEM_BIND_C11_COLL_MIN_MAX_EXTRAS',\n`");
    GEN_REDUCE_ASSOC(ptrdiff, ptrdiff_t, SHM_INTERNAL_PTRDIFF_T, MIN_MAX_REDUCE);
    GEN_REDUCE_ASSOC(int8,       int8_t, SHM_INTERNAL_INT8,      MIN_MAX_REDUCE);
    GEN_REDUCE_ASSOC(int16,     int16_t, SHM_INTERNAL_INT16,     MIN_MAX_REDUCE);
    GEN_REDUCE_ASSOC(int32,     int32_t, SHM_INTERNAL_INT32,     MIN_MAX_REDUCE);
    GEN_REDUCE_ASSOC(int64,     int64_t, SHM_INTERNAL_INT64,     MIN_MAX_REDUCE);
    GEN_REDUCE_ASSOC(uint32,    uint8_t, SHM_INTERNAL_UINT8,     MIN_MAX_REDUCE);
    GEN_REDUCE_ASSOC(uint64,   uint16_t, SHM_INTERNAL_UINT16,    MIN_MAX_REDUCE);
    GEN_REDUCE_ASSOC(uint32,   uint32_t, SHM_INTERNAL_UINT32,    MIN_MAX_REDUCE);
    GEN_REDUCE_ASSOC(uint64,   uint64_t, SHM_INTERNAL_UINT64,    MIN_MAX_REDUCE);
    GEN_REDUCE_ASSOC(size,       size_t, SHM_INTERNAL_SIZE_T,    MIN_MAX_REDUCE);
    printf("')dnl\n");

    printf("define(`SHMEM_BIND_C11_COLL_SUM_PROD_EXTRAS',\n`");
    GEN_REDUCE_ASSOC(ptrdiff, ptrdiff_t, SHM_INTERNAL_PTRDIFF_T, SUM_PROD_REDUCE);
    GEN_REDUCE_ASSOC(int8,       int8_t, SHM_INTERNAL_INT8,      SUM_PROD_REDUCE);
    GEN_REDUCE_ASSOC(int16,     int16_t, SHM_INTERNAL_INT16,     SUM_PROD_REDUCE);
    GEN_REDUCE_ASSOC(int32,     int32_t, SHM_INTERNAL_INT32,     SUM_PROD_REDUCE);
    GEN_REDUCE_ASSOC(int64,     int64_t, SHM_INTERNAL_INT64,     SUM_PROD_REDUCE);
    GEN_REDUCE_ASSOC(uint32,    uint8_t, SHM_INTERNAL_UINT8,     SUM_PROD_REDUCE);
    GEN_REDUCE_ASSOC(uint64,   uint16_t, SHM_INTERNAL_UINT16,    SUM_PROD_REDUCE);
    GEN_REDUCE_ASSOC(uint32,   uint32_t, SHM_INTERNAL_UINT32,    SUM_PROD_REDUCE);
    GEN_REDUCE_ASSOC(uint64,   uint64_t, SHM_INTERNAL_UINT64,    SUM_PROD_REDUCE);
    GEN_REDUCE_ASSOC(size,       size_t, SHM_INTERNAL_SIZE_T,    SUM_PROD_REDUCE);
    printf("')dnl\n");

    return 0;
}
