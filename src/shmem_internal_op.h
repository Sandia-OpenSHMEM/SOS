/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2017 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * Copyright (c) 2013 Mellanox Technologies, Inc. All rights reserved.
 *
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 *
 * This file is part of the Portals SHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include <stdint.h>
#include "transport.h"

#define FUNC_OP_CREATE(type_name, c_type, op_name, calc)                    \
    static inline void shmem_op_##type_name##_##op_name##_func(c_type *in,  \
                                                    c_type *out, int count) \
    {                                                                       \
        int i;                                                              \
        for (i = 0; i < count; ++i) {                                       \
            *(out) = calc(*(out), *(in));                                   \
            ++out;                                                          \
            ++in;                                                           \
        }                                                                   \
    }


/* Open SHMEM reduction operations */
#define shmem_internal_max_op(a, b) ((a) > (b) ? (a) : (b))
#define shmem_internal_min_op(a, b) ((a) < (b) ? (a) : (b))
#define shmem_internal_sum_op(a, b) ((a) + (b))
#define shmem_internal_prod_op(a, b) ((a) * (b))
#define shmem_internal_and_op(a, b) ((a) & (b))
#define shmem_internal_or_op(a, b) ((a) | (b))
#define shmem_internal_xor_op(a, b) ((a) ^ (b))

FUNC_OP_CREATE(char, char, max, shmem_internal_max_op)
FUNC_OP_CREATE(char, char, min, shmem_internal_min_op)
FUNC_OP_CREATE(char, char, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(char, char, prod, shmem_internal_prod_op)

FUNC_OP_CREATE(schar, signed char, max, shmem_internal_max_op)
FUNC_OP_CREATE(schar, signed char, min, shmem_internal_and_op)
FUNC_OP_CREATE(schar, signed char, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(schar, signed char, prod, shmem_internal_prod_op)

FUNC_OP_CREATE(uchar, unsigned char, and, shmem_internal_and_op)
FUNC_OP_CREATE(uchar, unsigned char, or, shmem_internal_or_op)
FUNC_OP_CREATE(uchar, unsigned char, xor, shmem_internal_xor_op)
FUNC_OP_CREATE(uchar, unsigned char, max, shmem_internal_max_op)
FUNC_OP_CREATE(uchar, unsigned char, min, shmem_internal_min_op)
FUNC_OP_CREATE(uchar, unsigned char, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(uchar, unsigned char, prod, shmem_internal_prod_op)

FUNC_OP_CREATE(short, short, max, shmem_internal_max_op)
FUNC_OP_CREATE(short, short, min, shmem_internal_min_op)
FUNC_OP_CREATE(short, short, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(short, short, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(short, short, and, shmem_internal_and_op)
FUNC_OP_CREATE(short, short, or, shmem_internal_or_op)
FUNC_OP_CREATE(short, short, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(ushort, unsigned short, max, shmem_internal_max_op)
FUNC_OP_CREATE(ushort, unsigned short, min, shmem_internal_min_op)
FUNC_OP_CREATE(ushort, unsigned short, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(ushort, unsigned short, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(ushort, unsigned short, and, shmem_internal_and_op)
FUNC_OP_CREATE(ushort, unsigned short, or, shmem_internal_or_op)
FUNC_OP_CREATE(ushort, unsigned short, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(int, int, max, shmem_internal_max_op)
FUNC_OP_CREATE(int, int, min, shmem_internal_min_op)
FUNC_OP_CREATE(int, int, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(int, int, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(int, int, and, shmem_internal_and_op)
FUNC_OP_CREATE(int, int, or, shmem_internal_or_op)
FUNC_OP_CREATE(int, int, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(uint, unsigned int, max, shmem_internal_max_op)
FUNC_OP_CREATE(uint, unsigned int, min, shmem_internal_min_op)
FUNC_OP_CREATE(uint, unsigned int, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(uint, unsigned int, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(uint, unsigned int, and, shmem_internal_and_op)
FUNC_OP_CREATE(uint, unsigned int, or, shmem_internal_or_op)
FUNC_OP_CREATE(uint, unsigned int, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(long, long, max, shmem_internal_max_op)
FUNC_OP_CREATE(long, long, min, shmem_internal_min_op)
FUNC_OP_CREATE(long, long, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(long, long, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(long, long, and, shmem_internal_and_op)
FUNC_OP_CREATE(long, long, or, shmem_internal_or_op)
FUNC_OP_CREATE(long, long, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(ulong, unsigned long, max, shmem_internal_max_op)
FUNC_OP_CREATE(ulong, unsigned long, min, shmem_internal_min_op)
FUNC_OP_CREATE(ulong, unsigned long, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(ulong, unsigned long, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(ulong, unsigned long, and, shmem_internal_and_op)
FUNC_OP_CREATE(ulong, unsigned long, or, shmem_internal_or_op)
FUNC_OP_CREATE(ulong, unsigned long, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(longlong, long long, max, shmem_internal_max_op)
FUNC_OP_CREATE(longlong, long long, min, shmem_internal_min_op)
FUNC_OP_CREATE(longlong, long long, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(longlong, long long, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(longlong, long long, and, shmem_internal_and_op)
FUNC_OP_CREATE(longlong, long long, or, shmem_internal_or_op)
FUNC_OP_CREATE(longlong, long long, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(ptrdiff, ptrdiff_t, max, shmem_internal_max_op)
FUNC_OP_CREATE(ptrdiff, ptrdiff_t, min, shmem_internal_min_op)
FUNC_OP_CREATE(ptrdiff, ptrdiff_t, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(ptrdiff, ptrdiff_t, prod, shmem_internal_prod_op)

FUNC_OP_CREATE(ulonglong, unsigned long long, max, shmem_internal_max_op)
FUNC_OP_CREATE(ulonglong, unsigned long long, min, shmem_internal_min_op)
FUNC_OP_CREATE(ulonglong, unsigned long long, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(ulonglong, unsigned long long, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(ulonglong, unsigned long long, and, shmem_internal_and_op)
FUNC_OP_CREATE(ulonglong, unsigned long long, or, shmem_internal_or_op)
FUNC_OP_CREATE(ulonglong, unsigned long long, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(int8, int8_t, max, shmem_internal_max_op)
FUNC_OP_CREATE(int8, int8_t, min, shmem_internal_min_op)
FUNC_OP_CREATE(int8, int8_t, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(int8, int8_t, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(int8, int8_t, and, shmem_internal_and_op)
FUNC_OP_CREATE(int8, int8_t, or, shmem_internal_or_op)
FUNC_OP_CREATE(int8, int8_t, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(int16, int16_t, max, shmem_internal_max_op)
FUNC_OP_CREATE(int16, int16_t, min, shmem_internal_min_op)
FUNC_OP_CREATE(int16, int16_t, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(int16, int16_t, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(int16, int16_t, and, shmem_internal_and_op)
FUNC_OP_CREATE(int16, int16_t, or, shmem_internal_or_op)
FUNC_OP_CREATE(int16, int16_t, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(int32, int32_t, max, shmem_internal_max_op)
FUNC_OP_CREATE(int32, int32_t, min, shmem_internal_min_op)
FUNC_OP_CREATE(int32, int32_t, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(int32, int32_t, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(int32, int32_t, and, shmem_internal_and_op)
FUNC_OP_CREATE(int32, int32_t, or, shmem_internal_or_op)
FUNC_OP_CREATE(int32, int32_t, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(int64, int64_t, max, shmem_internal_max_op)
FUNC_OP_CREATE(int64, int64_t, min, shmem_internal_min_op)
FUNC_OP_CREATE(int64, int64_t, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(int64, int64_t, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(int64, int64_t, and, shmem_internal_and_op)
FUNC_OP_CREATE(int64, int64_t, or, shmem_internal_or_op)
FUNC_OP_CREATE(int64, int64_t, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(uint8, uint8_t, max, shmem_internal_max_op)
FUNC_OP_CREATE(uint8, uint8_t, min, shmem_internal_min_op)
FUNC_OP_CREATE(uint8, uint8_t, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(uint8, uint8_t, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(uint8, uint8_t, and, shmem_internal_and_op)
FUNC_OP_CREATE(uint8, uint8_t, or, shmem_internal_or_op)
FUNC_OP_CREATE(uint8, uint8_t, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(uint16, uint16_t, max, shmem_internal_max_op)
FUNC_OP_CREATE(uint16, uint16_t, min, shmem_internal_min_op)
FUNC_OP_CREATE(uint16, uint16_t, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(uint16, uint16_t, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(uint16, uint16_t, and, shmem_internal_and_op)
FUNC_OP_CREATE(uint16, uint16_t, or, shmem_internal_or_op)
FUNC_OP_CREATE(uint16, uint16_t, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(uint32, uint32_t, max, shmem_internal_max_op)
FUNC_OP_CREATE(uint32, uint32_t, min, shmem_internal_min_op)
FUNC_OP_CREATE(uint32, uint32_t, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(uint32, uint32_t, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(uint32, uint32_t, and, shmem_internal_and_op)
FUNC_OP_CREATE(uint32, uint32_t, or, shmem_internal_or_op)
FUNC_OP_CREATE(uint32, uint32_t, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(uint64, uint64_t, max, shmem_internal_max_op)
FUNC_OP_CREATE(uint64, uint64_t, min, shmem_internal_min_op)
FUNC_OP_CREATE(uint64, uint64_t, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(uint64, uint64_t, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(uint64, uint64_t, and, shmem_internal_and_op)
FUNC_OP_CREATE(uint64, uint64_t, or, shmem_internal_or_op)
FUNC_OP_CREATE(uint64, uint64_t, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(size, size_t, max, shmem_internal_max_op)
FUNC_OP_CREATE(size, size_t, min, shmem_internal_min_op)
FUNC_OP_CREATE(size, size_t, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(size, size_t, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(size, size_t, and, shmem_internal_and_op)
FUNC_OP_CREATE(size, size_t, or, shmem_internal_or_op)
FUNC_OP_CREATE(size, size_t, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(float, float, max, shmem_internal_max_op)
FUNC_OP_CREATE(float, float, min, shmem_internal_min_op)
FUNC_OP_CREATE(float, float, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(float, float, prod, shmem_internal_prod_op)

FUNC_OP_CREATE(double, double, max, shmem_internal_max_op)
FUNC_OP_CREATE(double, double, min, shmem_internal_min_op)
FUNC_OP_CREATE(double, double, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(double, double, prod, shmem_internal_prod_op)

FUNC_OP_CREATE(long_double, long double, max, shmem_internal_max_op)
FUNC_OP_CREATE(long_double, long double, min, shmem_internal_min_op)
FUNC_OP_CREATE(long_double, long double, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(long_double, long double, prod, shmem_internal_prod_op)

FUNC_OP_CREATE(double_complex, double _Complex, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(double_complex, double _Complex, prod, shmem_internal_prod_op)

FUNC_OP_CREATE(float_complex, float _Complex, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(float_complex, float _Complex, prod, shmem_internal_prod_op)

#define REDUCE_LOCAL_DTYPE_CASE_FP(dtype, dtype_name, c_type)                             \
    case dtype:                                                                           \
        switch(op) {                                                                      \
            case SHM_INTERNAL_MIN:                                                        \
                shmem_op_##dtype_name##_min_func((c_type *) in, (c_type *) inout, count); \
                break;                                                                    \
            case SHM_INTERNAL_MAX:                                                        \
                shmem_op_##dtype_name##_max_func((c_type *) in, (c_type *) inout, count); \
                break;                                                                    \
            case SHM_INTERNAL_SUM:                                                        \
                shmem_op_##dtype_name##_sum_func((c_type *) in, (c_type *) inout, count); \
                break;                                                                    \
            case SHM_INTERNAL_PROD:                                                       \
                shmem_op_##dtype_name##_prod_func((c_type *) in, (c_type *) inout, count);\
                break;                                                                    \
            default:                                                                      \
                RAISE_ERROR_STR("unsupported reduction on " # c_type);                    \
        }                                                                                 \
        break;

#define REDUCE_LOCAL_DTYPE_CASE_CPLX(dtype, dtype_name, c_type)                           \
    case dtype:                                                                           \
        switch(op) {                                                                      \
            case SHM_INTERNAL_SUM:                                                        \
                shmem_op_##dtype_name##_sum_func((c_type *) in, (c_type *) inout, count); \
                break;                                                                    \
            case SHM_INTERNAL_PROD:                                                       \
                shmem_op_##dtype_name##_prod_func((c_type *) in, (c_type *) inout, count);\
                break;                                                                    \
            default:                                                                      \
                RAISE_ERROR_STR("unsupported reduction on " # c_type);                    \
        }                                                                                 \
        break;

#define REDUCE_LOCAL_DTYPE_CASE_INT(dtype, dtype_name, c_type)                            \
    case dtype:                                                                           \
        switch(op) {                                                                      \
            case SHM_INTERNAL_MIN:                                                        \
                shmem_op_##dtype_name##_min_func((c_type *) in, (c_type *) inout, count); \
                break;                                                                    \
            case SHM_INTERNAL_MAX:                                                        \
                shmem_op_##dtype_name##_max_func((c_type *) in, (c_type *) inout, count); \
                break;                                                                    \
            case SHM_INTERNAL_SUM:                                                        \
                shmem_op_##dtype_name##_sum_func((c_type *) in, (c_type *) inout, count); \
                break;                                                                    \
            case SHM_INTERNAL_PROD:                                                       \
                shmem_op_##dtype_name##_prod_func((c_type *) in, (c_type *) inout, count);\
                break;                                                                    \
            case SHM_INTERNAL_BAND:                                                       \
                shmem_op_##dtype_name##_and_func((c_type *) in, (c_type *) inout, count); \
                break;                                                                    \
            case SHM_INTERNAL_BOR:                                                        \
                shmem_op_##dtype_name##_or_func((c_type *) in, (c_type *) inout, count);  \
                break;                                                                    \
            case SHM_INTERNAL_BXOR:                                                       \
                shmem_op_##dtype_name##_xor_func((c_type *) in, (c_type *) inout, count); \
                break;                                                                    \
            default:                                                                      \
                RAISE_ERROR_STR("unsupported reduction on " # c_type);                    \
        }                                                                                 \
        break;

#define REDUCE_LOCAL_DTYPE_CASE_AND_OR_XOR(dtype, dtype_name, c_type)                     \
    case dtype:                                                                           \
        switch(op) {                                                                      \
            case SHM_INTERNAL_BAND:                                                       \
                shmem_op_##dtype_name##_and_func((c_type *) in, (c_type *) inout, count); \
                break;                                                                    \
            case SHM_INTERNAL_BOR:                                                        \
                shmem_op_##dtype_name##_or_func((c_type *) in, (c_type *) inout, count);  \
                break;                                                                    \
            case SHM_INTERNAL_BXOR:                                                       \
                shmem_op_##dtype_name##_xor_func((c_type *) in, (c_type *) inout, count); \
                break;                                                                    \
            default:                                                                      \
                RAISE_ERROR_STR("unsupported reduction on " # c_type);                    \
        }                                                                                 \
        break;

static inline void shmem_internal_reduce_local(shm_internal_op_t op,
                                shm_internal_datatype_t datatype, int count,
                                void *in, void *inout) {
    switch(datatype) {
        REDUCE_LOCAL_DTYPE_CASE_FP(SHM_INTERNAL_CHAR, char, char);
        REDUCE_LOCAL_DTYPE_CASE_FP(SHM_INTERNAL_SCHAR, schar, signed char);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_SHORT, short, short);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_INT, int, int);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_LONG, long, long);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_LONG_LONG, longlong, long long);
        REDUCE_LOCAL_DTYPE_CASE_FP(SHM_INTERNAL_PTRDIFF_T, ptrdiff, ptrdiff_t);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_UCHAR, uchar, unsigned char);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_USHORT, ushort, unsigned short);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_UINT, uint, unsigned int);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_ULONG, ulong, unsigned long);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_ULONG_LONG, ulonglong, unsigned long long);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_INT8, int8, int8_t);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_INT16, int16, int16_t);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_INT32, int32, int32_t);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_INT64, int64, int64_t);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_UINT8, uint8, uint8_t);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_UINT16, uint16, uint16_t);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_UINT32, uint32, uint32_t);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_UINT64, uint64, uint64_t);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_SIZE_T, size, size_t);
        REDUCE_LOCAL_DTYPE_CASE_FP(SHM_INTERNAL_FLOAT, float, float);
        REDUCE_LOCAL_DTYPE_CASE_FP(SHM_INTERNAL_DOUBLE, double, double);
        REDUCE_LOCAL_DTYPE_CASE_FP(SHM_INTERNAL_LONG_DOUBLE, long_double, long double);
        REDUCE_LOCAL_DTYPE_CASE_CPLX(SHM_INTERNAL_FLOAT_COMPLEX, float_complex, float _Complex);
        REDUCE_LOCAL_DTYPE_CASE_CPLX(SHM_INTERNAL_DOUBLE_COMPLEX, double_complex, double _Complex);

        default:
            RAISE_ERROR_MSG("invalid data type (%d)", (int) datatype);
    }
}

#undef REDUCE_LOCAL_DTYPE_CASE_FP
#undef REDUCE_LOCAL_DTYPE_CASE_CPLX
#undef REDUCE_LOCAL_DTYPE_CASE_INT
