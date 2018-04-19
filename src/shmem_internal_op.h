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

FUNC_OP_CREATE(long_double, long double, max, shmem_internal_max_op)
FUNC_OP_CREATE(long_double, long double, min, shmem_internal_min_op)
FUNC_OP_CREATE(long_double, long double, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(long_double, long double, prod, shmem_internal_prod_op)

FUNC_OP_CREATE(double_complex, double _Complex, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(double_complex, double _Complex, prod, shmem_internal_prod_op)

FUNC_OP_CREATE(float_complex, float _Complex, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(float_complex, float _Complex, prod, shmem_internal_prod_op)

FUNC_OP_CREATE(double, double, max, shmem_internal_max_op)
FUNC_OP_CREATE(double, double, min, shmem_internal_min_op)
FUNC_OP_CREATE(double, double, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(double, double, prod, shmem_internal_prod_op)

FUNC_OP_CREATE(float, float, max, shmem_internal_max_op)
FUNC_OP_CREATE(float, float, min, shmem_internal_min_op)
FUNC_OP_CREATE(float, float, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(float, float, prod, shmem_internal_prod_op)

FUNC_OP_CREATE(int64, int64_t, max, shmem_internal_max_op)
FUNC_OP_CREATE(int64, int64_t, min, shmem_internal_min_op)
FUNC_OP_CREATE(int64, int64_t, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(int64, int64_t, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(int64, int64_t, and, shmem_internal_and_op)
FUNC_OP_CREATE(int64, int64_t, or, shmem_internal_or_op)
FUNC_OP_CREATE(int64, int64_t, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(int32, int32_t, max, shmem_internal_max_op)
FUNC_OP_CREATE(int32, int32_t, min, shmem_internal_min_op)
FUNC_OP_CREATE(int32, int32_t, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(int32, int32_t, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(int32, int32_t, and, shmem_internal_and_op)
FUNC_OP_CREATE(int32, int32_t, or, shmem_internal_or_op)
FUNC_OP_CREATE(int32, int32_t, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(short, short, max, shmem_internal_max_op)
FUNC_OP_CREATE(short, short, min, shmem_internal_min_op)
FUNC_OP_CREATE(short, short, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(short, short, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(short, short, and, shmem_internal_and_op)
FUNC_OP_CREATE(short, short, or, shmem_internal_or_op)
FUNC_OP_CREATE(short, short, xor, shmem_internal_xor_op)

FUNC_OP_CREATE(int8, int8_t, max, shmem_internal_max_op)
FUNC_OP_CREATE(int8, int8_t, min, shmem_internal_min_op)
FUNC_OP_CREATE(int8, int8_t, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(int8, int8_t, prod, shmem_internal_prod_op)
FUNC_OP_CREATE(int8, int8_t, and, shmem_internal_and_op)
FUNC_OP_CREATE(int8, int8_t, or, shmem_internal_or_op)
FUNC_OP_CREATE(int8, int8_t, xor, shmem_internal_xor_op)

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

static inline void shmem_internal_reduce_local(shm_internal_op_t op,
                                shm_internal_datatype_t datatype, int count,
                                void *in, void *inout) {
    switch(datatype) {
        REDUCE_LOCAL_DTYPE_CASE_FP(SHM_INTERNAL_LONG_DOUBLE, long_double, long double);
        REDUCE_LOCAL_DTYPE_CASE_FP(SHM_INTERNAL_DOUBLE, double, double);
        REDUCE_LOCAL_DTYPE_CASE_FP(SHM_INTERNAL_FLOAT, float, float);
        REDUCE_LOCAL_DTYPE_CASE_CPLX(SHM_INTERNAL_DOUBLE_COMPLEX, double_complex, double _Complex);
        REDUCE_LOCAL_DTYPE_CASE_CPLX(SHM_INTERNAL_FLOAT_COMPLEX, float_complex, float _Complex);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_INT64, int64, int64_t);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_INT32, int32, int32_t);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_SHORT, short, short);
        REDUCE_LOCAL_DTYPE_CASE_INT(SHM_INTERNAL_SIGNED_BYTE, int8, int8_t);

        default:
            RAISE_ERROR_MSG("invalid data type (%d)", (int) datatype);
    }
}

#undef REDUCE_LOCAL_DTYPE_CASE_FP
#undef REDUCE_LOCAL_DTYPE_CASE_CPLX
#undef REDUCE_LOCAL_DTYPE_CASE_INT
