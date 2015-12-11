/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2015 Intel Corporation. All rights reserved.
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

/* Long double reducers */
FUNC_OP_CREATE(long_double, long double, max, shmem_internal_max_op)
FUNC_OP_CREATE(long_double, long double, min, shmem_internal_min_op)
FUNC_OP_CREATE(long_double, long double, sum, shmem_internal_sum_op)
FUNC_OP_CREATE(long_double, long double, prod, shmem_internal_prod_op)


void static inline shmem_internal_reduce_local(shm_internal_op_t op,
                                shm_internal_datatype_t datatype, int count,
                                void *in, void *inout) {

    /* Note: add more type and op pairs as needed */

    switch(datatype) {
        case SHM_INTERNAL_LONG_DOUBLE:
            switch(op) {
                case SHM_INTERNAL_MIN:
                    shmem_op_long_double_min_func((long double *) in, (long double *) inout, count);
                    break;
                case SHM_INTERNAL_MAX:
                    shmem_op_long_double_max_func((long double *) in, (long double *) inout, count);
                    break;
                case SHM_INTERNAL_SUM:
                    shmem_op_long_double_sum_func((long double *) in, (long double *) inout, count);
                    break;
                case SHM_INTERNAL_PROD:
                    shmem_op_long_double_prod_func((long double *) in, (long double *) inout, count);
                    break;
                default:
                    RAISE_ERROR_STR("unsupported reduction on long double");
            }
            break;
        /* FIXME: I'm pretty sure we also need support for 'float complex' and
         * 'double complex' */
        default:

            RAISE_ERROR_STR("unsupported data type");
    }
}
