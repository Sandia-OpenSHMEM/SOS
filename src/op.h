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

#define FUNC_OP_CREATE(name, calc)  \
    void static shmem_op_##name##_func(long double *in, long double *out, int count); \
    void static shmem_op_##name##_func(long double *in, long double *out, int count) \
    {                                                                       \
        int i;                                                              \
        for (i = 0; i < count; ++i) {                                       \
            *(out) = calc(*(out), *(in));                                   \
            ++out;                                                          \
            ++in;                                                           \
        }                                                                   \
    }


/* MAX */
#define shmem_internal_max_op(a, b) ((a) > (b) ? (a) : (b))
FUNC_OP_CREATE(max, shmem_internal_max_op)

/* MIN */
#define shmem_internal_min_op(a, b) ((a) < (b) ? (a) : (b))
FUNC_OP_CREATE(min, shmem_internal_min_op)

/* SUM */
#define shmem_internal_sum_op(a, b) ((a) + (b))
FUNC_OP_CREATE(sum, shmem_internal_sum_op)

/* PROD */
#define shmem_internal_prod_op(a, b) ((a) * (b))
FUNC_OP_CREATE(prod, shmem_internal_prod_op)


/*simple implementation: DT_CMPxOPS, DT_INTxBOPS */
void static inline shmem_op_tls(shm_internal_op_t op, shm_internal_datatype_t datatype,
			int count, void * target, void* current_target) {

	switch(op) {
	case SHM_INTERNAL_MIN:
		shmem_op_min_func((long double *) target, (long double *) current_target, count);
		break;
	case SHM_INTERNAL_MAX:
		shmem_op_max_func((long double *) target, (long double *) current_target, count);
		break;
	case SHM_INTERNAL_SUM:
		shmem_op_sum_func((long double *) target, (long double *) current_target, count);
		break;
	case SHM_INTERNAL_PROD:
		shmem_op_prod_func((long double *) target, (long double *) current_target, count);
		break;
	default:
		fprintf(stderr,"op type not supported =( \n");
		RAISE_ERROR(1);
	}
}
