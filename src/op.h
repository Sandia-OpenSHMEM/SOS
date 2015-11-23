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

#define FUNC_OP_CREATE(name, type_name, type, calc)  \
    void shmem_op_##name##_##type_name##_func(void *in, void *out, int count); \
    void shmem_op_##name##_##type_name##_func(void *in, void *out, int count) \
    {                                                                       \
        int i;                                                              \
        type *a = (type *) in;                                              \
        type *b = (type *) out;                                             \
        for (i = 0; i < count; ++i) {                                       \
            *(b) = calc(*(b), *(a));                                        \
            ++b;                                                            \
            ++a;                                                            \
        }                                                                   \
    }


 #define FUNC_CALL(op_name, type_name, in, out, count) \
	shmem_op_##op_name##_##type_name##_func(in, out, count); \



/* MAX */
#define __max_op(a, b) ((a) > (b) ? (a) : (b))
FUNC_OP_CREATE(max, longdouble, long double, __max_op)

/* MIN */
#define __min_op(a, b) ((a) < (b) ? (a) : (b))
FUNC_OP_CREATE(min, longdouble, long double, __min_op)

/* SUM */
#define __sum_op(a, b) ((a) + (b))
FUNC_OP_CREATE(sum, longdouble, long double, __sum_op)

/* PROD */
#define __prod_op(a, b) ((a) * (b))
FUNC_OP_CREATE(prod, longdouble, long double, __prod_op)

/*simple implementation: DT_CMPxOPS, DT_INTxBOPS */
void static inline shmem_op_tls(shm_internal_op_t op, shm_internal_datatype_t datatype,
			int count, void * target, void* current_target) {

	switch(op) {
	case SHM_INTERNAL_MIN:
		FUNC_CALL(min, longdouble, (long double *) target, (long double *) current_target, count);
		break;
	case SHM_INTERNAL_MAX:
		FUNC_CALL(max, longdouble, (long double *) target, (long double *) current_target, count);
		break;
	case SHM_INTERNAL_SUM:
		FUNC_CALL(sum, longdouble, (long double *) target, (long double *) current_target, count);
		break;
	case SHM_INTERNAL_PROD:
		FUNC_CALL(prod, longdouble, (long double *) target, (long double *) current_target, count);
		break;
	default:
		fprintf(stderr,"op type not supported =( \n");
		exit(1);
	}
}
