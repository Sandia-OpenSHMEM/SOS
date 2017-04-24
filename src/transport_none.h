/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2016 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef TRANSPORT_NONE_H
#define TRANSPORT_NONE_H

#include "shmem_internal.h"

/* Datatypes */
#define SHM_INTERNAL_FLOAT           -1
#define SHM_INTERNAL_DOUBLE          -2
#define SHM_INTERNAL_LONG_DOUBLE     -3
#define SHM_INTERNAL_FLOAT_COMPLEX   -4
#define SHM_INTERNAL_DOUBLE_COMPLEX  -5
#define SHM_INTERNAL_SIGNED_BYTE     -6
#define SHM_INTERNAL_INT8            -7
#define SHM_INTERNAL_INT16           -8
#define SHM_INTERNAL_INT32           -9
#define SHM_INTERNAL_INT64           -10
#define SHM_INTERNAL_SHORT           DTYPE_SHORT
#define SHM_INTERNAL_INT             DTYPE_INT
#define SHM_INTERNAL_LONG            DTYPE_LONG
#define SHM_INTERNAL_LONG_LONG       DTYPE_LONG_LONG
#define SHM_INTERNAL_FORTRAN_INTEGER DTYPE_FORTRAN_INTEGER

/* Operations */
#define SHM_INTERNAL_BAND            -1
#define SHM_INTERNAL_BOR             -2
#define SHM_INTERNAL_BXOR            -3
#define SHM_INTERNAL_MIN             -4
#define SHM_INTERNAL_MAX             -5
#define SHM_INTERNAL_SUM             -6
#define SHM_INTERNAL_PROD            -7

typedef int shm_internal_datatype_t;
typedef int shm_internal_op_t;
typedef int shmem_transport_ct_t;

static inline
int
shmem_transport_init(long eager_size)
{
    return 0;
}

static inline
int
shmem_transport_startup(void)
{
    return 0;
}

/* Print transport info, including settings of environment variables and other
 * parameters.  Called when SMA_INFO is set.
 */
static inline
void
shmem_transport_print_info(void)
{
    return;
}

static inline
int
shmem_transport_fini(void)
{
    return 0;
}

static inline
int
shmem_transport_quiet(void)
{
    return 0;
}

static inline
int
shmem_transport_fence(void)
{
    return 0;
}

static inline
void
shmem_transport_put_small(void *target, const void *source, size_t len, int pe)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_put_nb(void *target, const void *source, size_t len,
                       int pe, long *completion)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_put_wait(long *completion)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_put_nbi(void *target, const void *source, size_t len,
                       int pe)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_get(void *target, const void *source, size_t len, int pe)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_get_wait(void)
{
    RAISE_ERROR_STR("No path to peer");
}


static inline
void
shmem_transport_swap(void *target, const void *source, void *dest,
                     size_t len, int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_cswap(void *target, const void *source, void *dest,
                      const void *operand, size_t len, int pe,
                      shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_mswap(void *target, const void *source, void *dest,
                      const void *mask, size_t len, int pe,
                      shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_atomic_small(void *target, const void *source, size_t len,
                             int pe, shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_triggered_atomic_small(const void *source, size_t len,
                                       int pe, shm_internal_op_t op, shm_internal_datatype_t datatype,
                                       shmem_transport_ct_t *ct, long threshold)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_atomic_nb(void *target, const void *source, size_t len,
                          int pe, shm_internal_op_t op, shm_internal_datatype_t datatype,
                          long *completion)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_fetch_atomic(void *target, const void *source, void *dest, size_t len,
                             int pe, shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_atomic_fetch(void *target, const void *source, size_t len,
                             int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_atomic_set(void *target, const void *source, size_t len,
                             int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
int shmem_transport_atomic_supported(shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    return 0;
}

static inline
void shmem_transport_ct_create(shmem_transport_ct_t **ct_ptr)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void shmem_transport_ct_free(shmem_transport_ct_t **ct_ptr)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
long shmem_transport_ct_get(shmem_transport_ct_t *ct)
{
    RAISE_ERROR_STR("No path to peer");
    return 0;
}

static inline
void shmem_transport_ct_set(shmem_transport_ct_t *ct, long value)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void shmem_transport_triggered_ct_set(shmem_transport_ct_t *ct, long value, long threshold)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void shmem_transport_ct_wait(shmem_transport_ct_t *ct, long wait_for)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_put_ct_nb(shmem_transport_ct_t *ct, void *target, const void
                          *source, size_t len, int pe, long *completion)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void shmem_transport_get_ct(shmem_transport_ct_t *ct, void
                            *target, const void *source, size_t len, int pe)
{
    RAISE_ERROR_STR("No path to peer");
}

/**
 * Query the value of the transport's received messages counter.
 */
static inline
uint64_t shmem_transport_received_cntr_get(void)
{
    RAISE_ERROR_STR("No remote peers");
    return 0;
}

/**
 * Wait for the transport's received messages counter to be greater than or
 * equal to the given value.
 *
 * @param ge_val Function returns when received messages >= ge_val
 */
static inline
void shmem_transport_received_cntr_wait(uint64_t ge_val)
{
    RAISE_ERROR_STR("No remote peers");
}

#endif /* TRANSPORT_NONE_H */
