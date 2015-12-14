/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 * 
 * Copyright (c) 2015 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef TRANSPORT_H
#define TRANSPORT_H

#if defined (USE_PORTALS4)
#include "transport_portals4.h"

#elif defined (USE_OFI)
#include "transport_ofi.h"

#else /* No transport */

// Datatypes
#define SHM_INTERNAL_FLOAT           -1
#define SHM_INTERNAL_DOUBLE          -1
#define SHM_INTERNAL_LONG_DOUBLE     -1
#define SHM_INTERNAL_FLOAT_COMPLEX   -1
#define SHM_INTERNAL_DOUBLE_COMPLEX  -1
#define SHM_INTERNAL_SIGNED_BYTE     -1
#define SHM_INTERNAL_INT32           -1
#define SHM_INTERNAL_INT64           -1

 // Operations
#define SHM_INTERNAL_BAND            -1
#define SHM_INTERNAL_BOR             -1
#define SHM_INTERNAL_BXOR            -1
#define SHM_INTERNAL_MIN             -1
#define SHM_INTERNAL_MAX             -1
#define SHM_INTERNAL_SUM             -1
#define SHM_INTERNAL_PROD            -1

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
shmem_transport_swap(void *target, void *source, void *dest, size_t len, 
                     int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_cswap(void *target, void *source, void *dest, void *operand, size_t len, 
                      int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_mswap(void *target, void *source, void *dest, void *mask, size_t len, 
                      int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_atomic_small(void *target, void *source, size_t len,
                             int pe, shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_atomic_nb(void *target, void *source, size_t len,
                          int pe, shm_internal_op_t op, shm_internal_datatype_t datatype,
                          long *completion)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_fetch_atomic(void *target, void *source, void *dest, size_t len,
                             int pe, shm_internal_op_t op, shm_internal_datatype_t datatype)
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
}

static inline
void shmem_transport_ct_set(shmem_transport_ct_t *ct, long value)
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

#endif /* Transport selection */

#endif /* TRANSPORT_H */
