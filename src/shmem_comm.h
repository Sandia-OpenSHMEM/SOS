/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2015 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Portals SHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef PORTALS_SHMEM_DATA_H
#define PORTALS_SHMEM_DATA_H

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/param.h>

#include "shmem.h" /* shmemx_ct_t */

extern void *shmem_internal_heap_base;
extern long shmem_internal_heap_length;
extern void *shmem_internal_data_base;
extern long shmem_internal_data_length;

#ifdef USE_ON_NODE_COMMS
extern char *shmem_internal_location_array;
#define SHMEM_SET_RANK_SAME_NODE(pe, node_rank)         \
    do {                                                \
        shmem_internal_location_array[pe] = node_rank;  \
    } while (0)

#define SHMEM_GET_RANK_SAME_NODE(pe) (shmem_internal_location_array[pe])
#else
#define SHMEM_GET_RANK_SAME_NODE(pe) (-1)
#endif


#ifdef USE_PORTALS4
#include "transport_portals4.h"
#endif

#ifdef USE_XPMEM
#include "transport_xpmem.h"
#endif

#ifdef USE_CMA
#include "transport_cma.h"
#endif

#ifdef USE_OFI
#include "transport_ofi.h"
#endif

static inline
void
shmem_internal_put_small(void *target, const void *source, size_t len, int pe)
{
    int node_rank;

    if (-1 != (node_rank = SHMEM_GET_RANK_SAME_NODE(pe))) {
#if USE_XPMEM
        shmem_transport_xpmem_put(target, source, len, pe, node_rank);
#elif USE_CMA
        shmem_transport_cma_put(target, source, len, pe, node_rank);
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    } else {
#if USE_PORTALS4
        shmem_transport_portals4_put_small(target, source, len, pe);
#elif USE_OFI
        shmem_transport_ofi_put_small(target, source, len, pe);
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    }
}


static inline
void
shmem_internal_put_nb(void *target, const void *source, size_t len, int pe,
                      long *completion)
{
    int node_rank;

    if (-1 != (node_rank = SHMEM_GET_RANK_SAME_NODE(pe))) {
#if USE_XPMEM
        shmem_transport_xpmem_put(target, source, len, pe, node_rank);
#elif USE_CMA
        if (len > shmem_transport_cma_put_max) {
            shmem_transport_cma_put(target, source, len, pe, node_rank);
        } else {
#  if USE_PORTALS4
            shmem_transport_portals4_put_nb(target, source, len, pe, completion);
#  elif USE_OFI
            shmem_transport_ofi_put_nb(target, source, len, pe, completion);
#  else
        RAISE_ERROR_STR("No path to peer");
#  endif /* USE_PORTALS4 */

        }
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    } else {
#if USE_PORTALS4
        shmem_transport_portals4_put_nb(target, source, len, pe, completion);
#elif USE_OFI
        shmem_transport_ofi_put_nb(target, source, len, pe, completion);
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    }
}


static inline
void
shmem_internal_put_ct_nb(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe,
                      long *completion)
{
    /* TODO: add shortcut for on-node-comms */
#if USE_PORTALS4
    shmem_transport_portals4_put_ct_nb((shmem_transport_portals4_ct_t *)
                                       ct, target, source, len, pe,
                                       completion);
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
void
shmem_internal_put_wait(long *completion)
{
#if USE_PORTALS4
    shmem_transport_portals4_put_wait(completion);
#elif USE_OFI
    shmem_transport_ofi_put_wait(completion);
#endif
    /* on-node is always blocking, so this is a no-op for them */
}


static inline
void
shmem_internal_get(void *target, const void *source, size_t len, int pe)
{
    int node_rank;

    if (-1 != (node_rank = SHMEM_GET_RANK_SAME_NODE(pe))) {
#if USE_XPMEM
        shmem_transport_xpmem_get(target, source, len, pe, node_rank);
#elif USE_CMA
        if (len > shmem_transport_cma_get_max) {
            shmem_transport_portals4_get(target, source, len, pe);
        } else {
            shmem_transport_cma_get(target, source, len, pe, node_rank);
        }
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    } else {
#if USE_PORTALS4
        shmem_transport_portals4_get(target, source, len, pe);
#elif USE_OFI
	shmem_transport_ofi_get(target, source, len, pe);
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    }
}


static inline
void
shmem_internal_get_ct(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe)
{
    /* TODO: add shortcut for on-node-comms */
#if USE_PORTALS4
    shmem_transport_portals4_get_ct((shmem_transport_portals4_ct_t *) ct,
                                    target, source, len, pe);
#elif USE_OFI
        fprintf(stderr,"NOT IMPLEMENTED\n");
        exit(0);
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
void
shmem_internal_get_wait(void)
{
#if USE_PORTALS4
    shmem_transport_portals4_get_wait();
#elif USE_OFI
    shmem_transport_ofi_get_wait();
#endif
    /* on-node is always blocking, so this is a no-op for them */
}

#if defined( USE_PORTALS4 )
typedef ptl_datatype_t shm_internal_datatype_t;
typedef ptl_op_t shm_internal_op_t;
#define SHM_INTERNAL_FLOAT PTL_FLOAT
#define SHM_INTERNAL_DOUBLE PTL_DOUBLE
#define SHM_INTERNAL_LONG_DOUBLE PTL_LONG_DOUBLE
#define SHM_INTERNAL_FLOAT_COMPLEX PTL_FLOAT_COMPLEX
#define SHM_INTERNAL_DOUBLE_COMPLEX PTL_DOUBLE_COMPLEX

#define SHM_INTERNAL_SHORT PTL_SHORT
#define SHM_INTERNAL_SIGNED_BYTE PTL_INT8_T
#define SHM_INTERNAL_INT32 PTL_INT32_T
#define SHM_INTERNAL_INT64 PTL_INT64_T

#define SHM_INTERNAL_BAND PTL_BAND
#define SHM_INTERNAL_BOR PTL_BOR
#define SHM_INTERNAL_BXOR PTL_BXOR
#define SHM_INTERNAL_MIN PTL_MIN
#define SHM_INTERNAL_MAX PTL_MAX
#define SHM_INTERNAL_SUM PTL_SUM
#define SHM_INTERNAL_PROD PTL_PROD

#elif defined ( USE_OFI )
typedef enum fi_datatype shm_internal_datatype_t;
typedef enum fi_op       shm_internal_op_t;

// Datatypes
#define SHM_INTERNAL_FLOAT           FI_FLOAT
#define SHM_INTERNAL_DOUBLE          FI_DOUBLE
#define SHM_INTERNAL_LONG_DOUBLE     FI_LONG_DOUBLE
#define SHM_INTERNAL_FLOAT_COMPLEX   FI_FLOAT_COMPLEX
#define SHM_INTERNAL_DOUBLE_COMPLEX  FI_DOUBLE_COMPLEX
#define SHM_INTERNAL_SIGNED_BYTE     FI_INT8
#define SHM_INTERNAL_INT32           FI_INT32
#define SHM_INTERNAL_INT64           FI_INT64

 // Operations
#define SHM_INTERNAL_BAND            FI_BAND
#define SHM_INTERNAL_BOR             FI_BOR
#define SHM_INTERNAL_BXOR            FI_BXOR
#define SHM_INTERNAL_MIN             FI_MIN
#define SHM_INTERNAL_MAX             FI_MAX
#define SHM_INTERNAL_SUM             FI_SUM
#define SHM_INTERNAL_PROD            FI_PROD

#else
#error "Fatal:  No Transport defined"
#endif


static inline
void
shmem_internal_swap(void *target, void *source, void *dest, size_t len,
                    int pe, shm_internal_datatype_t datatype)
{
#if USE_PORTALS4
    shmem_transport_portals4_swap(target, source, dest, len, pe, datatype);
#elif USE_OFI
    shmem_transport_ofi_swap(target, source, dest, len, pe, datatype);
#endif
}


static inline
void
shmem_internal_cswap(void *target, void *source, void *dest, void *operand, size_t len,
                    int pe, shm_internal_datatype_t datatype)
{
#if USE_PORTALS4
    shmem_transport_portals4_cswap(target, source, dest, operand, len, pe, datatype);
#elif USE_OFI
    shmem_transport_ofi_cswap(target, source, dest, operand, len, pe, datatype);
#endif
}


static inline
void
shmem_internal_mswap(void *target, void *source, void *dest, void *mask, size_t len,
                    int pe, shm_internal_datatype_t datatype)
{
#if USE_PORTALS4
    shmem_transport_portals4_mswap(target, source, dest, mask, len, pe, datatype);
#elif USE_OFI
    shmem_transport_ofi_mswap(target, source, dest, mask, len, pe, datatype);
#endif
}


static inline
void
shmem_internal_atomic_small(void *target, void *source, size_t len,
			   int pe, shm_internal_op_t op, shm_internal_datatype_t datatype)
{
#if USE_PORTALS4
    shmem_transport_portals4_atomic_small(target, source, len, pe, op, datatype);
#elif USE_OFI
    shmem_transport_ofi_atomic_small(target, source, len, pe, op, datatype);
#endif
}


static inline
void
shmem_internal_atomic_nb(void *target, void *source, size_t len,
	              int pe, shm_internal_op_t op, shm_internal_datatype_t datatype,
                      long *completion)
{
#if USE_PORTALS4
    shmem_transport_portals4_atomic_nb(target, source, len, pe, op, datatype,
                                       completion);
#elif USE_OFI
    shmem_transport_ofi_atomic_nb(target, source, len, pe, op, datatype,
                                       completion);
#endif
}



static inline
void
shmem_internal_fetch_atomic(void *target, void *source, void *dest, size_t len,
			    int pe, shm_internal_op_t op, shm_internal_datatype_t datatype)
{
#if USE_PORTALS4
    shmem_transport_portals4_fetch_atomic(target, source, dest, len, pe, op, datatype);
#elif USE_OFI
    shmem_transport_ofi_fetch_atomic(target, source, dest, len, pe, op, datatype);
#endif
}


static inline
void shmem_internal_ct_create(shmemx_ct_t *ct)
{
#if USE_PORTALS4
    shmem_transport_portals4_ct_create((shmem_transport_portals4_ct_t **) ct);
#elif USE_OFI
        fprintf(stderr,"NOT IMPLEMENTED\n");
        exit(0);
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
void shmem_internal_ct_free(shmemx_ct_t *ct)
{
#if USE_PORTALS4
    shmem_transport_portals4_ct_free((shmem_transport_portals4_ct_t **) ct);
#elif USE_OFI
        fprintf(stderr,"NOT IMPLEMENTED\n");
        exit(0);
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
long shmem_internal_ct_get(shmemx_ct_t ct)
{
#if USE_PORTALS4
    return shmem_transport_portals4_ct_get((shmem_transport_portals4_ct_t *) ct);
#elif USE_OFI
        fprintf(stderr,"NOT IMPLEMENTED\n");
        exit(0);
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
void shmem_internal_ct_set(shmemx_ct_t ct, long value)
{
#if USE_PORTALS4
    shmem_transport_portals4_ct_set((shmem_transport_portals4_ct_t *) ct, value);
#elif USE_OFI
        fprintf(stderr,"NOT IMPLEMENTED\n");
        exit(0);
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
void shmem_internal_ct_wait(shmemx_ct_t ct, long wait_for)
{
#if USE_PORTALS4
    shmem_transport_portals4_ct_wait((shmem_transport_portals4_ct_t *) ct, wait_for);
#elif USE_OFI
        fprintf(stderr,"NOT IMPLEMENTED\n");
        exit(0);
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


#endif
