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

#include "shmem.h"
#include "shmemx.h"

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

#include "transport.h"

#ifdef USE_XPMEM
#include "transport_xpmem.h"
#endif

#ifdef USE_CMA
#include "transport_cma.h"
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
        shmem_transport_put_small(target, source, len, pe);
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
            shmem_transport_put_nb(target, source, len, pe, completion);
        }
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    } else {
        shmem_transport_put_nb(target, source, len, pe, completion);
    }
}


static inline
void
shmem_internal_put_ct_nb(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe,
                      long *completion)
{
    /* TODO: add shortcut for on-node-comms */
    shmem_transport_put_ct_nb((shmem_transport_ct_t *)
                              ct, target, source, len, pe, completion);
}


static inline
void
shmem_internal_put_wait(long *completion)
{
    shmem_transport_put_wait(completion);
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
            shmem_transport_get(target, source, len, pe);
        } else {
            shmem_transport_cma_get(target, source, len, pe, node_rank);
        }
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    } else {
        shmem_transport_get(target, source, len, pe);
    }
}


static inline
void
shmem_internal_get_ct(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe)
{
    /* TODO: add shortcut for on-node-comms */
    shmem_transport_get_ct((shmem_transport_ct_t *) ct,
                           target, source, len, pe);
}


static inline
void
shmem_internal_get_wait(void)
{
    shmem_transport_get_wait();
    /* on-node is always blocking, so this is a no-op for them */
}

static inline
void
shmem_internal_swap(void *target, void *source, void *dest, size_t len,
                    int pe, shm_internal_datatype_t datatype)
{
    shmem_transport_swap(target, source, dest, len, pe, datatype);
}


static inline
void
shmem_internal_cswap(void *target, void *source, void *dest, void *operand, size_t len,
                    int pe, shm_internal_datatype_t datatype)
{
    shmem_transport_cswap(target, source, dest, operand, len, pe, datatype);
}


static inline
void
shmem_internal_mswap(void *target, void *source, void *dest, void *mask, size_t len,
                    int pe, shm_internal_datatype_t datatype)
{
    shmem_transport_mswap(target, source, dest, mask, len, pe, datatype);
}


static inline
void
shmem_internal_atomic_small(void *target, void *source, size_t len,
			   int pe, shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    shmem_transport_atomic_small(target, source, len, pe, op, datatype);
}


static inline
void
shmem_internal_atomic_nb(void *target, void *source, size_t len,
	              int pe, shm_internal_op_t op, shm_internal_datatype_t datatype,
                      long *completion)
{
    shmem_transport_atomic_nb(target, source, len, pe, op, datatype, completion);
}



static inline
void
shmem_internal_fetch_atomic(void *target, void *source, void *dest, size_t len,
			    int pe, shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    shmem_transport_fetch_atomic(target, source, dest, len, pe, op, datatype);
}


static inline
void shmem_internal_ct_create(shmemx_ct_t *ct)
{
    shmem_transport_ct_create((shmem_transport_ct_t **) ct);
}


static inline
void shmem_internal_ct_free(shmemx_ct_t *ct)
{
    shmem_transport_ct_free((shmem_transport_ct_t **) ct);
}


static inline
long shmem_internal_ct_get(shmemx_ct_t ct)
{
    return shmem_transport_ct_get((shmem_transport_ct_t *) ct);
}


static inline
void shmem_internal_ct_set(shmemx_ct_t ct, long value)
{
    shmem_transport_ct_set((shmem_transport_ct_t *) ct, value);
}


static inline
void shmem_internal_ct_wait(shmemx_ct_t ct, long wait_for)
{
    shmem_transport_ct_wait((shmem_transport_ct_t *) ct, wait_for);
}


#endif
