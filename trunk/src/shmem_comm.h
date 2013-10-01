/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
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

#include "shmem.h" /* shmem_ct_t */

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

static inline
void
shmem_internal_put_small(void *target, const void *source, size_t len, int pe)
{
    int node_rank;

    if (-1 != (node_rank = SHMEM_GET_RANK_SAME_NODE(pe))) {
#if USE_XPMEM
        shmem_transport_xpmem_put(target, source, len, pe, node_rank);
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    } else {
#if USE_PORTALS4
        shmem_transport_portals4_put_small(target, source, len, pe);
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
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    } else {
#if USE_PORTALS4
        shmem_transport_portals4_put_nb(target, source, len, pe, completion);
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    }
}


static inline
void
shmem_internal_put_ct_nb(shmem_ct_t ct, void *target, const void *source, size_t len, int pe,
                      long *completion)
{
    int node_rank;

    if (-1 != (node_rank = SHMEM_GET_RANK_SAME_NODE(pe))) {
        /* TODO: Not implemented for ON_NODE_COMMS */
        /*
#if USE_XPMEM
        shmem_transport_xpmem_put(target, source, len, pe, node_rank);
#else
        RAISE_ERROR_STR("No path to peer");
#endif
        */
        RAISE_ERROR_STR("No path to peer");
    } else {
#if USE_PORTALS4
        shmem_transport_portals4_put_ct_nb((shmem_transport_portals4_ct_t *)
                                           ct, target, source, len, pe,
                                           completion);
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    }
}


static inline
void
shmem_internal_put_wait(long *completion)
{
#if USE_PORTALS4
    shmem_transport_portals4_put_wait(completion);
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
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    } else {
#if USE_PORTALS4
        shmem_transport_portals4_get(target, source, len, pe);
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    }
}


static inline
void
shmem_internal_get_wait(void)
{
#if USE_PORTALS4
    shmem_transport_portals4_get_wait();
#endif
    /* on-node is always blocking, so this is a no-op for them */
}


static inline
void
shmem_internal_swap(void *target, void *source, void *dest, size_t len, 
                    int pe, ptl_datatype_t datatype)
{
    shmem_transport_portals4_swap(target, source, dest, len, pe, datatype);
}


static inline
void
shmem_internal_cswap(void *target, void *source, void *dest, void *operand, size_t len, 
                     int pe, ptl_datatype_t datatype)
{
    shmem_transport_portals4_cswap(target, source, dest, operand, len, pe, datatype);
}


static inline
void
shmem_internal_mswap(void *target, void *source, void *dest, void *mask, size_t len, 
                     int pe, ptl_datatype_t datatype)
{
    shmem_transport_portals4_mswap(target, source, dest, mask, len, pe, datatype);
}


static inline
void
shmem_internal_atomic_small(void *target, void *source, size_t len,
                             int pe, ptl_op_t op, ptl_datatype_t datatype)
{
    shmem_transport_portals4_atomic_small(target, source, len, pe, op, datatype);
}


static inline
void
shmem_internal_atomic_nb(void *target, void *source, size_t len,
                      int pe, ptl_op_t op, ptl_datatype_t datatype,
                      long *completion)
{
    shmem_transport_portals4_atomic_nb(target, source, len, pe, op, datatype,
                                       completion);
}



static inline
void
shmem_internal_fetch_atomic(void *target, void *source, void *dest, size_t len,
                            int pe, ptl_op_t op, ptl_datatype_t datatype)
{
    shmem_transport_portals4_fetch_atomic(target, source, dest, len, pe, op, datatype);
}


static inline
void shmem_internal_ct_create(shmem_ct_t *ct)
{
#if USE_PORTALS4
    shmem_transport_portals4_ct_create((shmem_transport_portals4_ct_t **) ct);
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
void shmem_internal_ct_free(shmem_ct_t *ct)
{
#if USE_PORTALS4
    shmem_transport_portals4_ct_free((shmem_transport_portals4_ct_t **) ct);
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
long shmem_internal_ct_get(shmem_ct_t ct)
{
#if USE_PORTALS4
    return shmem_transport_portals4_ct_get((shmem_transport_portals4_ct_t *) ct);
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
void shmem_internal_ct_set(shmem_ct_t ct, long value)
{
#if USE_PORTALS4
    shmem_transport_portals4_ct_set((shmem_transport_portals4_ct_t *) ct, value);
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
void shmem_internal_ct_wait(shmem_ct_t ct, long wait_for)
{
#if USE_PORTALS4
    shmem_transport_portals4_ct_wait((shmem_transport_portals4_ct_t *) ct, wait_for);
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


#endif
