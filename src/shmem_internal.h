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

#ifndef PORTALS_SHMEM_INTERNAL_H
#define PORTALS_SHMEM_INTERNAL_H

#include <portals4.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/param.h>

#define DATA_IDX 10
#define HEAP_IDX 11

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

extern void *shmem_internal_heap_base;
extern long shmem_internal_heap_length;
extern void *shmem_internal_data_base;
extern long shmem_internal_data_length;

extern int shmem_internal_my_pe;
extern int shmem_internal_num_pes;

extern int shmem_internal_initialized;
extern int shmem_internal_finalized;


#define RAISE_ERROR(ret)                                                \
    do {                                                                \
        fprintf(stderr, "[%03d] ERROR: %s:%d return code %d\n",         \
                shmem_internal_my_pe, __FILE__, __LINE__, (int) ret);   \
        abort();                                                        \
    } while (0)


#define RAISE_ERROR_STR(str)                                            \
    do {                                                                \
        fprintf(stderr, "[%03d] ERROR: %s:%d: %s\n",                    \
                shmem_internal_my_pe, __FILE__, __LINE__, str);         \
        abort();                                                        \
    } while (0)

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
int
shmem_internal_put(void *target, const void *source, size_t len, int pe)
{
    int node_rank;

    if (-1 != (node_rank = SHMEM_GET_RANK_SAME_NODE(pe))) {
#if USE_XPMEM
        shmem_transport_xpmem_put(target, source, len, pe, node_rank);
        return 0;
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    } else {
#if USE_PORTALS4
        return shmem_transport_portals4_put(target, source, len, pe);
#else
        RAISE_ERROR_STR("No path to peer");
#endif
    }
}


static inline
void
shmem_internal_put_wait(int count)
{
#if USE_PORTALS4
    shmem_transport_portals4_put_wait(count);
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
int
shmem_internal_swap(void *target, void *source, void *dest, size_t len, 
                    int pe, ptl_datatype_t datatype)
{
    return shmem_transport_portals4_swap(target, source, dest, len, pe, datatype);
}


static inline
int
shmem_internal_cswap(void *target, void *source, void *dest, void *operand, size_t len, 
                     int pe, ptl_datatype_t datatype)
{
    return shmem_transport_portals4_cswap(target, source, dest, operand, len, pe, datatype);
}


static inline
int
shmem_internal_mswap(void *target, void *source, void *dest, void *mask, size_t len, 
                     int pe, ptl_datatype_t datatype)
{
    return shmem_transport_portals4_mswap(target, source, dest, mask, len, pe, datatype);
}


static inline
int
shmem_internal_atomic(void *target, void *source, size_t len,
                      int pe, ptl_op_t op, ptl_datatype_t datatype)
{
    return shmem_transport_portals4_atomic(target, source, len, pe, op, datatype);
}


static inline
int
shmem_internal_fetch_atomic(void *target, void *source, void *dest, size_t len,
                            int pe, ptl_op_t op, ptl_datatype_t datatype)
{
    return shmem_transport_portals4_fetch_atomic(target, source, dest, len, pe, op, datatype);
}


/* initialization routines */
int shmem_internal_symmetric_init(long req_len);
int shmem_internal_symmetric_fini(void);
int shmem_internal_collectives_init(int requested_crossover, 
                                    int requested_radix);



#endif
