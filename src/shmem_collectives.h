/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2017 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef SHMEM_COLLECTIVES_H
#define SHMEM_COLLECTIVES_H

#include "shmem_synchronization.h"


enum coll_type_t {
    AUTO = 0,
    LINEAR,
    TREE,
    DISSEM,
    RING,
    RECDBL
};
typedef enum coll_type_t coll_type_t;

extern char *coll_type_str[];

extern long *shmem_internal_barrier_all_psync;
extern long *shmem_internal_sync_all_psync;

extern coll_type_t shmem_internal_barrier_type;
extern coll_type_t shmem_internal_bcast_type;
extern coll_type_t shmem_internal_reduce_type;
extern coll_type_t shmem_internal_scan_type;
extern coll_type_t shmem_internal_collect_type;
extern coll_type_t shmem_internal_fcollect_type;

void shmem_internal_sync_linear(int PE_start, int PE_stride, int PE_size, long *pSync);
void shmem_internal_sync_tree(int PE_start, int PE_stride, int PE_size, long *pSync);
void shmem_internal_sync_dissem(int PE_start, int PE_stride, int PE_size, long *pSync);

static inline
void
shmem_internal_sync(int PE_start, int PE_stride, int PE_size, long *pSync)
{
    if (shmem_internal_params.BARRIERS_FLUSH) {
        fflush(stdout);
        fflush(stderr);
    }

    if (PE_size == 1) return;

    switch (shmem_internal_barrier_type) {
    case AUTO:
        if (PE_size < shmem_internal_params.COLL_CROSSOVER) {
            shmem_internal_sync_linear(PE_start, PE_stride, PE_size, pSync);
        } else {
            shmem_internal_sync_tree(PE_start, PE_stride, PE_size, pSync);
        }
        break;
    case LINEAR:
        shmem_internal_sync_linear(PE_start, PE_stride, PE_size, pSync);
        break;
    case TREE:
        shmem_internal_sync_tree(PE_start, PE_stride, PE_size, pSync);
        break;
    case DISSEM:
        shmem_internal_sync_dissem(PE_start, PE_stride, PE_size, pSync);
        break;
    default:
        RAISE_ERROR_MSG("Illegal barrier/sync type (%d)\n",
                        shmem_internal_barrier_type);
    }

    /* Ensure remote updates are visible in memory */
    shmem_internal_membar_acq_rel();
    shmem_transport_syncmem();
}


static inline
void
shmem_internal_sync_all(void)
{
    shmem_internal_sync(0, 1, shmem_internal_num_pes, shmem_internal_sync_all_psync);
}


static inline
void
shmem_internal_barrier(int PE_start, int PE_stride, int PE_size, long *pSync)
{
    shmem_internal_quiet(SHMEM_CTX_DEFAULT);
    shmem_internal_sync(PE_start, PE_stride, PE_size, pSync);
}


static inline
void
shmem_internal_barrier_all(void)
{
    shmem_internal_quiet(SHMEM_CTX_DEFAULT);
    shmem_internal_sync(0, 1, shmem_internal_num_pes, shmem_internal_barrier_all_psync);
}


void shmem_internal_bcast_linear(void *target, const void *source, size_t len,
                                 int PE_root, int PE_start, int PE_stride, int PE_size,
                                 long *pSync, int complete);
void shmem_internal_bcast_tree(void *target, const void *source, size_t len,
                               int PE_root, int PE_start, int PE_stride, int PE_size,
                               long *pSync, int complete);

static inline
void
shmem_internal_bcast(void *target, const void *source, size_t len,
                     int PE_root, int PE_start, int PE_stride, int PE_size,
                     long *pSync, int complete)
{
    switch (shmem_internal_bcast_type) {
    case AUTO:
        if (PE_size < shmem_internal_params.COLL_CROSSOVER) {
            shmem_internal_bcast_linear(target, source, len, PE_root, PE_start,
                                        PE_stride, PE_size, pSync, complete);
        } else {
            shmem_internal_bcast_tree(target, source, len, PE_root, PE_start,
                                      PE_stride, PE_size, pSync, complete);
        }
        break;
    case LINEAR:
        shmem_internal_bcast_linear(target, source, len, PE_root, PE_start,
                                    PE_stride, PE_size, pSync, complete);
        break;
    case TREE:
        shmem_internal_bcast_tree(target, source, len, PE_root, PE_start,
                                  PE_stride, PE_size, pSync, complete);
        break;
    default:
        RAISE_ERROR_MSG("Illegal broadcast type (%d)\n",
                        shmem_internal_bcast_type);
    }
}


void shmem_internal_op_to_all_linear(void *target, const void *source, size_t count, size_t type_size,
                                     int PE_start, int PE_stride, int PE_size,
                                     void *pWrk, long *pSync,
                                     shm_internal_op_t op, shm_internal_datatype_t datatype);
void shmem_internal_op_to_all_ring(void *target, const void *source, size_t count, size_t type_size,
                                   int PE_start, int PE_stride, int PE_size,
                                   void *pWrk, long *pSync,
                                   shm_internal_op_t op, shm_internal_datatype_t datatype);
void shmem_internal_op_to_all_tree(void *target, const void *source, size_t count, size_t type_size,
                                   int PE_start, int PE_stride, int PE_size,
                                   void *pWrk, long *pSync,
                                   shm_internal_op_t op, shm_internal_datatype_t datatype);

void shmem_internal_op_to_all_recdbl_sw(void *target, const void *source, size_t count, size_t type_size,
                                   int PE_start, int PE_stride, int PE_size,
                                   void *pWrk, long *pSync,
                                   shm_internal_op_t op, shm_internal_datatype_t datatype);

static inline
void
shmem_internal_op_to_all(void *target, const void *source, size_t count,
                         size_t type_size, int PE_start, int PE_stride,
                         int PE_size, void *pWrk, long *pSync,
                         shm_internal_op_t op,
                         shm_internal_datatype_t datatype)
{
    shmem_internal_assert(type_size > 0);

    switch (shmem_internal_reduce_type) {
        case AUTO:
            if (shmem_transport_atomic_supported(op, datatype)) {
                if (PE_size < shmem_internal_params.COLL_CROSSOVER) {
                    shmem_internal_op_to_all_linear(target, source, count, type_size,
                                                    PE_start, PE_stride, PE_size,
                                                    pWrk, pSync, op, datatype);
                } else {
                    shmem_internal_op_to_all_tree(target, source, count, type_size,
                                                  PE_start, PE_stride, PE_size,
                                                  pWrk, pSync, op, datatype);
                }
            } else {
                if (count * type_size < shmem_internal_params.COLL_SIZE_CROSSOVER)
                    shmem_internal_op_to_all_recdbl_sw(target, source, count, type_size,
                                                       PE_start, PE_stride, PE_size,
                                                       pWrk, pSync, op, datatype);
                else
                    shmem_internal_op_to_all_ring(target, source, count, type_size,
                                                  PE_start, PE_stride, PE_size,
                                                  pWrk, pSync, op, datatype);
            }

            break;
        case LINEAR:
            if (shmem_transport_atomic_supported(op, datatype)) {
                shmem_internal_op_to_all_linear(target, source, count, type_size,
                                                PE_start, PE_stride, PE_size,
                                                pWrk, pSync, op, datatype);
            } else {
                shmem_internal_op_to_all_recdbl_sw(target, source, count, type_size,
                                                   PE_start, PE_stride, PE_size,
                                                   pWrk, pSync, op, datatype);
            }
            break;
        case RING:
            shmem_internal_op_to_all_ring(target, source, count, type_size,
                                          PE_start, PE_stride, PE_size,
                                          pWrk, pSync, op, datatype);
            break;
        case TREE:
            if (shmem_transport_atomic_supported(op, datatype)) {
                shmem_internal_op_to_all_tree(target, source, count, type_size,
                                              PE_start, PE_stride, PE_size,
                                              pWrk, pSync, op, datatype);
            } else {
                shmem_internal_op_to_all_recdbl_sw(target, source, count, type_size,
                                                   PE_start, PE_stride, PE_size,
                                                   pWrk, pSync, op, datatype);
            }
            break;
        case RECDBL:
            shmem_internal_op_to_all_recdbl_sw(target, source, count, type_size,
                                               PE_start, PE_stride, PE_size,
                                               pWrk, pSync, op, datatype);
            break;
        default:
            RAISE_ERROR_MSG("Illegal reduction type (%d)\n",
                            shmem_internal_reduce_type);
    }
}

void shmem_internal_scan_linear(void *target, const void *source, size_t count, size_t type_size,
                                int PE_start, int PE_stride, int PE_size, void *pWrk, long *pSync,
                                shm_internal_op_t op, shm_internal_datatype_t datatype, int scantype);
                                     
void shmem_internal_scan_ring(void *target, const void *source, size_t count, size_t type_size,
                              int PE_start, int PE_stride, int PE_size, void *pWrk, long *pSync,
                              shm_internal_op_t op, shm_internal_datatype_t datatype, int scantype);
                                     
static inline
void
shmem_internal_exscan(void *target, const void *source, size_t count,
                      size_t type_size, int PE_start, int PE_stride,
                      int PE_size, void *pWrk, long *pSync,
                      shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    shmem_internal_assert(type_size > 0);

    switch (shmem_internal_scan_type) {
        case AUTO:
            shmem_internal_scan_linear(target, source, count, type_size,
                                       PE_start, PE_stride, PE_size,
                                       pWrk, pSync, op, datatype, 1);
            break;
        case LINEAR:
            shmem_internal_scan_linear(target, source, count, type_size,
                                       PE_start, PE_stride, PE_size,
                                       pWrk, pSync, op, datatype, 1);
            break;
        case RING:
            shmem_internal_scan_ring(target, source, count, type_size,
                                     PE_start, PE_stride, PE_size,
                                     pWrk, pSync, op, datatype, 1);
            break;
        default:
            RAISE_ERROR_MSG("Illegal exscan type (%d)\n",
                            shmem_internal_scan_type);
    }
}


static inline
void
shmem_internal_inscan(void *target, const void *source, size_t count,
                      size_t type_size, int PE_start, int PE_stride,
                      int PE_size, void *pWrk, long *pSync,
                      shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    shmem_internal_assert(type_size > 0);

    switch (shmem_internal_scan_type) {
        case AUTO:
            shmem_internal_scan_linear(target, source, count, type_size,
                                       PE_start, PE_stride, PE_size,
                                       pWrk, pSync, op, datatype, 0);
            break;
        case LINEAR:
            shmem_internal_scan_linear(target, source, count, type_size,
                                       PE_start, PE_stride, PE_size,
                                       pWrk, pSync, op, datatype, 0);
            break;
        case RING:
            shmem_internal_scan_ring(target, source, count, type_size,
                                     PE_start, PE_stride, PE_size,
                                     pWrk, pSync, op, datatype, 0);
            break;
        default:
            RAISE_ERROR_MSG("Illegal exscan type (%d)\n",
                            shmem_internal_scan_type);
    }
}

void shmem_internal_collect_linear(void *target, const void *source, size_t len,
                                   int PE_start, int PE_stride, int PE_size, long *pSync);

static inline
void
shmem_internal_collect(void *target, const void *source, size_t len,
                  int PE_start, int PE_stride, int PE_size, long *pSync)
{
    switch (shmem_internal_collect_type) {
    case AUTO:
        shmem_internal_collect_linear(target, source, len, PE_start, PE_stride,
                                      PE_size, pSync);
        break;
    case LINEAR:
        shmem_internal_collect_linear(target, source, len, PE_start, PE_stride,
                                      PE_size, pSync);
        break;
    default:
        RAISE_ERROR_MSG("Illegal collect type (%d)\n",
                        shmem_internal_collect_type);
    }
}


void shmem_internal_fcollect_linear(void *target, const void *source, size_t len,
                                    int PE_start, int PE_stride, int PE_size, long *pSync);
void shmem_internal_fcollect_ring(void *target, const void *source, size_t len,
                                  int PE_start, int PE_stride, int PE_size, long *pSync);
void shmem_internal_fcollect_recdbl(void *target, const void *source, size_t len,
                                    int PE_start, int PE_stride, int PE_size, long *pSync);

static inline
void
shmem_internal_fcollect(void *target, const void *source, size_t len,
                   int PE_start, int PE_stride, int PE_size, long *pSync)
{
    switch (shmem_internal_fcollect_type) {
    case AUTO:
        shmem_internal_fcollect_ring(target, source, len, PE_start, PE_stride,
                                     PE_size, pSync);
        break;
    case LINEAR:
        shmem_internal_fcollect_linear(target, source, len, PE_start, PE_stride,
                                       PE_size, pSync);
        break;
    case RING:
        shmem_internal_fcollect_ring(target, source, len, PE_start, PE_stride,
                                     PE_size, pSync);
        break;
    case RECDBL:
        if (0 == (PE_size & (PE_size - 1))) {
            shmem_internal_fcollect_recdbl(target, source, len, PE_start, PE_stride,
                                           PE_size, pSync);
        } else {
            shmem_internal_fcollect_ring(target, source, len, PE_start, PE_stride,
                                         PE_size, pSync);
        }
        break;
    default:
        RAISE_ERROR_MSG("Illegal fcollect type (%d)\n",
                        shmem_internal_fcollect_type);
    }
}


void shmem_internal_alltoall(void *dest, const void *source, size_t len,
                             int PE_start, int PE_stride, int PE_size, long *pSync);

void shmem_internal_alltoalls(void *dest, const void *source, ptrdiff_t dst,
                              ptrdiff_t sst, size_t elem_size, size_t nelems,
                              int PE_start, int PE_stride, int PE_size, long *pSync);
#endif
