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

#ifndef SHMEM_COLLECTIVES_H
#define SHMEM_COLLECTIVES_H

#include "shmem_synchronization.h"


enum coll_type_t {
    AUTO = 0,
    LINEAR,
    TREE,
    DISSEM,
    TRIGGER,
    RING,
    RECDBL
};
typedef enum coll_type_t coll_type_t;

extern char *coll_type_str[];

extern long *shmem_internal_barrier_all_psync;
extern int shmem_internal_tree_crossover;

extern coll_type_t shmem_internal_barrier_type;
extern coll_type_t shmem_internal_bcast_type;
extern coll_type_t shmem_internal_reduce_type;
extern coll_type_t shmem_internal_collect_type;
extern coll_type_t shmem_internal_fcollect_type;

void shmem_internal_barrier_linear(int PE_start, int logPE_stride, int PE_size, long *pSync);
void shmem_internal_barrier_tree(int PE_start, int logPE_stride, int PE_size, long *pSync);
void shmem_internal_barrier_trigger_tree(int PE_start, int logPE_stride, int PE_size, long *pSync);
void shmem_internal_barrier_dissem(int PE_start, int logPE_stride, int PE_size, long *pSync);

static inline
void
shmem_internal_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    switch (shmem_internal_barrier_type) {
    case AUTO:
        if (PE_size < shmem_internal_tree_crossover) {
            shmem_internal_barrier_linear(PE_start, logPE_stride, PE_size, pSync);
        } else {
            shmem_internal_barrier_tree(PE_start, logPE_stride, PE_size, pSync);
        }
        break;
    case LINEAR:
        shmem_internal_barrier_linear(PE_start, logPE_stride, PE_size, pSync);
        break;
    case TREE:
        shmem_internal_barrier_tree(PE_start, logPE_stride, PE_size, pSync);
        break;
    case DISSEM:
        shmem_internal_barrier_dissem(PE_start, logPE_stride, PE_size, pSync);
        break;
    case TRIGGER:
        shmem_internal_barrier_trigger_tree(PE_start, logPE_stride, PE_size, pSync);
        break;
    default:
        RAISE_ERROR_MSG("Illegal barrier type (%d)\n",
                        shmem_internal_barrier_type);
    }
}


static inline
void
shmem_internal_barrier_all(void)
{
    shmem_internal_barrier(0, 0, shmem_internal_num_pes, shmem_internal_barrier_all_psync);
}


void shmem_internal_bcast_linear(void *target, const void *source, size_t len,
                                 int PE_root, int PE_start, int logPE_stride, int PE_size,
                                 long *pSync, int complete);
void shmem_internal_bcast_tree(void *target, const void *source, size_t len,
                               int PE_root, int PE_start, int logPE_stride, int PE_size,
                               long *pSync, int complete);

static inline
void
shmem_internal_bcast(void *target, const void *source, size_t len,
                     int PE_root, int PE_start, int logPE_stride, int PE_size,
                     long *pSync, int complete)
{
    switch (shmem_internal_bcast_type) {
    case AUTO:
        if (PE_size < shmem_internal_tree_crossover) {
            shmem_internal_bcast_linear(target, source, len, PE_root, PE_start,
                                        logPE_stride, PE_size, pSync, complete);
        } else {
            shmem_internal_bcast_tree(target, source, len, PE_root, PE_start,
                                      logPE_stride, PE_size, pSync, complete);
        }
        break;
    case LINEAR:
        shmem_internal_bcast_linear(target, source, len, PE_root, PE_start,
                                    logPE_stride, PE_size, pSync, complete);
        break;
    case TREE:
        shmem_internal_bcast_tree(target, source, len, PE_root, PE_start,
                                  logPE_stride, PE_size, pSync, complete);
        break;
    default:
        RAISE_ERROR_MSG("Illegal broadcast type (%d)\n",
                        shmem_internal_bcast_type);
    }
}


void shmem_internal_op_to_all_linear(void *target, const void *source, int count, int type_size,
                                     int PE_start, int logPE_stride, int PE_size,
                                     void *pWrk, long *pSync,
                                     shm_internal_op_t op, shm_internal_datatype_t datatype);
void shmem_internal_op_to_all_tree(void *target, const void *source, int count, int type_size,
                                   int PE_start, int logPE_stride, int PE_size,
                                   void *pWrk, long *pSync,
                                   shm_internal_op_t op, shm_internal_datatype_t datatype);

void shmem_internal_op_to_all_recdbl_sw(void *target, const void *source, int count, int type_size,
                                   int PE_start, int logPE_stride, int PE_size,
                                   void *pWrk, long *pSync,
                                   shm_internal_op_t op, shm_internal_datatype_t datatype);

static inline
void
shmem_internal_op_to_all(void *target, const void *source, int count,
                         int type_size, int PE_start, int logPE_stride,
                         int PE_size, void *pWrk, long *pSync,
                         shm_internal_op_t op,
                         shm_internal_datatype_t datatype)
{
    switch (shmem_internal_reduce_type) {
        case AUTO:
            if (shmem_transport_atomic_supported(op, datatype)) {
                if (PE_size < shmem_internal_tree_crossover) {
                    shmem_internal_op_to_all_linear(target, source, count, type_size,
                                                    PE_start, logPE_stride, PE_size,
                                                    pWrk, pSync, op, datatype);
                } else {
                    shmem_internal_op_to_all_tree(target, source, count, type_size,
                                                  PE_start, logPE_stride, PE_size,
                                                  pWrk, pSync, op, datatype);
                }
            } else {
                shmem_internal_op_to_all_recdbl_sw(target, source, count, type_size,
                                                   PE_start, logPE_stride, PE_size,
                                                   pWrk, pSync, op, datatype);
            }

            break;
        case LINEAR:
            if (shmem_transport_atomic_supported(op, datatype)) {
                shmem_internal_op_to_all_linear(target, source, count, type_size,
                                                PE_start, logPE_stride, PE_size,
                                                pWrk, pSync, op, datatype);
            } else {
                shmem_internal_op_to_all_recdbl_sw(target, source, count, type_size,
                                                   PE_start, logPE_stride, PE_size,
                                                   pWrk, pSync, op, datatype);
            }
            break;
        case TREE:
            if (shmem_transport_atomic_supported(op, datatype)) {
                shmem_internal_op_to_all_tree(target, source, count, type_size,
                                              PE_start, logPE_stride, PE_size,
                                              pWrk, pSync, op, datatype);
            } else {
                shmem_internal_op_to_all_recdbl_sw(target, source, count, type_size,
                                                   PE_start, logPE_stride, PE_size,
                                                   pWrk, pSync, op, datatype);
            }
            break;
        case RECDBL:
            shmem_internal_op_to_all_recdbl_sw(target, source, count, type_size,
                                               PE_start, logPE_stride, PE_size,
                                               pWrk, pSync, op, datatype);
            break;
        default:
            RAISE_ERROR_MSG("Illegal reduction type (%d)\n",
                            shmem_internal_reduce_type);
    }
}


void shmem_internal_collect_linear(void *target, const void *source, size_t len,
                                   int PE_start, int logPE_stride, int PE_size, long *pSync);

static inline
void
shmem_internal_collect(void *target, const void *source, size_t len,
                  int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    switch (shmem_internal_collect_type) {
    case AUTO:
        shmem_internal_collect_linear(target, source, len, PE_start, logPE_stride,
                                      PE_size, pSync);
        break;
    case LINEAR:
        shmem_internal_collect_linear(target, source, len, PE_start, logPE_stride,
                                      PE_size, pSync);
        break;
    default:
        RAISE_ERROR_MSG("Illegal collect type (%d)\n",
                        shmem_internal_collect_type);
    }
}


void shmem_internal_fcollect_linear(void *target, const void *source, size_t len,
                                    int PE_start, int logPE_stride, int PE_size, long *pSync);
void shmem_internal_fcollect_ring(void *target, const void *source, size_t len,
                                  int PE_start, int logPE_stride, int PE_size, long *pSync);
void shmem_internal_fcollect_recdbl(void *target, const void *source, size_t len,
                                    int PE_start, int logPE_stride, int PE_size, long *pSync);

static inline
void
shmem_internal_fcollect(void *target, const void *source, size_t len,
                   int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    switch (shmem_internal_fcollect_type) {
    case AUTO:
        shmem_internal_fcollect_ring(target, source, len, PE_start, logPE_stride,
                                     PE_size, pSync);
        break;
    case LINEAR:
        shmem_internal_fcollect_linear(target, source, len, PE_start, logPE_stride,
                                       PE_size, pSync);
        break;
    case RING:
        shmem_internal_fcollect_ring(target, source, len, PE_start, logPE_stride,
                                     PE_size, pSync);
        break;
    case RECDBL:
        if (0 == (PE_size & (PE_size - 1))) {
            shmem_internal_fcollect_recdbl(target, source, len, PE_start, logPE_stride,
                                           PE_size, pSync);
        } else {
            shmem_internal_fcollect_ring(target, source, len, PE_start, logPE_stride,
                                         PE_size, pSync);
        }
        break;
    default:
        RAISE_ERROR_MSG("Illegal fcollect type (%d)\n",
                        shmem_internal_fcollect_type);
    }
}


void shmem_internal_alltoall(void *dest, const void *source, size_t len,
                             int PE_start, int logPE_stride, int PE_size, long *pSync);

void shmem_internal_alltoalls(void *dest, const void *source, ptrdiff_t dst,
                              ptrdiff_t sst, size_t elem_size, size_t nelems,
                              int PE_start, int logPE_stride, int PE_size, long *pSync);
#endif
