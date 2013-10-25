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

#ifndef SHMEM_COLLECTIVES_H
#define SHMEM_COLLECTIVES_H

#include "shmem_synchronization.h"


extern long *barrier_all_psync;
extern int *full_tree_children;
extern int full_tree_num_children;
extern int full_tree_parent;
extern int tree_crossover;
extern int tree_radix;

int build_kary_tree(int PE_start, int stride, int PE_size, int PE_root, int *parent, 
                    int *num_children, int *children);


void shmem_internal_barrier_linear(int PE_start, int logPE_stride, int PE_size, long *pSync);
void shmem_internal_barrier_tree(int PE_start, int logPE_stride, int PE_size, long *pSync);
void shmem_internal_barrier_dissem(int PE_start, int logPE_stride, int PE_size, long *pSync);

static inline
void
shmem_internal_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    if (PE_size < tree_crossover) {
        shmem_internal_barrier_linear(PE_start, logPE_stride, PE_size, pSync);
    } else {
        shmem_internal_barrier_tree(PE_start, logPE_stride, PE_size, pSync);
    }
}


static inline
void
shmem_internal_barrier_all(void)
{
    shmem_internal_barrier(0, 0, shmem_internal_num_pes, barrier_all_psync);
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
    if (PE_size < tree_crossover) {
        shmem_internal_bcast_linear(target, source, len, PE_root, PE_start,
                                    logPE_stride, PE_size, pSync, complete);
    } else {
        shmem_internal_bcast_tree(target, source, len, PE_root, PE_start,
                                  logPE_stride, PE_size, pSync, complete);
    }
}


void shmem_internal_op_to_all_linear(void *target, void *source, int count, int type_size,
                                     int PE_start, int logPE_stride, int PE_size,
                                     void *pWrk, long *pSync, 
                                     ptl_op_t op, ptl_datatype_t datatype);
void shmem_internal_op_to_all_tree(void *target, void *source, int count, int type_size,
                                   int PE_start, int logPE_stride, int PE_size,
                                   void *pWrk, long *pSync, 
                                   ptl_op_t op, ptl_datatype_t datatype);

static inline
void
shmem_internal_op_to_all(void *target, void *source, int count, int type_size,
                    int PE_start, int logPE_stride, int PE_size,
                    void *pWrk, long *pSync, 
                    ptl_op_t op, ptl_datatype_t datatype)
{
    if (PE_size < tree_crossover) {
        shmem_internal_op_to_all_linear(target, source, count, type_size,
                                        PE_start, logPE_stride, PE_size,
                                        pWrk, pSync, op, datatype);
    } else {
        shmem_internal_op_to_all_tree(target, source, count, type_size,
                                      PE_start, logPE_stride, PE_size,
                                      pWrk, pSync, op, datatype);
    }
}


void shmem_internal_collect_linear(void *target, const void *source, size_t len,
                                   int PE_start, int logPE_stride, int PE_size, long *pSync);

static inline
void
shmem_internal_collect(void *target, const void *source, size_t len,
                  int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    shmem_internal_collect_linear(target, source, len, PE_start, logPE_stride,
                                  PE_size, pSync);
}


void shmem_internal_fcollect_linear(void *target, const void *source, size_t len,
                                    int PE_start, int logPE_stride, int PE_size, long *pSync);

static inline
void
shmem_internal_fcollect(void *target, const void *source, size_t len,
                   int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    shmem_internal_fcollect_linear(target, source, len, PE_start, logPE_stride,
                                   PE_size, pSync);
}


#endif
