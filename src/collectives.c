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

#include "config.h"

#include "shmem_internal.h"
#include "shmem_collectives.h"
#include "shmem.h"
#include "shmem_internal_op.h"

coll_type_t shmem_internal_barrier_type = AUTO;
coll_type_t shmem_internal_bcast_type = AUTO;
coll_type_t shmem_internal_reduce_type = AUTO;
coll_type_t shmem_internal_collect_type = AUTO;
coll_type_t shmem_internal_fcollect_type = AUTO;
long *shmem_internal_barrier_all_psync;
int shmem_internal_tree_crossover = -1;

char *coll_type_str[] = { "AUTO",
                          "LINEAR",
                          "TREE",
                          "DISSEM",
                          "RING",
                          "RECDBL" };

static int *full_tree_children;
static int full_tree_num_children;
static int full_tree_parent;
static int tree_radix = -1;

#define COLL_DEBUG

int
shmem_internal_build_kary_tree(int PE_start, int stride, int PE_size, int PE_root, int *parent, 
                               int *num_children, int *children)
{
    int i;
    /* my_id is the index in a theoretical 0...N-1 array of
       participating tasks. where the 0th entry is the root */
    int my_id = (((shmem_internal_my_pe - PE_start) / stride) + PE_size - PE_root) % PE_size;

    *parent = PE_start + ((my_id - 1) / tree_radix + PE_root) * stride;

    *num_children = 0;
    for (i = 1 ; i <= tree_radix ; ++i) {
        int tmp = tree_radix * my_id + i;
        if (tmp < PE_size) {
            children[(*num_children)++] = (PE_start + ((tmp + PE_root) * stride)) % (PE_size * stride);
        }
    }

    return 0;
}


int
shmem_internal_collectives_init(int requested_crossover,
                                int requested_radix)
{
    int i, j, k;
    int tmp_radix;
    int my_root = 0;
    char *type;

    tree_radix = requested_radix;
    shmem_internal_tree_crossover = requested_crossover;

    /* initialize barrier_all psync array */
    shmem_internal_barrier_all_psync = 
        shmem_internal_shmalloc(sizeof(long) * SHMEM_BARRIER_SYNC_SIZE);
    if (NULL == shmem_internal_barrier_all_psync) return -1;

    for (i = 0; i < SHMEM_BARRIER_SYNC_SIZE; i++)
        shmem_internal_barrier_all_psync[i] = SHMEM_SYNC_VALUE;

    /* initialize the binomial tree for collective operations over
       entire tree */
    full_tree_num_children = 0;
    for (i = 1 ; i <= shmem_internal_num_pes ; i *= tree_radix) {
        tmp_radix = (shmem_internal_num_pes / i < tree_radix) ? 
            (shmem_internal_num_pes / i) + 1 : tree_radix;
        my_root = (shmem_internal_my_pe / (tmp_radix * i)) * (tmp_radix * i);
        if (my_root != shmem_internal_my_pe) break;
        for (j = 1 ; j < tmp_radix ; ++j) {
            if (shmem_internal_my_pe + i * j < shmem_internal_num_pes) {
                full_tree_num_children++;
            }
        }
    }

    full_tree_children = malloc(sizeof(int) * full_tree_num_children);
    if (NULL == full_tree_children) return -1;

    k = full_tree_num_children - 1;
    for (i = 1 ; i <= shmem_internal_num_pes ; i *= tree_radix) {
        tmp_radix = (shmem_internal_num_pes / i < tree_radix) ? 
            (shmem_internal_num_pes / i) + 1 : tree_radix;
        my_root = (shmem_internal_my_pe / (tmp_radix * i)) * (tmp_radix * i);
        if (my_root != shmem_internal_my_pe) break;
        for (j = 1 ; j < tmp_radix ; ++j) {
            if (shmem_internal_my_pe + i * j < shmem_internal_num_pes) {
                full_tree_children[k--] = shmem_internal_my_pe + i * j;
            }
        }
    }
    full_tree_parent = my_root;

    if (NULL != (type = shmem_util_getenv_str("BARRIER_ALGORITHM"))) {
        if (0 == strcmp(type, "auto")) {
            shmem_internal_barrier_type = AUTO;
        } else if (0 == strcmp(type, "linear")) {
            shmem_internal_barrier_type = LINEAR;
        } else if (0 == strcmp(type, "tree")) {
            shmem_internal_barrier_type = TREE;
        } else if (0 == strcmp(type, "dissem")) {
            shmem_internal_barrier_type = DISSEM;
        } else {
            fprintf(stderr, "[%03d] Bad barrier algorithm %s\n",
                    shmem_internal_my_pe, type);
        }
    }
    if (NULL != (type = shmem_util_getenv_str("BCAST_ALGORITHM"))) {
        if (0 == strcmp(type, "auto")) {
            shmem_internal_bcast_type = AUTO;
        } else if (0 == strcmp(type, "linear")) {
            shmem_internal_bcast_type = LINEAR;
        } else if (0 == strcmp(type, "tree")) {
            shmem_internal_bcast_type = TREE;
        } else {
            fprintf(stderr, "[%03d] Bad broadcast algorithm %s\n",
                    shmem_internal_my_pe, type);
        }
    }
    if (NULL != (type = shmem_util_getenv_str("REDUCE_ALGORITHM"))) {
        if (0 == strcmp(type, "auto")) {
            shmem_internal_reduce_type = AUTO;
        } else if (0 == strcmp(type, "linear")) {
            shmem_internal_reduce_type = LINEAR;
        } else if (0 == strcmp(type, "tree")) {
            shmem_internal_reduce_type = TREE;
        } else if (0 == strcmp(type, "recdbl")) {
            shmem_internal_reduce_type = RECDBL;
        } else {
            fprintf(stderr, "[%03d] Bad reduction algorithm %s\n",
                    shmem_internal_my_pe, type);
        }
    }
    if (NULL != (type = shmem_util_getenv_str("COLLECT_ALGORITHM"))) {
        if (0 == strcmp(type, "auto")) {
            shmem_internal_collect_type = AUTO;
        } else if (0 == strcmp(type, "linear")) {
            shmem_internal_collect_type = LINEAR;
        } else {
            fprintf(stderr, "[%03d] Bad collect algorithm %s\n",
                    shmem_internal_my_pe, type);
        }
    }
    if (NULL != (type = shmem_util_getenv_str("FCOLLECT_ALGORITHM"))) {
        if (0 == strcmp(type, "auto")) {
            shmem_internal_fcollect_type = AUTO;
        } else if (0 == strcmp(type, "linear")) {
            shmem_internal_fcollect_type = LINEAR;
        } else if (0 == strcmp(type, "tree")) {
            shmem_internal_fcollect_type = TREE;
        } else if (0 == strcmp(type, "ring")) {
            shmem_internal_fcollect_type = RING;
        } else if (0 == strcmp(type, "recdbl")) {
            shmem_internal_fcollect_type = RECDBL;
        } else {
            fprintf(stderr, "[%03d] Bad fcollect algorithm %s\n",
                    shmem_internal_my_pe, type);
        }
    }

    return 0;
}


/*****************************************
 *
 * BARRIER
 *
 *****************************************/
void
shmem_internal_barrier_linear(int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    long zero = 0, one = 1;
    int stride = 1 << logPE_stride;

    /* need 1 slot */
    shmem_internal_assert(SHMEM_BARRIER_SYNC_SIZE >= 1);

    shmem_internal_quiet();

    if (PE_start == shmem_internal_my_pe) {
        int pe, i;

        /* wait for N - 1 callins up the tree */
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, PE_size - 1);

        /* Clear pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                 shmem_internal_my_pe);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);

        /* Send acks down psync tree */
        for (pe = PE_start + stride, i = 1 ; 
             i < PE_size ;  
             i++, pe += stride) {
            shmem_internal_put_small(pSync, &one, sizeof(one), pe);
        }

    } else {
        /* send message to root */
        shmem_internal_atomic_small(pSync, &one, sizeof(one), PE_start, 
                                    SHM_INTERNAL_SUM, DTYPE_LONG);

        /* wait for ack down psync tree */
        SHMEM_WAIT(pSync, 0);

        /* Clear pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                 shmem_internal_my_pe);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);
    }

}


void
shmem_internal_barrier_tree(int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    long zero = 0, one = 1;
    int stride = 1 << logPE_stride;
    int parent, num_children, *children;

    /* need 1 slot */
    shmem_internal_assert(SHMEM_BARRIER_SYNC_SIZE >= 1);

    shmem_internal_quiet();

    if (PE_size == shmem_internal_num_pes) {
        /* we're the full tree, use the binomial tree */
        parent = full_tree_parent;
        num_children = full_tree_num_children;
        children = full_tree_children;
    } else {
        children = alloca(sizeof(int) * tree_radix);
        shmem_internal_build_kary_tree(PE_start, stride, PE_size, 0, &parent, 
                                       &num_children, children);
    }

    if (num_children != 0) {
        /* Not a pure leaf node */
        int i;

        /* wait for num_children callins up the tree */
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, num_children);

        if (parent == shmem_internal_my_pe) {
            /* The root of the tree */

            /* Clear pSync */
            shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                     shmem_internal_my_pe);
            SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);

            /* Send acks down to children */
            for (i = 0 ; i < num_children ; ++i) {
                shmem_internal_atomic_small(pSync, &one, sizeof(one), 
                                            children[i], 
                                            SHM_INTERNAL_SUM, DTYPE_LONG);
            }

        } else {
            /* Middle of the tree */

            /* send ack to parent */
            shmem_internal_atomic_small(pSync, &one, sizeof(one), 
                                        parent, SHM_INTERNAL_SUM, DTYPE_LONG);

            /* wait for ack from parent */
            SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, num_children  + 1);

            /* Clear pSync */
            shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                     shmem_internal_my_pe);
            SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);

            /* Send acks down to children */
            for (i = 0 ; i < num_children ; ++i) {
                shmem_internal_atomic_small(pSync, &one, sizeof(one),
                                            children[i], 
                                            SHM_INTERNAL_SUM, DTYPE_LONG);
            }
        }

    } else {
        /* Leaf node */

        /* send message up psync tree */
        shmem_internal_atomic_small(pSync, &one, sizeof(one), parent, 
                                    SHM_INTERNAL_SUM, DTYPE_LONG);

        /* wait for ack down psync tree */
        SHMEM_WAIT(pSync, 0);

        /* Clear pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                 shmem_internal_my_pe);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);
    }
}


void
shmem_internal_barrier_dissem(int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    int8_t one = 1, neg_one = -1;
    int stride = 1 << logPE_stride;
    int distance, to, i;
    int coll_rank = (shmem_internal_my_pe - PE_start) / stride;
    int8_t *pSync_bytes = (int8_t*) pSync;

    /* need log2(num_procs) int8_t slots.  max_num_procs is
       2^(sizeof(int)*8-1)-1, so make the math a bit easier and assume
       2^(sizeof(int) * 8), which means log2(num_procs) is always less
       than sizeof(int) * 8. */
    shmem_internal_assert(SHMEM_BARRIER_SYNC_SIZE >= (sizeof(int) * 8) / sizeof(long));

    shmem_internal_quiet();

    for (i = 0, distance = 1 ; distance < PE_size ; ++i, distance <<= 1) {
        to = ((coll_rank + distance) % PE_size);
        to = PE_start + (to * stride);

        shmem_internal_atomic_small(&pSync_bytes[i], &one, sizeof(int8_t),
                                    to,
                                    SHM_INTERNAL_SUM, SHM_INTERNAL_SIGNED_BYTE);

        SHMEM_WAIT_UNTIL(&pSync_bytes[i], SHMEM_CMP_NE, 0);
        /* There's a path where the next update from a peer can get
           here before the update below, but there's no path for two
           updates to arrive before the decrement */
        shmem_internal_assert(pSync_bytes[i] < 3);

        /* this slot is no longer used, so subtract off results now */
        shmem_internal_atomic_small(&pSync_bytes[i], &neg_one, sizeof(int8_t),
                                    shmem_internal_my_pe,
                                    SHM_INTERNAL_SUM, SHM_INTERNAL_SIGNED_BYTE);
    }
}


/*****************************************
 *
 * BROADCAST
 *
 *****************************************/
void
shmem_internal_bcast_linear(void *target, const void *source, size_t len,
                            int PE_root, int PE_start, int logPE_stride, int PE_size,
                            long *pSync, int complete)
{
    long zero = 0, one = 1;
    int stride = 1 << logPE_stride;
    int real_root = PE_start + PE_root * stride;
    long completion = 0;

    /* need 1 slot */
    shmem_internal_assert(SHMEM_BCAST_SYNC_SIZE >= 1);

    if (real_root == shmem_internal_my_pe) {
        int i, pe;

        /* send data to all peers */
        for (pe = PE_start,i=0; i < PE_size; pe += stride, i++) {
            if (pe == shmem_internal_my_pe) continue;
            shmem_internal_put_nb(target, source, len, pe, &completion);
        }
        shmem_internal_put_wait(&completion);
    
        shmem_internal_fence();
    
        /* send completion ack to all peers */
        for (pe = PE_start,i=0; i < PE_size; pe += stride, i++) {
            if (pe == shmem_internal_my_pe) continue;
            shmem_internal_put_small(pSync, &one, sizeof(long), pe);
        }

        if (1 == complete) {
            /* wait for acks from everyone */
            SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, PE_size - 1);

            /* Clear pSync */
            shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                     shmem_internal_my_pe);
            SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);
        }

    } else {
        /* wait for data arrival message */
        SHMEM_WAIT(pSync, 0);

        /* Clear pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                 shmem_internal_my_pe);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);
            
        if (1 == complete) {
            /* send ack back to root */
            shmem_internal_atomic_small(pSync, &one, sizeof(one), 
                                        real_root, 
                                        SHM_INTERNAL_SUM, DTYPE_LONG);
        }
    }
}


void
shmem_internal_bcast_tree(void *target, const void *source, size_t len,
                          int PE_root, int PE_start, int logPE_stride, int PE_size,
                          long *pSync, int complete)
{
    long zero = 0, one = 1;
    int stride = 1 << logPE_stride;
    long completion = 0;
    int parent, num_children, *children;
    const void *send_buf = source;

    /* need 1 slot */
    shmem_internal_assert(SHMEM_BCAST_SYNC_SIZE >= 1);

    if (PE_size == shmem_internal_num_pes && 0 == PE_root) {
        /* we're the full tree, use the binomial tree */
        parent = full_tree_parent;
        num_children = full_tree_num_children;
        children = full_tree_children;
    } else {
        children = alloca(sizeof(int) * tree_radix);
        shmem_internal_build_kary_tree(PE_start, stride, PE_size, PE_root, &parent, 
                                       &num_children, children);
    }

    if (0 != num_children) {
        int i;

        if (parent != shmem_internal_my_pe) {
            /* wait for data arrival message if not the root */
            SHMEM_WAIT(pSync, 0);

            /* if complete, send ack */
            if (1 == complete) {
                shmem_internal_atomic_small(pSync, &one, sizeof(one),
                                            parent,
                                            SHM_INTERNAL_SUM, DTYPE_LONG);
            }
        }

        /* send data to all leaves */
        for (i = 0 ; i < num_children ; ++i) {
            shmem_internal_put_nb(target, send_buf, len, children[i],
                                  &completion);
        }
        shmem_internal_put_wait(&completion);

        shmem_internal_fence();
    
        /* send completion ack to all peers */
        for (i = 0 ; i < num_children ; ++i) {
            shmem_internal_put_small(pSync, &one, sizeof(long), 
                                     children[i]);
        }

        if (1 == complete) {
            /* wait for acks from everyone */
            SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 
                                  num_children  + 
                                  ((parent == shmem_internal_my_pe) ?
                                   0 : 1));
        }

        /* Clear pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                 shmem_internal_my_pe);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);

    } else {
        /* wait for data arrival message */
        SHMEM_WAIT(pSync, 0);

        /* if complete, send ack */
        if (1 == complete) {
            shmem_internal_atomic_small(pSync, &one, sizeof(one),
                                        parent,
                                        SHM_INTERNAL_SUM, DTYPE_LONG);
        }
            
        /* Clear pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                 shmem_internal_my_pe);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);
    }
}


/*****************************************
 *
 * REDUCTION
 *
 *****************************************/
void
shmem_internal_op_to_all_linear(void *target, const void *source, int count, int type_size,
                                int PE_start, int logPE_stride, int PE_size,
                                void *pWrk, long *pSync, 
                                shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    int stride = 1 << logPE_stride;
    long zero = 0, one = 1;
    long completion = 0;

    /* need 2 slots, plus bcast */
    shmem_internal_assert(SHMEM_REDUCE_SYNC_SIZE >= 2 + SHMEM_BCAST_SYNC_SIZE);

    if (PE_start == shmem_internal_my_pe) {
        int pe, i;
        /* update our target buffer with our contribution.  The put
           will flush any atomic cache value that may currently
           exist. */
        shmem_internal_put_nb(target, source, count * type_size,
                              shmem_internal_my_pe, &completion);
        shmem_internal_put_wait(&completion);
        shmem_internal_quiet();

        /* let everyone know that it's safe to send to us */
        for (pe = PE_start + stride, i = 1 ; 
             i < PE_size ;  
             i++, pe += stride) {
            shmem_internal_put_small(pSync, &one, sizeof(one), pe);
        }

        /* Wait for others to acknowledge sending data */
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, PE_size - 1);

        /* reset pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), shmem_internal_my_pe);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);

    } else {
        /* wait for clear to send */
        SHMEM_WAIT(pSync, 0);

        /* reset pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), shmem_internal_my_pe);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);

        /* send data, ack, and wait for completion */
        shmem_internal_atomic_nb(target, source, count * type_size, PE_start,
                                 op, datatype, &completion);
        shmem_internal_put_wait(&completion);
        shmem_internal_fence();

        shmem_internal_atomic_small(pSync, &one, sizeof(one), PE_start, SHM_INTERNAL_SUM, DTYPE_LONG);
    }

    /* broadcast out */
    shmem_internal_bcast(target, target, count * type_size, 0, PE_start, 
                         logPE_stride, PE_size, pSync + 2, 0);
}


void
shmem_internal_op_to_all_tree(void *target, const void *source, int count, int type_size,
                              int PE_start, int logPE_stride, int PE_size,
                              void *pWrk, long *pSync, 
                              shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    int stride = 1 << logPE_stride;
    long zero = 0, one = 1;
    long completion = 0;
    int parent, num_children, *children;

    /* need 2 slots, plus bcast */
    shmem_internal_assert(SHMEM_REDUCE_SYNC_SIZE >= 2 + SHMEM_BCAST_SYNC_SIZE);

    if (PE_size == shmem_internal_num_pes) {
        /* we're the full tree, use the binomial tree */
        parent = full_tree_parent;
        num_children = full_tree_num_children;
        children = full_tree_children;
    } else {
        children = alloca(sizeof(int) * tree_radix);
        shmem_internal_build_kary_tree(PE_start, stride, PE_size, 0, &parent, 
                                       &num_children, children);
    }

    if (0 != num_children) {
        int i;

        /* update our target buffer with our contribution.  The put
           will flush any atomic cache value that may currently
           exist. */
        shmem_internal_put_nb(target, source, count * type_size,
                              shmem_internal_my_pe, &completion);
        shmem_internal_put_wait(&completion);
        shmem_internal_quiet();

        /* let everyone know that it's safe to send to us */
        for (i = 0 ; i < num_children ; ++i) {
            shmem_internal_put_small(pSync + 1, &one, sizeof(one), children[i]);
        }

        /* Wait for others to acknowledge sending data */
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, num_children);

        /* reset pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), shmem_internal_my_pe);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);
    }

    if (parent != shmem_internal_my_pe) {
        /* wait for clear to send */
        SHMEM_WAIT(pSync + 1, 0);

        /* reset pSync */
        shmem_internal_put_small(pSync + 1, &zero, sizeof(zero), shmem_internal_my_pe);
        SHMEM_WAIT_UNTIL(pSync + 1, SHMEM_CMP_EQ, 0);

        /* send data, ack, and wait for completion */
        shmem_internal_atomic_nb(target, (num_children == 0) ? source : target,
                                 count * type_size, parent,
                                 op, datatype, &completion);
        shmem_internal_put_wait(&completion);
        shmem_internal_fence();

        shmem_internal_atomic_small(pSync, &one, sizeof(one), parent, SHM_INTERNAL_SUM, DTYPE_LONG);
    }

    /* broadcast out */
    shmem_internal_bcast(target, target, count * type_size, 0, PE_start, 
                         logPE_stride, PE_size, pSync + 2, 0);
}



void
shmem_internal_op_to_all_recdbl_sw(void *target, const void *source, int count, int type_size,
                                int PE_start, int logPE_stride, int PE_size,
                                void *pWrk, long *pSync,
                                shm_internal_op_t op, shm_internal_datatype_t datatype)
{
   int stride = 1 << logPE_stride;
   int my_id = ((shmem_internal_my_pe - PE_start) / stride);
   long one = 1, neg_one = -1, buff = 0;
   int log2_proc = 1, pow2_proc = 2;
   int i = PE_size >> 1;
   int wrk_size = type_size*count;
   void * const current_target = malloc(wrk_size);
   int peer = 0;
   long completion = 0;
   long * pSync_extra_peer = pSync + SHMEM_REDUCE_SYNC_SIZE - 2;

 /***********************************
 *
 *       input checks and var init
 *
 * **************************************/

   if (PE_size == 1) {
      memcpy(target, source, type_size*count);
      free(current_target);
      return;
   }

   while (i != 1) {
      i >>= 1;
     pow2_proc <<= 1;
     log2_proc++;
   }

         /*Currently SHMEM_REDUCE_SYNC_SIZE assumes space for 2^32 PEs; this
            parameter may be changed if need-be */
   shmem_internal_assert(log2_proc <= (SHMEM_REDUCE_SYNC_SIZE - 2));

   if (current_target)
      memcpy(current_target, (void *) source, wrk_size);
   else
      RAISE_ERROR(1);

 /***********************************
 *
 *   alg: reduce N number of PE's into a power of two recursive doubling algorithm
 *   have extra_peers do the operation with one of the power of two PE's so the information
 *   is in the power of two algorithm, at the end, update extra_peers with answer found
 *   by power of two team
 *
 *   -target is used as "temp" buffer -- current_target tracks latest result
 *   give partner current_result,
 *
 * **************************************/

   /*extra peer exchange: grab information from extra_peer so its part of pairwise exchange*/
   if (my_id >= pow2_proc) {
      peer = (my_id - pow2_proc) * stride + PE_start;
      shmem_internal_put_nb(target, current_target, wrk_size, peer,
                            &completion);
      shmem_internal_put_wait(&completion);
      shmem_internal_fence();

      buff = neg_one;
      shmem_internal_put_small(pSync_extra_peer, &buff, sizeof(long),
                               peer);
      shmem_internal_fence();
      buff = neg_one;
      SHMEM_WAIT_UNTIL(pSync_extra_peer, SHMEM_CMP_EQ, buff);

   } else {
      if ((PE_size - pow2_proc) > my_id) {
         peer = (my_id + pow2_proc) * stride + PE_start;
         buff = neg_one;
         SHMEM_WAIT_UNTIL(pSync_extra_peer, SHMEM_CMP_EQ, buff);

         shmem_internal_reduce_local(op, datatype, count, target, current_target);
      }

   /* Pairwise exchange: (only for PE's that are within the power of 2 set) with every iteration,
      the information from each previous exchange is passed forward in the new interation */

      long *step_psync;

      for (i = 0; i < log2_proc; i++) {

         peer = (my_id ^ (1 << i)) * stride + PE_start;
         step_psync = &pSync[i];

         if (my_id < peer) {
            shmem_internal_put_small(step_psync, &one,
                                     sizeof(int), peer);
            SHMEM_WAIT(step_psync, 0);
            shmem_internal_put_nb(target, current_target,
                                  wrk_size, peer, &completion);
            shmem_internal_put_wait(&completion);
            shmem_internal_fence();
            shmem_internal_atomic_small(step_psync, &one,
                                        sizeof(int), peer,
            SHM_INTERNAL_SUM, DTYPE_INT);
         } else {
            SHMEM_WAIT(step_psync, 0);
            shmem_internal_put_nb(target, current_target,
                                  wrk_size, peer, &completion);
            shmem_internal_put_wait(&completion);
            shmem_internal_fence();
            shmem_internal_put_small(step_psync,
                                     &one, sizeof(int), peer);
            SHMEM_WAIT(step_psync, 1);
         }

         /* perform op */
         shmem_internal_reduce_local(op, datatype, count,
                                     target, current_target);

      }

      shmem_internal_quiet();

   /*update extra peer with the final result from the pairwise exchange */
      if ((PE_size - pow2_proc) > my_id) {
         peer = (my_id + pow2_proc) * stride + PE_start;

         shmem_internal_put_nb(target, current_target, wrk_size,
                               peer, &completion);
         shmem_internal_put_wait(&completion);
         shmem_internal_fence();

         buff = neg_one;
         shmem_internal_put_small(pSync_extra_peer, &buff,
                                  sizeof(long), peer);
         shmem_internal_fence();
      }

      memcpy(target, current_target, wrk_size);
   }

   free(current_target);

   for (i = 0; i < SHMEM_REDUCE_SYNC_SIZE; i++)
      pSync[i] = SHMEM_SYNC_VALUE;
}


/*****************************************
 *
 * COLLECT (variable size)
 *
 *****************************************/
void
shmem_internal_collect_linear(void *target, const void *source, size_t len,
                              int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    long tmp[3];
    int stride = 1 << logPE_stride;
    int pe;
    int bcast_len = 0, my_offset = 0;
    long completion = 0;

    /* need 3 slots, plus bcast */
    shmem_internal_assert(SHMEM_COLLECT_SYNC_SIZE >= 3 + SHMEM_BCAST_SYNC_SIZE);

    if (PE_size == 1) {
        if (target != source) memcpy(target, source, len);
        return;
    }

    /* send the update lengths to everyone */
    if (PE_start == shmem_internal_my_pe) {
        tmp[0] = len;
        tmp[1] = 1;
        shmem_internal_put_small(pSync, tmp, 2 * sizeof(long), PE_start + stride);

	/* wait for last guy to tell us we're done */
        SHMEM_WAIT_UNTIL(&pSync[1], SHMEM_CMP_EQ, 1);

	/* find out how long total data was */
        bcast_len = pSync[0];
    } else {
        /* wait for send data */
        SHMEM_WAIT_UNTIL(&pSync[1], SHMEM_CMP_EQ, 1);

        if (shmem_internal_my_pe == PE_start + stride * (PE_size - 1)) {
            /* last guy, send the offset to PE_start so he can know
               the bcast len */
            pe = PE_start;
        } else {
            /* Not the last guy, so send offset up the chain */
            pe = shmem_internal_my_pe + stride;
        }

        my_offset = pSync[0];
        tmp[0] = my_offset + len;
        tmp[1] = 1;
        shmem_internal_put_small(pSync, tmp, 2 * sizeof(long), pe);
    }

    /* everyone sends data to PE_start */
    if (! (PE_start == shmem_internal_my_pe && source == target)) {
        shmem_internal_put_nb((char*) target + my_offset, source, len, PE_start,
                              &completion);
        shmem_internal_fence();
        shmem_internal_put_wait(&completion);
    }

    /* Let PE_start know we're done.  This can definitely be a tree. */
    tmp[0] = 1;
    shmem_internal_atomic_small(pSync + 2, &tmp[0], sizeof(tmp[0]), PE_start,
                                SHM_INTERNAL_SUM, DTYPE_LONG);

    /* root waits for completion */
    if (PE_start == shmem_internal_my_pe) {
        SHMEM_WAIT_UNTIL(&pSync[2], SHMEM_CMP_EQ, PE_size);
    }

    /* clear pSync, split to stay within size limits of put_small */
    tmp[0] = tmp[1] = tmp[2] = 0;
    shmem_internal_put_small(pSync, tmp, 2 * sizeof(long), shmem_internal_my_pe);
    shmem_internal_put_small(&pSync[2], &tmp[2], sizeof(long), shmem_internal_my_pe);

    /* broadcast out */
    shmem_internal_bcast(target, target, bcast_len, 0, PE_start, logPE_stride, PE_size, pSync + 3, 0);

    /* make sure our pSync is clean before we leave... */
    SHMEM_WAIT_UNTIL(&pSync[1], SHMEM_CMP_EQ, 0);
}


/*****************************************
 *
 * COLLECT (same size)
 *
 *****************************************/
void
shmem_internal_fcollect_linear(void *target, const void *source, size_t len,
                               int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    long tmp = 1;
    int stride = 1 << logPE_stride;
    long completion = 0;

    /* need 1 slot, plus bcast */
    shmem_internal_assert(SHMEM_COLLECT_SYNC_SIZE >= 1 + SHMEM_BCAST_SYNC_SIZE);

    if (PE_start == shmem_internal_my_pe) {
        /* Copy data into the target */
        if (source != target) memcpy(target, source, len);

        /* send completion update */
        shmem_internal_atomic_small(pSync, &tmp, sizeof(long), PE_start, SHM_INTERNAL_SUM, DTYPE_LONG);

        /* wait for N updates */
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, PE_size);

        /* Clear pSync */
        tmp = 0;
        shmem_internal_put_small(pSync, &tmp, sizeof(tmp), PE_start);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);
    } else {
        /* Push data into the target */
        size_t offset = ((shmem_internal_my_pe - PE_start) / stride) * len;
        shmem_internal_put_nb((char*) target + offset, source, len, PE_start,
                              &completion);
        shmem_internal_put_wait(&completion);

        /* ensure ordering */
        shmem_internal_fence();

        /* send completion update */
        shmem_internal_atomic_small(pSync, &tmp, sizeof(long), PE_start, SHM_INTERNAL_SUM, DTYPE_LONG);
    }

    shmem_internal_bcast(target, target, len * PE_size, 0, PE_start, logPE_stride, 
                         PE_size, pSync + 1, 0);
}


/* Ring algorithm, in which every process sends only to its next
 * highest neighbor, each time sending the data it received in the
 * previous iteration.  This algorithm works regardless of process
 * count and is efficient at larger message sizes.
 * 
 *   (p - 1) alpha + ((p - 1)/p)n beta
 */
void
shmem_internal_fcollect_ring(void *target, const void *source, size_t len,
                             int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    int stride = 1 << logPE_stride;
    int i;
    /* my_id is the index in a theoretical 0...N-1 array of
       participating tasks */
    int my_id = ((shmem_internal_my_pe - PE_start) / stride);
    int next_proc = PE_start + ((my_id + 1) % PE_size) * stride;
    long completion = 0;
    long zero = 0, one = 1;

    /* need 1 slot */
    shmem_internal_assert(SHMEM_COLLECT_SYNC_SIZE >= 1);

    /* copy my portion to the right place */
    memcpy((char*) target + (my_id * len), source, len); 

    /* send n - 1 messages to the next highest proc.  Each message
       contains what we received the previous step (including our own
       data for step 1). */
    for (i = 1 ; i < PE_size ; ++i) {
        size_t iter_offset = ((my_id + 1 - i + PE_size) % PE_size) * len;

        /* send data to me + 1 */
        shmem_internal_put_nb((char*) target + iter_offset, (char*) target + iter_offset,
                             len, next_proc, &completion);
        shmem_internal_put_wait(&completion);
        shmem_internal_fence();
        
        /* send completion for this round to next proc.  Note that we
           only ever sent to next_proc and there's a shmem_fence
           between successive calls to the put above.  So a rolling
           counter is safe here. */
        shmem_internal_atomic_small(pSync, &one, sizeof(long), next_proc, SHM_INTERNAL_SUM, DTYPE_LONG);

        /* wait for completion for this round */
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_GE, i);
    }

    /* zero out psync */
    shmem_internal_put_small(pSync, &zero, sizeof(long), shmem_internal_my_pe);
    SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);
}


/* recursive doubling algorithm.  Pairs of doubling distance send
 * doubling amounts of data at each step.  This implementation only
 * supports power of two processes and is less efficient than the ring
 * algorithm at large messages.
 * 
 *   log(p) alpha + (p-1)/p n beta
 */
void
shmem_internal_fcollect_recdbl(void *target, const void *source, size_t len,
                               int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    int stride = 1 << logPE_stride;
    int my_id = ((shmem_internal_my_pe - PE_start) / stride);
    int i;
    long completion = 0;
    size_t curr_offset;
    int8_t *pSync_bytes = (int8_t*) pSync;
    int8_t one = 1, neg_one = -1;
    int distance;

    /* need log2(num_procs) int8_t slots.  max_num_procs is
       2^(sizeof(int)*8-1)-1, so make the math a bit easier and assume
       2^(sizeof(int) * 8), which means log2(num_procs) is always less
       than sizeof(int) * 8. */
    shmem_internal_assert(SHMEM_COLLECT_SYNC_SIZE >= (sizeof(int) * 8) / sizeof(long));
    shmem_internal_assert(0 == (PE_size & (PE_size - 1)));

    /* copy my portion to the right place */
    curr_offset = my_id * len;
    memcpy((char*) target + curr_offset, source, len); 

    for (i = 0, distance = 0x1 ; distance < PE_size ; i++, distance <<= 1) {
        int peer = my_id ^ distance;
        int real_peer = PE_start + (peer * stride);

        /* send data to peer */
        shmem_internal_put_nb((char*) target + curr_offset, (char*) target + curr_offset,
                              distance * len, real_peer, &completion);
        shmem_internal_put_wait(&completion);
        shmem_internal_fence();

        /* mark completion for this round */
        shmem_internal_atomic_small(&pSync_bytes[i], &one, sizeof(int8_t),
                                    real_peer,
                                    SHM_INTERNAL_SUM, SHM_INTERNAL_SIGNED_BYTE);

        SHMEM_WAIT_UNTIL(&pSync_bytes[i], SHMEM_CMP_NE, 0);

        /* this slot is no longer used, so subtract off results now */
        shmem_internal_atomic_small(&pSync_bytes[i], &neg_one, sizeof(int8_t),
                                    shmem_internal_my_pe,
                                    SHM_INTERNAL_SUM, SHM_INTERNAL_SIGNED_BYTE);

        if (my_id > peer) {
            curr_offset -= (distance * len);
        }
    }

    shmem_internal_quiet();
}


/* Circulator iterator for PE active sets */
static inline int
shmem_internal_circular_iter_next(int curr, int PE_start, int logPE_stride, int PE_size)
{
    const int stride = 1 << logPE_stride;
    const int last = PE_start + (stride * (PE_size - 1));
    int next;

    next = curr + stride;
    if (next > last)
        next = PE_start;

    return next;
}


void
shmem_internal_alltoall(void *dest, const void *source, size_t len,
                        int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    const long one = 1;
    const void *dest_ptr = (uint8_t *) dest + shmem_internal_my_pe * len;
    int peer;

    if (0 == len)
        return;

    /* Send data round-robin, starting with my PE */
    peer = shmem_internal_my_pe;
    do {
        shmem_internal_put_nb((void *) dest_ptr, (uint8_t *) source + peer * len,
                              len, peer, NULL);
        peer = shmem_internal_circular_iter_next(peer, PE_start, logPE_stride,
                                                 PE_size);
    } while (peer != shmem_internal_my_pe);

    shmem_internal_fence();

    /* Send flags round-robin, starting with my PE */
    peer = shmem_internal_my_pe;
    do {
        shmem_internal_atomic_small(pSync, &one, sizeof(long), peer,
                                    SHM_INTERNAL_SUM, DTYPE_LONG);
        peer = shmem_internal_circular_iter_next(peer, PE_start, logPE_stride,
                                                 PE_size);
    } while (peer != shmem_internal_my_pe);

    SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, PE_size);
    *pSync = 0;
}


void
shmem_internal_alltoalls(void *dest, const void *source, ptrdiff_t dst,
                         ptrdiff_t sst, size_t elem_size, size_t nelems,
                         int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    const long one = 1;
    const void *dest_base = (uint8_t *) dest + shmem_internal_my_pe * nelems * dst * elem_size;
    int peer;

    if (0 == nelems)
        return;

    /* Implementation note: Neither OFI nor Portals presently has support for
     * noncontiguous data at the target of a one-sided operation.  I'm not sure
     * of the best communication schedule for the resulting doubly-nested
     * all-to-all.  It may be preferable in some scenarios to exchange the
     * loops below to spread out the communication and decrease the exposure to
     * incast.
     */

    /* Send data round-robin, starting with my PE */
    peer = shmem_internal_my_pe;
    do {
        size_t i;
        uint8_t *dest_ptr   = (uint8_t *) dest_base;
        uint8_t *source_ptr = (uint8_t *) source + peer * nelems * sst * elem_size;

        for (i = nelems ; i > 0; i--) {
            shmem_internal_put_small((void *) dest_ptr, (uint8_t *) source_ptr,
                                     elem_size, peer);

            source_ptr += sst * elem_size;
            dest_ptr   += dst * elem_size;
        }
        peer = shmem_internal_circular_iter_next(peer, PE_start, logPE_stride,
                                                 PE_size);
    } while (peer != shmem_internal_my_pe);

    shmem_internal_fence();

    /* Send flags round-robin, starting with my PE */
    peer = shmem_internal_my_pe;
    do {
        shmem_internal_atomic_small(pSync, &one, sizeof(long), peer,
                                    SHM_INTERNAL_SUM, DTYPE_LONG);
        peer = shmem_internal_circular_iter_next(peer, PE_start, logPE_stride,
                                                 PE_size);
    } while (peer != shmem_internal_my_pe);

    SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, PE_size);
    *pSync = 0;
}
