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

#include "config.h"
#include <string.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_collectives.h"
#include "shmem_internal_op.h"

coll_type_t shmem_internal_barrier_type = AUTO;
coll_type_t shmem_internal_bcast_type = AUTO;
coll_type_t shmem_internal_reduce_type = AUTO;
coll_type_t shmem_internal_collect_type = AUTO;
coll_type_t shmem_internal_fcollect_type = AUTO;
long *shmem_internal_barrier_all_psync;
long *shmem_internal_sync_all_psync;

char *coll_type_str[] = { "AUTO",
                          "LINEAR",
                          "TREE",
                          "DISSEM",
                          "RING",
                          "RECDBL" };

static int *full_tree_children;
static int full_tree_num_children;
static int full_tree_parent;
static long tree_radix = -1;


static int
shmem_internal_build_kary_tree(int radix, int PE_start, int stride,
                               int PE_size, int PE_root, int *parent,
                               int *num_children, int *children)
{
    int i;

    /* my_id is the index in a theoretical 0...N-1 array of
       participating tasks. where the 0th entry is the root */
    int my_id = (((shmem_internal_my_pe - PE_start) / stride) + PE_size - PE_root) % PE_size;

    /* We shift PE_root to index 0, resulting in a PE active set layout of (for
       example radix 2): 0 [ 1 2 ] [ 3 4 ] [ 5 6 ] ...  The first group [ 1 2 ]
       are chilren of 0, second group [ 3 4 ] are chilren of 1, and so on */
    *parent = PE_start + (((my_id - 1) / radix + PE_root) % PE_size) * stride;

    *num_children = 0;
    for (i = 1 ; i <= radix ; ++i) {
        int tmp = radix * my_id + i;
        if (tmp < PE_size) {
            const int child_idx = (PE_root + tmp) % PE_size;
            children[(*num_children)++] = PE_start + child_idx * stride;
        }
    }

    if (shmem_internal_params.DEBUG) {
        size_t len;
        char debug_str[256];
        len = snprintf(debug_str, sizeof(debug_str), "Building k-ary tree:"
                       "\n\t\tradix=%d, PE_start=%d, stride=%d, PE_size=%d, PE_root=%d\n",
                       radix, PE_start, stride, PE_size, PE_root);

        len += snprintf(debug_str+len, sizeof(debug_str) - len, "\t\tid=%d, parent=%d, children[%d] = { ",
                        my_id, *parent, *num_children);

        for (i = 0; i < *num_children && len < sizeof(debug_str); i++)
            len += snprintf(debug_str+len, sizeof(debug_str) - len, "%d ",
                            children[i]);

        if (len < sizeof(debug_str))
            len += snprintf(debug_str+len, sizeof(debug_str) - len, "}");

        DEBUG_STR(debug_str);
    }

    return 0;
}


/* Circulator iterator for PE active sets */
static inline int
shmem_internal_circular_iter_next(int curr, int PE_start, int PE_stride, int PE_size)
{
    const int last = PE_start + (PE_stride * (PE_size - 1));
    int next;

    next = curr + PE_stride;
    if (next > last)
        next = PE_start;

    return next;
}


int
shmem_internal_collectives_init(void)
{
    int i, j, k;
    int tmp_radix;
    int my_root = 0;
    char *type;

    tree_radix = shmem_internal_params.COLL_RADIX;

    /* initialize barrier_all psync array */
    shmem_internal_barrier_all_psync =
        shmem_internal_shmalloc(sizeof(long) * SHMEM_BARRIER_SYNC_SIZE);
    if (NULL == shmem_internal_barrier_all_psync) return -1;

    for (i = 0; i < SHMEM_BARRIER_SYNC_SIZE; i++)
        shmem_internal_barrier_all_psync[i] = SHMEM_SYNC_VALUE;

    /* initialize sync_all psync array */
    shmem_internal_sync_all_psync =
        shmem_internal_shmalloc(sizeof(long) * SHMEM_BARRIER_SYNC_SIZE);
    if (NULL == shmem_internal_sync_all_psync) return -1;

    for (i = 0; i < SHMEM_BARRIER_SYNC_SIZE; i++)
        shmem_internal_sync_all_psync[i] = SHMEM_SYNC_VALUE;

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

    if (shmem_internal_params.BARRIER_ALGORITHM_provided) {
        type = shmem_internal_params.BARRIER_ALGORITHM;
        if (0 == strcmp(type, "auto")) {
            shmem_internal_barrier_type = AUTO;
        } else if (0 == strcmp(type, "linear")) {
            shmem_internal_barrier_type = LINEAR;
        } else if (0 == strcmp(type, "tree")) {
            shmem_internal_barrier_type = TREE;
        } else if (0 == strcmp(type, "dissem")) {
            shmem_internal_barrier_type = DISSEM;
        } else {
            RAISE_WARN_MSG("Ignoring bad barrier algorithm '%s'\n", type);
        }
    }
    if (shmem_internal_params.BCAST_ALGORITHM_provided) {
        type = shmem_internal_params.BCAST_ALGORITHM;
        if (0 == strcmp(type, "auto")) {
            shmem_internal_bcast_type = AUTO;
        } else if (0 == strcmp(type, "linear")) {
            shmem_internal_bcast_type = LINEAR;
        } else if (0 == strcmp(type, "tree")) {
            shmem_internal_bcast_type = TREE;
        } else {
            RAISE_WARN_MSG("Ignoring bad broadcast algorithm '%s'\n", type);
        }
    }
    if (shmem_internal_params.REDUCE_ALGORITHM_provided) {
        type = shmem_internal_params.REDUCE_ALGORITHM;
        if (0 == strcmp(type, "auto")) {
            shmem_internal_reduce_type = AUTO;
        } else if (0 == strcmp(type, "linear")) {
            shmem_internal_reduce_type = LINEAR;
        } else if (0 == strcmp(type, "ring")) {
            shmem_internal_reduce_type = RING;
        } else if (0 == strcmp(type, "tree")) {
            shmem_internal_reduce_type = TREE;
        } else if (0 == strcmp(type, "recdbl")) {
            shmem_internal_reduce_type = RECDBL;
        } else {
            RAISE_WARN_MSG("Ignoring bad reduction algorithm '%s'\n", type);
        }
    }
    if (shmem_internal_params.COLLECT_ALGORITHM_provided) {
        type = shmem_internal_params.COLLECT_ALGORITHM;
        if (0 == strcmp(type, "auto")) {
            shmem_internal_collect_type = AUTO;
        } else if (0 == strcmp(type, "linear")) {
            shmem_internal_collect_type = LINEAR;
        } else {
            RAISE_WARN_MSG("Ignoring bad collect algorithm '%s'\n", type);
        }
    }
    if (shmem_internal_params.FCOLLECT_ALGORITHM_provided) {
        type = shmem_internal_params.FCOLLECT_ALGORITHM;
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
            RAISE_WARN_MSG("Ignoring bad fcollect algorithm '%s'\n", type);
        }
    }

    return 0;
}


/*****************************************
 *
 * BARRIER/SYNC Implementations
 *
 *****************************************/
void
shmem_internal_sync_linear(int PE_start, int PE_stride, int PE_size, long *pSync,
                           size_t nic_idx)
{
    long zero = 0, one = 1;

    /* need 1 slot */
    shmem_internal_assert(SHMEM_BARRIER_SYNC_SIZE >= 1);

    if (PE_start == shmem_internal_my_pe) {
        int pe, i;

        /* wait for N - 1 callins up the tree */
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, PE_size - 1);

        /* Clear pSync */
        shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &zero, sizeof(zero),
                                 shmem_internal_my_pe, nic_idx);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);

        /* Send acks down psync tree */
        for (pe = PE_start + PE_stride, i = 1 ;
             i < PE_size ;
             i++, pe += PE_stride) {
            shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(one), pe, nic_idx);
        }

    } else {
        /* send message to root */
        shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(one), PE_start,
                              SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);

        /* wait for ack down psync tree */
        SHMEM_WAIT(pSync, 0);

        /* Clear pSync */
        shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &zero, sizeof(zero),
                                 shmem_internal_my_pe, nic_idx);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);
    }

}


void
shmem_internal_sync_tree(int PE_start, int PE_stride, int PE_size, long *pSync,
                         size_t nic_idx)
{
    long zero = 0, one = 1;
    int parent, num_children, *children;

    /* need 1 slot */
    shmem_internal_assert(SHMEM_BARRIER_SYNC_SIZE >= 1);

    if (PE_size == shmem_internal_num_pes) {
        /* we're the full tree, use the binomial tree */
        parent = full_tree_parent;
        num_children = full_tree_num_children;
        children = full_tree_children;
    } else {
        children = alloca(sizeof(int) * tree_radix);
        shmem_internal_build_kary_tree(tree_radix, PE_start, PE_stride, PE_size,
                                       0, &parent, &num_children, children);
    }

    if (num_children != 0) {
        /* Not a pure leaf node */
        int i;

        /* wait for num_children callins up the tree */
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, num_children);

        if (parent == shmem_internal_my_pe) {
            /* The root of the tree */

            /* Clear pSync */
            shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &zero, sizeof(zero),
                                     shmem_internal_my_pe, nic_idx);
            SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);

            /* Send acks down to children */
            for (i = 0 ; i < num_children ; ++i) {
                shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(one),
                                      children[i], SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);
            }

        } else {
            /* Middle of the tree */

            /* send ack to parent */
            shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(one),
                                  parent, SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);

            /* wait for ack from parent */
            SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, num_children  + 1);

            /* Clear pSync */
            shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &zero, sizeof(zero),
                                     shmem_internal_my_pe, nic_idx);
            SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);

            /* Send acks down to children */
            for (i = 0 ; i < num_children ; ++i) {
                shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(one),
                                      children[i], SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);
            }
        }

    } else {
        /* Leaf node */

        /* send message up psync tree */
        shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(one), parent,
                              SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);

        /* wait for ack down psync tree */
        SHMEM_WAIT(pSync, 0);

        /* Clear pSync */
        shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &zero, sizeof(zero),
                                 shmem_internal_my_pe, nic_idx);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);
    }
}


void
shmem_internal_sync_dissem(int PE_start, int PE_stride, int PE_size, long *pSync,
                           size_t nic_idx)
{
    int one = 1, neg_one = -1;
    int distance, to, i;
    int coll_rank = (shmem_internal_my_pe - PE_start) / PE_stride;
    int *pSync_ints = (int*) pSync;

    /* need log2(num_procs) int slots.  max_num_procs is
       2^(sizeof(int)*8-1)-1, so make the math a bit easier and assume
       2^(sizeof(int) * 8), which means log2(num_procs) is always less
       than sizeof(int) * 8. */
    /* Note: pSync can be treated as a byte array rather than an int array to
     * get better cache locality.  We chose int here for portability, since SUM
     * on INT is required by the SHMEM atomics API. */
    shmem_internal_assert(SHMEM_BARRIER_SYNC_SIZE >= (sizeof(int) * 8) / (sizeof(long) / sizeof(int)));

    for (i = 0, distance = 1 ; distance < PE_size ; ++i, distance <<= 1) {
        to = ((coll_rank + distance) % PE_size);
        to = PE_start + (to * PE_stride);

        shmem_internal_atomic(SHMEM_CTX_DEFAULT, &pSync_ints[i], &one, sizeof(int),
                              to, SHM_INTERNAL_SUM, SHM_INTERNAL_INT, nic_idx);

        SHMEM_WAIT_UNTIL(&pSync_ints[i], SHMEM_CMP_NE, 0);
        /* There's a path where the next update from a peer can get
           here before the update below, but there's no path for two
           updates to arrive before the decrement */
        shmem_internal_assert(pSync_ints[i] < 3);

        /* this slot is no longer used, so subtract off results now */
        shmem_internal_atomic(SHMEM_CTX_DEFAULT, &pSync_ints[i], &neg_one, sizeof(int),
                              shmem_internal_my_pe, SHM_INTERNAL_SUM, SHM_INTERNAL_INT, nic_idx);
    }

    /* Ensure local pSync decrements are done before a subsequent barrier */
    shmem_internal_quiet(SHMEM_CTX_DEFAULT);
}


/*****************************************
 *
 * BROADCAST
 *
 *****************************************/
void
shmem_internal_bcast_linear(void *target, const void *source, size_t len,
                            int PE_root, int PE_start, int PE_stride, int PE_size,
                            long *pSync, int complete, size_t nic_idx)
{
    long zero = 0, one = 1;
    int real_root = PE_start + PE_root * PE_stride;
    long completion = 0;

    /* need 1 slot */
    shmem_internal_assert(SHMEM_BCAST_SYNC_SIZE >= 1);

    if (PE_size == 1 || len == 0) return;

    if (real_root == shmem_internal_my_pe) {
        int i, pe;

        /* send data to all peers */
        for (pe = PE_start,i=0; i < PE_size; pe += PE_stride, i++) {
            if (pe == shmem_internal_my_pe) continue;
            shmem_internal_put_nb(SHMEM_CTX_DEFAULT, target, source, len, pe, &completion, nic_idx);
        }
        shmem_internal_put_wait(SHMEM_CTX_DEFAULT, &completion, nic_idx);

        shmem_internal_fence(SHMEM_CTX_DEFAULT);

        /* send completion ack to all peers */
        for (pe = PE_start,i=0; i < PE_size; pe += PE_stride, i++) {
            if (pe == shmem_internal_my_pe) continue;
            shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(long), pe, nic_idx);
        }

        if (1 == complete) {
            /* wait for acks from everyone */
            SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, PE_size - 1);

            /* Clear pSync */
            shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &zero, sizeof(zero),
                                     shmem_internal_my_pe, nic_idx);
            SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);
        }

    } else {
        /* wait for data arrival message */
        SHMEM_WAIT(pSync, 0);

        /* Clear pSync */
        shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &zero, sizeof(zero),
                                 shmem_internal_my_pe, nic_idx);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);

        if (1 == complete) {
            /* send ack back to root */
            shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(one),
                                  real_root, SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);
        }
    }
}


void
shmem_internal_bcast_tree(void *target, const void *source, size_t len,
                          int PE_root, int PE_start, int PE_stride, int PE_size,
                          long *pSync, int complete, size_t nic_idx)
{
    long zero = 0, one = 1;
    long completion = 0;
    int parent, num_children, *children;
    const void *send_buf = source;

    /* need 1 slot */
    shmem_internal_assert(SHMEM_BCAST_SYNC_SIZE >= 1);

    if (PE_size == 1 || len == 0) return;

    if (PE_size == shmem_internal_num_pes && 0 == PE_root) {
        /* we're the full tree, use the binomial tree */
        parent = full_tree_parent;
        num_children = full_tree_num_children;
        children = full_tree_children;
    } else {
        children = alloca(sizeof(int) * tree_radix);
        shmem_internal_build_kary_tree(tree_radix, PE_start, PE_stride, PE_size,
                                       PE_root, &parent, &num_children, children);
    }

    if (0 != num_children) {
        int i;

        if (parent != shmem_internal_my_pe) {
            send_buf = target;

            /* wait for data arrival message if not the root */
            SHMEM_WAIT(pSync, 0);

            /* if complete, send ack */
            if (1 == complete) {
                shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(one),
                                      parent, SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);
            }
        }

        /* send data to all leaves */
        for (i = 0 ; i < num_children ; ++i) {
            shmem_internal_put_nb(SHMEM_CTX_DEFAULT, target, send_buf, len, children[i],
                                  &completion, nic_idx);
        }
        shmem_internal_put_wait(SHMEM_CTX_DEFAULT, &completion, nic_idx);

        shmem_internal_fence(SHMEM_CTX_DEFAULT);

        /* send completion ack to all peers */
        for (i = 0 ; i < num_children ; ++i) {
            shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(long),
                                     children[i], nic_idx);
        }

        if (1 == complete) {
            /* wait for acks from everyone */
            SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ,
                                  num_children  +
                                  ((parent == shmem_internal_my_pe) ?
                                   0 : 1));
        }

        /* Clear pSync */
        shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &zero, sizeof(zero),
                                 shmem_internal_my_pe, nic_idx);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);

    } else {
        /* wait for data arrival message */
        SHMEM_WAIT(pSync, 0);

        /* if complete, send ack */
        if (1 == complete) {
            shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(one),
                                  parent, SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);
        }

        /* Clear pSync */
        shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &zero, sizeof(zero),
                                 shmem_internal_my_pe, nic_idx);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);
    }
}


/*****************************************
 *
 * REDUCTION
 *
 *****************************************/
void
shmem_internal_op_to_all_linear(void *target, const void *source, size_t count, size_t type_size,
                                int PE_start, int PE_stride, int PE_size,
                                void *pWrk, long *pSync,
                                shm_internal_op_t op, shm_internal_datatype_t datatype,
                                size_t nic_idx)
{

    long zero = 0, one = 1;
    long completion = 0;

    /* need 2 slots, plus bcast */
    shmem_internal_assert(SHMEM_REDUCE_SYNC_SIZE >= 2 + SHMEM_BCAST_SYNC_SIZE);

    if (count == 0) return;

    if (PE_start == shmem_internal_my_pe) {
        int pe, i;
        /* update our target buffer with our contribution.  The put
           will flush any atomic cache value that may currently
           exist. */
        shmem_internal_put_nb(SHMEM_CTX_DEFAULT, target, source, count * type_size,
                              shmem_internal_my_pe, &completion, nic_idx);
        shmem_internal_put_wait(SHMEM_CTX_DEFAULT, &completion, nic_idx);
        shmem_internal_quiet(SHMEM_CTX_DEFAULT);

        /* let everyone know that it's safe to send to us */
        for (pe = PE_start + PE_stride, i = 1 ;
             i < PE_size ;
             i++, pe += PE_stride) {
            shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(one), pe, nic_idx);
        }

        /* Wait for others to acknowledge sending data */
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, PE_size - 1);

        /* reset pSync */
        shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &zero, sizeof(zero), shmem_internal_my_pe, nic_idx);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);

    } else {
        /* wait for clear to send */
        SHMEM_WAIT(pSync, 0);

        /* reset pSync */
        shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &zero, sizeof(zero), shmem_internal_my_pe, nic_idx);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);

        /* send data, ack, and wait for completion */
        shmem_internal_atomicv(SHMEM_CTX_DEFAULT, target, source, count * type_size,
                               PE_start, op, datatype, &completion, nic_idx);
        shmem_internal_put_wait(SHMEM_CTX_DEFAULT, &completion, nic_idx);
        shmem_internal_fence(SHMEM_CTX_DEFAULT);

        shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(one),
                              PE_start, SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);
    }

    /* broadcast out */
    shmem_internal_bcast(target, target, count * type_size, 0,
                         PE_start, PE_stride, PE_size, pSync + 2, 0, nic_idx);
}


#define chunk_count(id_, count_, npes_) \
    (count_)/(npes_) + ((id_) < (count_) % (_npes))

void
shmem_internal_op_to_all_ring(void *target, const void *source, size_t count, size_t type_size,
                              int PE_start, int PE_stride, int PE_size,
                              void *pWrk, long *pSync,
                              shm_internal_op_t op, shm_internal_datatype_t datatype,
                              size_t nic_idx)
{
    int group_rank = (shmem_internal_my_pe - PE_start) / PE_stride;
    long zero = 0, one = 1;

    int peer = PE_start + ((group_rank + 1) % PE_size) * PE_stride;
    int free_source = 0;

    /* One slot for reduce-scatter and another for the allgather */
    shmem_internal_assert(SHMEM_REDUCE_SYNC_SIZE >= 2 + SHMEM_BARRIER_SYNC_SIZE);

    if (count == 0) return;

    if (PE_size == 1) {
        if (target != source)
            shmem_internal_copy_self(target, source, count * type_size, nic_idx);
        return;
    }

    /* In-place reduction: copy source data to a temporary buffer so we can use
     * the symmetric buffer to accumulate reduced data. */
    if (target == source) {
        void *tmp = malloc(count * type_size);

        if (NULL == tmp)
            RAISE_ERROR_MSG("Unable to allocate %zub temporary buffer\n", count*type_size);

        shmem_internal_copy_self(tmp, target, count * type_size, nic_idx);
        free_source = 1;
        source = tmp;

        shmem_internal_sync(PE_start, PE_stride, PE_size, pSync + 2, nic_idx);
    }

    /* Perform reduce-scatter:
     *
     * The source buffer is divided into PE_size chunks.  PEs send data to the
     * right around the ring, starting with the chunk index equal to the PE id
     * and decreasing.  For example, with 4 PEs, PE 0 sends chunks 0, 3, 2 and
     * PE 1 sends chunks 1, 0, 3.  At the end, each PE has the reduced chunk
     * corresponding to its PE id + 1.
     */
    for (int i = 0; i < PE_size - 1; i++) {
        size_t chunk_in  = (group_rank - i - 1 + PE_size) % PE_size;
        size_t chunk_out = (group_rank - i + PE_size) % PE_size;

        /* Evenly distribute extra elements across first count % PE_size chunks */
        size_t chunk_in_extra  = chunk_in  < count % PE_size;
        size_t chunk_out_extra = chunk_out < count % PE_size;
        size_t chunk_in_count  = count/PE_size + chunk_in_extra;
        size_t chunk_out_count = count/PE_size + chunk_out_extra;

        /* Account for extra elements in the displacement */
        size_t chunk_out_disp  = chunk_out_extra ?
                                 chunk_out * chunk_out_count * type_size :
                                 (chunk_out * chunk_out_count + count % PE_size) * type_size;
        size_t chunk_in_disp   = chunk_in_extra ?
                                 chunk_in * chunk_in_count * type_size :
                                 (chunk_in * chunk_in_count + count % PE_size) * type_size;

        shmem_internal_put_nbi(SHMEM_CTX_DEFAULT,
                               ((uint8_t *) target) + chunk_out_disp,
                               i == 0 ?
                                   ((uint8_t *) source) + chunk_out_disp :
                                   ((uint8_t *) target) + chunk_out_disp,
                               chunk_out_count * type_size, peer, nic_idx);
        shmem_internal_fence(SHMEM_CTX_DEFAULT);
        shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(one),
                              peer, SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);

        /* Wait for chunk */
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_GE, i+1);

        shmem_internal_reduce_local(op, datatype, chunk_in_count,
                                    ((uint8_t *) source) + chunk_in_disp,
                                    ((uint8_t *) target) + chunk_in_disp);
    }

    /* Reset reduce-scatter pSync */
    shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &zero, sizeof(zero), shmem_internal_my_pe, nic_idx);
    SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);

    /* Perform all-gather:
     *
     * Initially, each PE has the reduced chunk for PE id + 1.  Forward chunks
     * around the ring until all PEs have all chunks.
     */
    for (int i = 0; i < PE_size - 1; i++) {
        size_t chunk_out = (group_rank + 1 - i + PE_size) % PE_size;
        size_t chunk_out_extra = chunk_out < count % PE_size;
        size_t chunk_out_count = count/PE_size + chunk_out_extra;
        size_t chunk_out_disp  = chunk_out_extra ?
                                 chunk_out * chunk_out_count * type_size :
                                 (chunk_out * chunk_out_count + count % PE_size) * type_size;

        shmem_internal_put_nbi(SHMEM_CTX_DEFAULT,
                               ((uint8_t *) target) + chunk_out_disp,
                               ((uint8_t *) target) + chunk_out_disp,
                               chunk_out_count * type_size, peer, nic_idx);
        shmem_internal_fence(SHMEM_CTX_DEFAULT);
        shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync+1, &one, sizeof(one),
                              peer, SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);

        /* Wait for chunk */
        SHMEM_WAIT_UNTIL(pSync+1, SHMEM_CMP_GE, i+1);
    }

    /* reset pSync */
    shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync+1, &zero, sizeof(zero), shmem_internal_my_pe, nic_idx);
    SHMEM_WAIT_UNTIL(pSync+1, SHMEM_CMP_EQ, 0);

    if (free_source)
        free((void *)source);
}


void
shmem_internal_op_to_all_tree(void *target, const void *source, size_t count, size_t type_size,
                              int PE_start, int PE_stride, int PE_size,
                              void *pWrk, long *pSync,
                              shm_internal_op_t op, shm_internal_datatype_t datatype,
                              size_t nic_idx)
{
    long zero = 0, one = 1;
    long completion = 0;
    int parent, num_children, *children;

    /* need 2 slots, plus bcast */
    shmem_internal_assert(SHMEM_REDUCE_SYNC_SIZE >= 2 + SHMEM_BCAST_SYNC_SIZE);

    if (PE_size == 1) {
        if (target != source) {
            shmem_internal_copy_self(target, source, type_size * count, nic_idx);
        }
        return;
    }

    if (count == 0) return;

    if (PE_size == shmem_internal_num_pes) {
        /* we're the full tree, use the binomial tree */
        parent = full_tree_parent;
        num_children = full_tree_num_children;
        children = full_tree_children;
    } else {
        children = alloca(sizeof(int) * tree_radix);
        shmem_internal_build_kary_tree(tree_radix, PE_start, PE_stride, PE_size,
                                       0, &parent, &num_children, children);
    }

    if (0 != num_children) {
        int i;

        /* update our target buffer with our contribution.  The put
           will flush any atomic cache value that may currently
           exist. */
        shmem_internal_put_nb(SHMEM_CTX_DEFAULT, target, source, count * type_size,
                              shmem_internal_my_pe, &completion, nic_idx);
        shmem_internal_put_wait(SHMEM_CTX_DEFAULT, &completion, nic_idx);
        shmem_internal_quiet(SHMEM_CTX_DEFAULT);

        /* let everyone know that it's safe to send to us */
        for (i = 0 ; i < num_children ; ++i) {
            shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync + 1, &one, sizeof(one), children[i], nic_idx);
        }

        /* Wait for others to acknowledge sending data */
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, num_children);

        /* reset pSync */
        shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &zero, sizeof(zero), shmem_internal_my_pe, nic_idx);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);
    }

    if (parent != shmem_internal_my_pe) {
        /* wait for clear to send */
        SHMEM_WAIT(pSync + 1, 0);

        /* reset pSync */
        shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync + 1, &zero, sizeof(zero), shmem_internal_my_pe, nic_idx);
        SHMEM_WAIT_UNTIL(pSync + 1, SHMEM_CMP_EQ, 0);

        /* send data, ack, and wait for completion */
        shmem_internal_atomicv(SHMEM_CTX_DEFAULT, target,
                               (num_children == 0) ? source : target,
                               count * type_size, parent,
                               op, datatype, &completion, nic_idx);
        shmem_internal_put_wait(SHMEM_CTX_DEFAULT, &completion, nic_idx);
        shmem_internal_fence(SHMEM_CTX_DEFAULT);

        shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(one),
                              parent, SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);
    }

    /* broadcast out */
    shmem_internal_bcast(target, target, count * type_size, 0, PE_start,
                         PE_stride, PE_size, pSync + 2, 0, nic_idx);
}


void
shmem_internal_op_to_all_recdbl_sw(void *target, const void *source, size_t count, size_t type_size,
                                   int PE_start, int PE_stride, int PE_size,
                                   void *pWrk, long *pSync,
                                   shm_internal_op_t op, shm_internal_datatype_t datatype,
                                   size_t nic_idx)
{
    int my_id = ((shmem_internal_my_pe - PE_start) / PE_stride);
    int log2_proc = 1, pow2_proc = 2;
    int i = PE_size >> 1;
    size_t wrk_size = type_size*count;
    void * const current_target = malloc(wrk_size);
    long completion = 0;
    long * pSync_extra_peer = pSync + SHMEM_REDUCE_SYNC_SIZE - 2;
    const long ps_target_ready = 1, ps_data_ready = 2;

    if (PE_size == 1) {
        if (target != source) {
            shmem_internal_copy_self(target, source, type_size * count, nic_idx);
        }
        free(current_target);
        return;
    }

    if (count == 0) {
        free(current_target);
        return;
    }

    while (i != 1) {
        i >>= 1;
        pow2_proc <<= 1;
        log2_proc++;
    }

    /* Currently SHMEM_REDUCE_SYNC_SIZE assumes space for 2^32 PEs; this
       parameter may be changed if need-be */
    shmem_internal_assert(log2_proc <= (SHMEM_REDUCE_SYNC_SIZE - 2));

    if (current_target)
        memcpy(current_target, (void *) source, wrk_size);
    else
        RAISE_ERROR_MSG("Failed to allocate current_target (count=%zu, type_size=%zu, size=%zuB)\n",
                        count, type_size, wrk_size);

    /* Algorithm: reduce N number of PE's into a power of two recursive
     * doubling algorithm have extra_peers do the operation with one of the
     * power of two PE's so the information is in the power of two algorithm,
     * at the end, update extra_peers with answer found by power of two team
     *
     * -target is used as "temp" buffer -- current_target tracks latest result
     * give partner current_result,
     */

    /* extra peer exchange: grab information from extra_peer so its part of
     * pairwise exchange */
    if (my_id >= pow2_proc) {
        int peer = (my_id - pow2_proc) * PE_stride + PE_start;

        /* Wait for target ready, required when source and target overlap */
        SHMEM_WAIT_UNTIL(pSync_extra_peer, SHMEM_CMP_EQ, ps_target_ready);

        shmem_internal_put_nb(SHMEM_CTX_DEFAULT, target, current_target, wrk_size, peer,
                              &completion, nic_idx);
        shmem_internal_put_wait(SHMEM_CTX_DEFAULT, &completion, nic_idx);
        shmem_internal_fence(SHMEM_CTX_DEFAULT);

        shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync_extra_peer, &ps_data_ready, sizeof(long), peer, nic_idx);
        SHMEM_WAIT_UNTIL(pSync_extra_peer, SHMEM_CMP_EQ, ps_data_ready);

    } else {
        if (my_id < PE_size - pow2_proc) {
            int peer = (my_id + pow2_proc) * PE_stride + PE_start;
            shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync_extra_peer, &ps_target_ready, sizeof(long), peer, nic_idx);

            SHMEM_WAIT_UNTIL(pSync_extra_peer, SHMEM_CMP_EQ, ps_data_ready);
            shmem_internal_reduce_local(op, datatype, count, target, current_target);
        }

        /* Pairwise exchange: (only for PE's that are within the power of 2
         * set) with every iteration, the information from each previous
         * exchange is passed forward in the new interation */

        for (i = 0; i < log2_proc; i++) {
            long *step_psync = &pSync[i];
            int peer = (my_id ^ (1 << i)) * PE_stride + PE_start;

            if (shmem_internal_my_pe < peer) {
                shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, step_psync, &ps_target_ready,
                                         sizeof(long), peer, nic_idx);
                SHMEM_WAIT_UNTIL(step_psync, SHMEM_CMP_EQ, ps_data_ready);

                shmem_internal_put_nb(SHMEM_CTX_DEFAULT, target, current_target,
                                      wrk_size, peer, &completion, nic_idx);
                shmem_internal_put_wait(SHMEM_CTX_DEFAULT, &completion, nic_idx);
                shmem_internal_fence(SHMEM_CTX_DEFAULT);
                shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, step_psync, &ps_data_ready,
                                         sizeof(long), peer, nic_idx);
            }
            else {
                SHMEM_WAIT_UNTIL(step_psync, SHMEM_CMP_EQ, ps_target_ready);

                shmem_internal_put_nb(SHMEM_CTX_DEFAULT, target, current_target,
                                      wrk_size, peer, &completion, nic_idx);
                shmem_internal_put_wait(SHMEM_CTX_DEFAULT, &completion, nic_idx);
                shmem_internal_fence(SHMEM_CTX_DEFAULT);
                shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, step_psync, &ps_data_ready,
                                         sizeof(long), peer, nic_idx);

                SHMEM_WAIT_UNTIL(step_psync, SHMEM_CMP_EQ, ps_data_ready);
            }

            shmem_internal_reduce_local(op, datatype, count,
                                        target, current_target);
        }

        /* update extra peer with the final result from the pairwise exchange */
        if (my_id < PE_size - pow2_proc) {
            int peer = (my_id + pow2_proc) * PE_stride + PE_start;

            shmem_internal_put_nb(SHMEM_CTX_DEFAULT, target, current_target, wrk_size,
                                  peer, &completion, nic_idx);
            shmem_internal_put_wait(SHMEM_CTX_DEFAULT, &completion, nic_idx);
            shmem_internal_fence(SHMEM_CTX_DEFAULT);
            shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync_extra_peer, &ps_data_ready,
                                     sizeof(long), peer, nic_idx);
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
                              int PE_start, int PE_stride, int PE_size, long *pSync,
                              size_t nic_idx)
{
    size_t my_offset;
    long tmp[2];
    int peer, start_pe, i;

    /* Need 2 for lengths and barrier for completion */
    shmem_internal_assert(SHMEM_COLLECT_SYNC_SIZE >= 2 + SHMEM_BARRIER_SYNC_SIZE);

    DEBUG_MSG("target=%p, source=%p, len=%zd, PE_Start=%d, PE_stride=%d, PE_size=%d, pSync=%p\n",
              target, source, len, PE_start, PE_stride, PE_size, (void*) pSync);

    if (PE_size == 1) {
        if (target != source) shmem_internal_copy_self(target, source, len, nic_idx);
        return;
    }

    /* Linear prefix sum -- propagate update lengths and calculate offset */
    if (PE_start == shmem_internal_my_pe) {
        my_offset = 0;
        tmp[0] = (long) len; /* FIXME: Potential truncation of size_t into long */
        tmp[1] = 1; /* FIXME: Packing flag with data relies on byte ordering */
        shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, tmp, 2 * sizeof(long), PE_start + PE_stride, nic_idx);
    }
    else {
        /* wait for send data */
        SHMEM_WAIT_UNTIL(&pSync[1], SHMEM_CMP_EQ, 1);
        my_offset = pSync[0];

        /* Not the last guy, so send offset to next PE */
        if (shmem_internal_my_pe < PE_start + PE_stride * (PE_size - 1)) {
            tmp[0] = (long) (my_offset + len);
            tmp[1] = 1;
            shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, tmp, 2 * sizeof(long),
                                     shmem_internal_my_pe + PE_stride, nic_idx);
        }
    }

    /* Send data round-robin, ending with my PE */
    start_pe = shmem_internal_circular_iter_next(shmem_internal_my_pe,
                                                 PE_start, PE_stride,
                                                 PE_size);
    peer = start_pe;
    do {
        if (len > 0) {
            shmem_internal_put_nbi(SHMEM_CTX_DEFAULT, ((uint8_t *) target) + my_offset, source,
                                  len, peer, nic_idx);
        }
        peer = shmem_internal_circular_iter_next(peer, PE_start, PE_stride,
                                                 PE_size);
    } while (peer != start_pe);

    shmem_internal_barrier(PE_start, PE_stride, PE_size, &pSync[2], nic_idx);

    pSync[0] = SHMEM_SYNC_VALUE;
    pSync[1] = SHMEM_SYNC_VALUE;

    for (i = 0; i < SHMEM_BARRIER_SYNC_SIZE; i++)
        pSync[2+i] = SHMEM_SYNC_VALUE;
}


/*****************************************
 *
 * COLLECT (same size)
 *
 *****************************************/
void
shmem_internal_fcollect_linear(void *target, const void *source, size_t len,
                               int PE_start, int PE_stride, int PE_size, long *pSync,
                               size_t nic_idx)
{
    long tmp = 1;
    long completion = 0;

    /* need 1 slot, plus bcast */
    shmem_internal_assert(SHMEM_COLLECT_SYNC_SIZE >= 1 + SHMEM_BCAST_SYNC_SIZE);

    if (PE_start == shmem_internal_my_pe) {
        /* Copy data into the target */
        if (source != target) shmem_internal_copy_self(target, source, len, nic_idx);

        /* send completion update */
        shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync, &tmp, sizeof(long),
                              PE_start, SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);

        /* wait for N updates */
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, PE_size);

        /* Clear pSync */
        tmp = 0;
        shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &tmp, sizeof(tmp), PE_start, nic_idx);
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_EQ, 0);
    } else {
        /* Push data into the target */
        size_t offset = ((shmem_internal_my_pe - PE_start) / PE_stride) * len;
        shmem_internal_put_nb(SHMEM_CTX_DEFAULT, (char*) target + offset, source, len, PE_start,
                              &completion, nic_idx);
        shmem_internal_put_wait(SHMEM_CTX_DEFAULT, &completion, nic_idx);

        /* ensure ordering */
        shmem_internal_fence(SHMEM_CTX_DEFAULT);

        /* send completion update */
        shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync, &tmp, sizeof(long),
                              PE_start, SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);
    }

    shmem_internal_bcast(target, target, len * PE_size, 0, PE_start, PE_stride,
                         PE_size, pSync + 1, 0, nic_idx);
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
                             int PE_start, int PE_stride, int PE_size, long *pSync,
                             size_t nic_idx)
{
    int i;
    /* my_id is the index in a theoretical 0...N-1 array of
       participating tasks */
    int my_id = ((shmem_internal_my_pe - PE_start) / PE_stride);
    int next_proc = PE_start + ((my_id + 1) % PE_size) * PE_stride;
    long completion = 0;
    long zero = 0, one = 1;

    /* need 1 slot */
    shmem_internal_assert(SHMEM_COLLECT_SYNC_SIZE >= 1);

    if (len == 0) return;

    /* copy my portion to the right place */
    shmem_internal_copy_self((char*) target + (my_id * len), source, len, nic_idx);

    /* send n - 1 messages to the next highest proc.  Each message
       contains what we received the previous step (including our own
       data for step 1). */
    for (i = 1 ; i < PE_size ; ++i) {
        size_t iter_offset = ((my_id + 1 - i + PE_size) % PE_size) * len;

        /* send data to me + 1 */
        shmem_internal_put_nb(SHMEM_CTX_DEFAULT, (char*) target + iter_offset, (char*) target + iter_offset,
                             len, next_proc, &completion, nic_idx);
        shmem_internal_put_wait(SHMEM_CTX_DEFAULT, &completion, nic_idx);
        shmem_internal_fence(SHMEM_CTX_DEFAULT);

        /* send completion for this round to next proc.  Note that we
           only ever sent to next_proc and there's a shmem_fence
           between successive calls to the put above.  So a rolling
           counter is safe here. */
        shmem_internal_atomic(SHMEM_CTX_DEFAULT, pSync, &one, sizeof(long),
                              next_proc, SHM_INTERNAL_SUM, SHM_INTERNAL_LONG, nic_idx);

        /* wait for completion for this round */
        SHMEM_WAIT_UNTIL(pSync, SHMEM_CMP_GE, i);
    }

    /* zero out psync */
    shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, pSync, &zero, sizeof(long), shmem_internal_my_pe, nic_idx);
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
                               int PE_start, int PE_stride, int PE_size, long *pSync,
                               size_t nic_idx)
{
    int my_id = ((shmem_internal_my_pe - PE_start) / PE_stride);
    int i;
    long completion = 0;
    size_t curr_offset;
    int *pSync_ints = (int*) pSync;
    int one = 1, neg_one = -1;
    int distance;

    /* need log2(num_procs) int slots.  max_num_procs is
       2^(sizeof(int)*8-1)-1, so make the math a bit easier and assume
       2^(sizeof(int) * 8), which means log2(num_procs) is always less
       than sizeof(int) * 8. */
    /* Note: pSync can be treated as a byte array rather than an int array to
     * get better cache locality.  We chose int here for portability, since SUM
     * on INT is required by the SHMEM atomics API. */
    shmem_internal_assert(SHMEM_COLLECT_SYNC_SIZE >= (sizeof(int) * 8) / (sizeof(long) / sizeof(int)));
    shmem_internal_assert(0 == (PE_size & (PE_size - 1)));

    if (len == 0) return;

    /* copy my portion to the right place */
    curr_offset = my_id * len;
    shmem_internal_copy_self((char*) target + curr_offset, source, len, nic_idx);

    for (i = 0, distance = 0x1 ; distance < PE_size ; i++, distance <<= 1) {
        int peer = my_id ^ distance;
        int real_peer = PE_start + (peer * PE_stride);

        /* send data to peer */
        shmem_internal_put_nb(SHMEM_CTX_DEFAULT, (char*) target + curr_offset, (char*) target + curr_offset,
                              distance * len, real_peer, &completion, nic_idx);
        shmem_internal_put_wait(SHMEM_CTX_DEFAULT, &completion, nic_idx);
        shmem_internal_fence(SHMEM_CTX_DEFAULT);

        /* mark completion for this round */
        shmem_internal_atomic(SHMEM_CTX_DEFAULT, &pSync_ints[i], &one, sizeof(int),
                              real_peer, SHM_INTERNAL_SUM, SHM_INTERNAL_INT, nic_idx);

        SHMEM_WAIT_UNTIL(&pSync_ints[i], SHMEM_CMP_NE, 0);

        /* this slot is no longer used, so subtract off results now */
        shmem_internal_atomic(SHMEM_CTX_DEFAULT, &pSync_ints[i], &neg_one, sizeof(int),
                              shmem_internal_my_pe, SHM_INTERNAL_SUM, SHM_INTERNAL_INT, nic_idx);

        if (my_id > peer) {
            curr_offset -= (distance * len);
        }
    }

    shmem_internal_quiet(SHMEM_CTX_DEFAULT);
}


void
shmem_internal_alltoall(void *dest, const void *source, size_t len,
                        int PE_start, int PE_stride, int PE_size, long *pSync,
                        size_t nic_idx)
{
    const int my_as_rank = (shmem_internal_my_pe - PE_start) / PE_stride;
    const void *dest_ptr = (uint8_t *) dest + my_as_rank * len;
    int peer, start_pe, i;

    shmem_internal_assert(SHMEM_ALLTOALL_SYNC_SIZE >= SHMEM_BARRIER_SYNC_SIZE);

    if (0 == len)
        return;

    /* Send data round-robin, ending with my PE */
    start_pe = shmem_internal_circular_iter_next(shmem_internal_my_pe,
                                                 PE_start, PE_stride,
                                                 PE_size);
    peer = start_pe;
    do {
        int peer_as_rank = (peer - PE_start) / PE_stride; /* Peer's index in active set */

        shmem_internal_put_nbi(SHMEM_CTX_DEFAULT, (void *) dest_ptr, (uint8_t *) source + peer_as_rank * len,
                              len, peer, nic_idx);
        peer = shmem_internal_circular_iter_next(peer, PE_start, PE_stride,
                                                 PE_size);
    } while (peer != start_pe);

    shmem_internal_barrier(PE_start, PE_stride, PE_size, pSync, nic_idx);

    for (i = 0; i < SHMEM_BARRIER_SYNC_SIZE; i++)
        pSync[i] = SHMEM_SYNC_VALUE;
}


void
shmem_internal_alltoalls(void *dest, const void *source, ptrdiff_t dst,
                         ptrdiff_t sst, size_t elem_size, size_t nelems,
                         int PE_start, int PE_stride, int PE_size, long *pSync,
                         size_t nic_idx)
{
    const int my_as_rank = (shmem_internal_my_pe - PE_start) / PE_stride;
    const void *dest_base = (uint8_t *) dest + my_as_rank * nelems * dst * elem_size;
    int peer, start_pe, i;

    shmem_internal_assert(SHMEM_ALLTOALLS_SYNC_SIZE >= SHMEM_BARRIER_SYNC_SIZE);

    if (0 == nelems)
        return;

    /* Implementation note: Neither OFI nor Portals presently has support for
     * noncontiguous data at the target of a one-sided operation.  I'm not sure
     * of the best communication schedule for the resulting doubly-nested
     * all-to-all.  It may be preferable in some scenarios to exchange the
     * loops below to spread out the communication and decrease the exposure to
     * incast.
     */

    /* Send data round-robin, ending with my PE */
    start_pe = shmem_internal_circular_iter_next(shmem_internal_my_pe,
                                                 PE_start, PE_stride,
                                                 PE_size);
    peer = start_pe;
    do {
        size_t i;
        int peer_as_rank    = (peer - PE_start) / PE_stride; /* Peer's index in active set */
        uint8_t *dest_ptr   = (uint8_t *) dest_base;
        uint8_t *source_ptr = (uint8_t *) source + peer_as_rank * nelems * sst * elem_size;

        for (i = nelems ; i > 0; i--) {
            shmem_internal_put_scalar(SHMEM_CTX_DEFAULT, (void *) dest_ptr, (uint8_t *) source_ptr,
                                     elem_size, peer, nic_idx);

            source_ptr += sst * elem_size;
            dest_ptr   += dst * elem_size;
        }
        peer = shmem_internal_circular_iter_next(peer, PE_start, PE_stride,
                                                 PE_size);
    } while (peer != start_pe);

    shmem_internal_barrier(PE_start, PE_stride, PE_size, pSync, nic_idx);

    for (i = 0; i < SHMEM_BARRIER_SYNC_SIZE; i++)
        pSync[i] = SHMEM_SYNC_VALUE;
}
