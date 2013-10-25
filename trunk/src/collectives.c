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

#include "config.h"

#include "shmem_internal.h"
#include "shmem_collectives.h"


long *barrier_all_psync;
int *full_tree_children;
int full_tree_num_children;
int full_tree_parent;
int tree_crossover = -1;
int tree_radix = -1;

/* #define COLL_DEBUG */

int
build_kary_tree(int PE_start, int stride, int PE_size, int PE_root, int *parent, 
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

    tree_radix = requested_radix;
    tree_crossover = requested_crossover;

    /* initialize barrier_all psync array */
    barrier_all_psync = shmalloc_init(sizeof(long) * _SHMEM_BARRIER_SYNC_SIZE);
    if (NULL == barrier_all_psync) return -1;
    bzero(barrier_all_psync, sizeof(long) * _SHMEM_BARRIER_SYNC_SIZE);

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
    assert(_SHMEM_BARRIER_SYNC_SIZE >= 1);

    shmem_internal_quiet();

    if (PE_start == shmem_internal_my_pe) {
        int pe, i;

        /* wait for N - 1 callins up the tree */
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, PE_size - 1);

        /* Clear pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                 shmem_internal_my_pe);
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);

        /* Send acks down psync tree */
        for (pe = PE_start + stride, i = 1 ; 
             i < PE_size ;  
             i++, pe += stride) {
            shmem_internal_put_small(pSync, &one, sizeof(one), pe);
        }

    } else {
        /* send message to root */
        shmem_internal_atomic_small(pSync, &one, sizeof(one), PE_start, 
                                    PTL_SUM, DTYPE_LONG);

        /* wait for ack down psync tree */
        shmem_long_wait(pSync, 0);

        /* Clear pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                 shmem_internal_my_pe);
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
    }
}


void
shmem_internal_barrier_tree(int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    long zero = 0, one = 1;
    int stride = 1 << logPE_stride;
    int parent, num_children, *children;

    /* need 1 slot */
    assert(_SHMEM_BARRIER_SYNC_SIZE >= 1);

    shmem_internal_quiet();

    if (PE_size == shmem_internal_num_pes) {
        /* we're the full tree, use the binomial tree */
        parent = full_tree_parent;
        num_children = full_tree_num_children;
        children = full_tree_children;
    } else {
        children = alloca(sizeof(int) * tree_radix);
        build_kary_tree(PE_start, stride, PE_size, 0, &parent, 
                        &num_children, children);
    }

    if (num_children != 0) {
        /* Not a pure leaf node */
        int i;

        /* wait for num_children callins up the tree */
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, num_children);

        if (parent == shmem_internal_my_pe) {
            /* The root of the tree */

            /* Clear pSync */
            shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                     shmem_internal_my_pe);
            shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);

            /* Send acks down to children */
            for (i = 0 ; i < num_children ; ++i) {
                shmem_internal_atomic_small(pSync, &one, sizeof(one), 
                                            children[i], 
                                            PTL_SUM, DTYPE_LONG);
            }

        } else {
            /* Middle of the tree */

            /* send ack to parent */
            shmem_internal_atomic_small(pSync, &one, sizeof(one), 
                                        parent, PTL_SUM, DTYPE_LONG);

            /* wait for ack from parent */
            shmem_long_wait_until(pSync, SHMEM_CMP_EQ, num_children  + 1);

            /* Clear pSync */
            shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                     shmem_internal_my_pe);
            shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);

            /* Send acks down to children */
            for (i = 0 ; i < num_children ; ++i) {
                shmem_internal_atomic_small(pSync, &one, sizeof(one),
                                            children[i], 
                                            PTL_SUM, DTYPE_LONG);
            }
        }

    } else {
        /* Leaf node */

        /* send message up psync tree */
        shmem_internal_atomic_small(pSync, &one, sizeof(one), parent, 
                                    PTL_SUM, DTYPE_LONG);

        /* wait for ack down psync tree */
        shmem_long_wait(pSync, 0);

        /* Clear pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                 shmem_internal_my_pe);
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
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
#ifdef COLL_DEBUG
    static int iter = 0;
#endif

    /* need log2(num_procs) int8_t slots.  max_num_procs is
       2^(sizeof(int)*8-1)-1, so make the math a bit easier and assume
       2^(sizeof(int) * 8), which means log2(num_procs) is always less
       than sizeof(int) * 8. */
    assert(_SHMEM_BARRIER_SYNC_SIZE >= (sizeof(int) * 8) / sizeof(long));

    shmem_internal_quiet();

#ifdef COLL_DEBUG
    fprintf(stderr, "%d [%03d] start barrier\n", iter, shmem_internal_my_pe);
#endif

    for (i = 0, distance = 1 ; distance < PE_size ; ++i, distance <<= 1) {
        to = ((coll_rank + distance) % PE_size);
        to = PE_start + (to * stride);
#ifdef COLL_DEBUG
        fprintf(stderr, "%d [%03d] barrier (iter %02d) send to %03d\n", 
                iter, shmem_internal_my_pe, i, to);
#endif

        shmem_internal_atomic_small(&pSync_bytes[i], &one, sizeof(int8_t),
                                    to,
                                    PTL_SUM, PTL_INT8_T);

#ifdef COLL_DEBUG
        fprintf(stderr, "%d [%03d] barrier (iter %02d) wait on %d\n", 
                iter, shmem_internal_my_pe, i, (int) pSync_bytes[i]);
#endif

        SHMEM_WAIT_UNTIL(&pSync_bytes[i], SHMEM_CMP_EQ, 1);

#ifdef COLL_DEBUG
        fprintf(stderr, "%d [%03d] barrier (iter %02d) done waiting on %d\n", 
                iter, shmem_internal_my_pe, i, (int) pSync_bytes[i]);
#endif

        /* this slot is no longer used, so subtract off results now */
        shmem_internal_atomic_small(&pSync_bytes[i], &neg_one, sizeof(int8_t),
                                    shmem_internal_my_pe,
                                    PTL_SUM, PTL_INT8_T);
    }

    shmem_internal_quiet();

#ifdef COLL_DEBUG
    fprintf(stderr, "%d [%03d] end barrier\n", iter, shmem_internal_my_pe);
    iter++;
#endif
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
    assert(_SHMEM_BCAST_SYNC_SIZE >= 1);

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
            shmem_long_wait_until(pSync, SHMEM_CMP_EQ, PE_size - 1);

            /* Clear pSync */
            shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                     shmem_internal_my_pe);
            shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
        }

    } else {
        /* wait for data arrival message */
        shmem_long_wait(pSync, 0);

        /* Clear pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                 shmem_internal_my_pe);
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
            
        if (1 == complete) {
            /* send ack back to root */
            shmem_internal_atomic_small(pSync, &one, sizeof(one), 
                                        real_root, 
                                        PTL_SUM, DTYPE_LONG);
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
    assert(_SHMEM_BCAST_SYNC_SIZE >= 1);

    if (PE_size == shmem_internal_num_pes && 0 == PE_root) {
        /* we're the full tree, use the binomial tree */
        parent = full_tree_parent;
        num_children = full_tree_num_children;
        children = full_tree_children;
    } else {
        children = alloca(sizeof(int) * tree_radix);
        build_kary_tree(PE_start, stride, PE_size, PE_root, &parent, 
                        &num_children, children);
    }

    if (0 != num_children) {
        int i;

        if (parent != shmem_internal_my_pe) {
            /* wait for data arrival message if not the root */
            shmem_long_wait(pSync, 0);

            /* if complete, send ack */
            if (1 == complete) {
                shmem_internal_atomic_small(pSync, &one, sizeof(one),
                                            parent,
                                            PTL_SUM, DTYPE_LONG);
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
            shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 
                                  num_children  + 
                                  ((parent == shmem_internal_my_pe) ?
                                   0 : 1));
        }

        /* Clear pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                 shmem_internal_my_pe);
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);

    } else {
        /* wait for data arrival message */
        shmem_long_wait(pSync, 0);

        /* if complete, send ack */
        if (1 == complete) {
            shmem_internal_atomic_small(pSync, &one, sizeof(one),
                                        parent,
                                        PTL_SUM, DTYPE_LONG);
        }
            
        /* Clear pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), 
                                 shmem_internal_my_pe);
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
    }
}


/*****************************************
 *
 * REDUCTION
 *
 *****************************************/
void
shmem_internal_op_to_all_linear(void *target, void *source, int count, int type_size,
                                int PE_start, int logPE_stride, int PE_size,
                                void *pWrk, long *pSync, 
                                ptl_op_t op, ptl_datatype_t datatype)
{
    int stride = 1 << logPE_stride;
    long zero = 0, one = 1;
    long completion = 0;

    /* need 2 slots, plus bcast */
    assert(_SHMEM_REDUCE_SYNC_SIZE >= 2 + _SHMEM_BCAST_SYNC_SIZE);

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
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, PE_size - 1);

        /* reset pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), shmem_internal_my_pe);
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);

    } else {
        /* wait for clear to send */
        shmem_long_wait(pSync, 0);

        /* reset pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), shmem_internal_my_pe);
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);

        /* send data, ack, and wait for completion */
        shmem_internal_atomic_nb(target, source, count * type_size, PE_start,
                                 op, datatype, &completion);
        shmem_internal_put_wait(&completion);
        shmem_internal_fence();

        shmem_internal_atomic_small(pSync, &one, sizeof(one), PE_start, PTL_SUM, DTYPE_LONG);
    }

    /* broadcast out */
    shmem_internal_bcast(target, target, count * type_size, 0, PE_start, 
                         logPE_stride, PE_size, pSync + 2, 0);
}


void
shmem_internal_op_to_all_tree(void *target, void *source, int count, int type_size,
                              int PE_start, int logPE_stride, int PE_size,
                              void *pWrk, long *pSync, 
                              ptl_op_t op, ptl_datatype_t datatype)
{
    int stride = 1 << logPE_stride;
    long zero = 0, one = 1;
    long completion = 0;
    int parent, num_children, *children;

    /* need 2 slots, plus bcast */
    assert(_SHMEM_REDUCE_SYNC_SIZE >= 2 + _SHMEM_BCAST_SYNC_SIZE);

    if (PE_size == shmem_internal_num_pes) {
        /* we're the full tree, use the binomial tree */
        parent = full_tree_parent;
        num_children = full_tree_num_children;
        children = full_tree_children;
    } else {
        children = alloca(sizeof(int) * tree_radix);
        build_kary_tree(PE_start, stride, PE_size, 0, &parent, 
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
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, num_children);

        /* reset pSync */
        shmem_internal_put_small(pSync, &zero, sizeof(zero), shmem_internal_my_pe);
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
    }

    if (parent != shmem_internal_my_pe) {
        /* wait for clear to send */
        shmem_long_wait(pSync + 1, 0);

        /* reset pSync */
        shmem_internal_put_small(pSync + 1, &zero, sizeof(zero), shmem_internal_my_pe);
        shmem_long_wait_until(pSync + 1, SHMEM_CMP_EQ, 0);

        /* send data, ack, and wait for completion */
        shmem_internal_atomic_nb(target, (num_children == 0) ? source : target,
                                 count * type_size, parent,
                                 op, datatype, &completion);
        shmem_internal_put_wait(&completion);
        shmem_internal_fence();

        shmem_internal_atomic_small(pSync, &one, sizeof(one), parent, PTL_SUM, DTYPE_LONG);
    }

    /* broadcast out */
    shmem_internal_bcast(target, target, count * type_size, 0, PE_start, 
                         logPE_stride, PE_size, pSync + 2, 0);
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
    assert(_SHMEM_COLLECT_SYNC_SIZE >= 3 + _SHMEM_BCAST_SYNC_SIZE);

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
        shmem_long_wait_until(&pSync[1], SHMEM_CMP_EQ, 1);

	/* find out how long total data was */
        bcast_len = pSync[0];
    } else {
        /* wait for send data */
        shmem_long_wait_until(&pSync[1], SHMEM_CMP_EQ, 1);

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
    shmem_internal_put_nb((char*) target + my_offset, source, len, PE_start,
                          &completion);
    shmem_internal_fence();
    shmem_internal_put_wait(&completion);

    /* Let PE_start know we're done.  This can definitely be a tree. */
    tmp[0] = 1;
    shmem_internal_atomic_small(pSync + 2, &tmp[0], sizeof(tmp[0]), PE_start,
                                PTL_SUM, DTYPE_LONG);

    /* root waits for completion */
    if (PE_start == shmem_internal_my_pe) {
        shmem_long_wait_until(&pSync[2], SHMEM_CMP_EQ, 1);
    }

    /* clear pSync */
    tmp[0] = tmp[1] = tmp[2] = 0;
    shmem_internal_put_small(pSync, tmp, 3 * sizeof(long), shmem_internal_my_pe);

    /* broadcast out */
    shmem_internal_bcast(target, target, bcast_len, 0, PE_start, logPE_stride, PE_size, pSync + 3, 0);

    /* make sure our pSync is clean before we leave... */
    shmem_long_wait_until(&pSync[1], SHMEM_CMP_EQ, 0);
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
    assert(_SHMEM_COLLECT_SYNC_SIZE >= 1 + _SHMEM_BCAST_SYNC_SIZE);

    if (PE_start == shmem_internal_my_pe) {
        /* Copy data into the target */
        if (source != target) memcpy(target, source, len);

        /* send completion update */
        shmem_internal_atomic_small(pSync, &tmp, sizeof(long), PE_start, PTL_SUM, DTYPE_LONG);

        /* wait for N updates */
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, PE_size);

        /* Clear pSync */
        tmp = 0;
        shmem_internal_put_small(pSync, &tmp, sizeof(tmp), PE_start);
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
    } else {
        /* Push data into the target */
        size_t offset = ((shmem_internal_my_pe - PE_start) / stride) * len;
        shmem_internal_put_nb((char*) target + offset, source, len, PE_start,
                              &completion);
        shmem_internal_put_wait(&completion);

        /* ensure ordering */
        shmem_internal_fence();

        /* send completion update */
        shmem_internal_atomic_small(pSync, &tmp, sizeof(long), PE_start, PTL_SUM, DTYPE_LONG);
    }

    shmem_internal_bcast(target, target, len * PE_size, 0, PE_start, logPE_stride, 
                         PE_size, pSync + 1, 0);
}
