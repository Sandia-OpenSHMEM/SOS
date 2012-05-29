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

#include <portals4.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"


static long *barrier_all_psync;

static int *full_tree_children;
static int full_tree_num_children;
static int full_tree_parent;

static int tree_crossover = -1;
static int tree_radix = -1;


static int
build_kary_tree(int PE_start, int stride, int PE_size, int *parent, 
                int *num_children, int *children)
{
    int i;
    /* my_id is the index in a theoretical 0...N-1 array of
       participating tasks */
    int my_id = (shmem_internal_my_pe - PE_start) / stride;

    *parent = PE_start + ((my_id - 1) / tree_radix) * stride;

    *num_children = 0;
    for (i = 1 ; i <= tree_radix ; ++i) {
        int tmp = tree_radix * my_id + i;
        if (tmp < PE_size) {
            children[(*num_children)++] = PE_start + tmp * stride;
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
    barrier_all_psync = shmalloc(sizeof(long) * _SHMEM_BARRIER_SYNC_SIZE);
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


void
shmem_barrier_all(void)
{

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif
    shmem_quiet();
    shmem_barrier(0, 0, shmem_internal_num_pes, barrier_all_psync);
}


/* Simple fan-in/fan-out algorithm. Should be safe to reuse pSync
   array immediately after return */
void
shmem_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    int ret = 0;
    long zero = 0, one = 1;
    int stride = (logPE_stride == 0) ? 1 : 1 << logPE_stride;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    if (PE_size <= tree_crossover) {
        if (PE_start == shmem_internal_my_pe) {
            int pe, i;
            /* wait for N - 1 callins up the tree */
            shmem_long_wait_until(pSync, SHMEM_CMP_EQ, PE_size - 1);

            /* Clear pSync */
            ret = shmem_internal_put(pSync, &zero, sizeof(zero), 
                                     shmem_internal_my_pe);
            shmem_internal_put_wait(ret);
            shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);

            ret = 0;
            /* Send acks down psync tree */
            for (pe = PE_start + stride, i = 1 ; 
                 i < PE_size ;  
                 i++, pe += stride) {
                ret += shmem_internal_put(pSync, &one, sizeof(one), pe);
            }
            shmem_internal_put_wait(ret);

        } else {
            /* send message to root */
            ret = shmem_internal_atomic(pSync, &one, sizeof(one), PE_start, 
                                        PTL_SUM, DTYPE_LONG);
            shmem_internal_put_wait(ret);
            /* wait for ack down psync tree */
            shmem_long_wait(pSync, 0);

            /* Clear pSync */
            ret = shmem_internal_put(pSync, &zero, sizeof(zero), 
                                     shmem_internal_my_pe);
            shmem_internal_put_wait(ret);
            shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
        }

    } else {
        int parent, num_children, *children;

        if (PE_size == shmem_n_pes()) {
            /* we're the full tree, use the binomial tree */
            parent = full_tree_parent;
            num_children = full_tree_num_children;
            children = full_tree_children;
        } else {
            children = alloca(sizeof(int) * tree_radix);
            build_kary_tree(PE_start, stride, PE_size, &parent, 
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
                ret += shmem_internal_put(pSync, &zero, sizeof(zero), 
                                          shmem_internal_my_pe);
                shmem_internal_put_wait(ret);
                shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);

                /* Send acks down to children */
                ret = 0;
                for (i = 0 ; i < num_children ; ++i) {
                    ret += shmem_internal_atomic(pSync, &one, sizeof(one), 
                                                 children[i], 
                                                 PTL_SUM, DTYPE_LONG);
                }
                shmem_internal_put_wait(ret);

            } else {
                /* Middle of the tree */

                /* send ack to parent */
                ret = shmem_internal_atomic(pSync, &one, sizeof(one), 
                                            parent, PTL_SUM, DTYPE_LONG);
                shmem_internal_put_wait(ret);

                /* wait for ack from parent */
                shmem_long_wait_until(pSync, SHMEM_CMP_EQ, num_children  + 1);

                /* Clear pSync */
                ret += shmem_internal_put(pSync, &zero, sizeof(zero), 
                                          shmem_internal_my_pe);
                shmem_internal_put_wait(ret);
                shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);

                /* Send acks down to children */
                ret = 0;
                for (i = 0 ; i < num_children ; ++i) {
                    ret += shmem_internal_atomic(pSync, &one, sizeof(one),
                                                 children[i], 
                                                 PTL_SUM, DTYPE_LONG);
                }
                shmem_internal_put_wait(ret);
            }

        } else {
            /* Leaf node */

            /* send message up psync tree */
            ret += shmem_internal_atomic(pSync, &one, sizeof(one), parent, 
                                         PTL_SUM, DTYPE_LONG);
            shmem_internal_put_wait(ret);

            /* wait for ack down psync tree */
            shmem_long_wait(pSync, 0);

            /* Clear pSync */
            ret += shmem_internal_put(pSync, &zero, sizeof(zero), 
                                      shmem_internal_my_pe);
            shmem_internal_put_wait(ret);
            shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
        }
    }

    shmem_quiet();
}


static inline
void
shmem_internal_bcast(void *target, const void *source, size_t len,
                     int PE_root, int PE_start, int logPE_stride, int PE_size,
                     long *pSync, int complete)
{
    int ret = 0;
    long zero = 0, one = 1;
    int stride = (logPE_stride == 0) ? 1 : 1 << logPE_stride;
    int real_root = PE_start + PE_root * stride;

    if (PE_size < tree_crossover) {
        if (real_root == shmem_internal_my_pe) {
            int i, pe;

            /* send data to all peers */
            for (pe = PE_start,i=0; i < PE_size; pe += stride, i++) {
                if (pe == shmem_internal_my_pe && source == target) continue;
                ret += shmem_internal_put(target, source, len, pe);
            }
            shmem_internal_put_wait(ret);
            ret = 0;
    
            shmem_fence();
    
            /* send completion ack to all peers */
            for (pe = PE_start,i=0; i < PE_size; pe += stride, i++) {
                if (pe == shmem_internal_my_pe) continue;
                ret += shmem_internal_put(pSync, &one, sizeof(long), pe);
            }
            shmem_internal_put_wait(ret);

            if (1 == complete) {
                /* wait for acks from everyone */
                shmem_long_wait_until(pSync, SHMEM_CMP_EQ, PE_size - 1);

                /* Clear pSync */
                ret += shmem_internal_put(pSync, &zero, sizeof(zero), 
                                          shmem_internal_my_pe);
                shmem_internal_put_wait(ret);
                shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
            }

        } else {
            /* wait for data arrival message */
            shmem_long_wait(pSync, 0);

            /* Clear pSync */
            ret += shmem_internal_put(pSync, &zero, sizeof(zero), 
                                      shmem_internal_my_pe);
            shmem_internal_put_wait(ret);
            shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
            
            if (1 == complete) {
                /* send ack back to root */
                ret += shmem_internal_atomic(pSync, &one, sizeof(one), 
                                             real_root, 
                                             PTL_SUM, DTYPE_LONG);
                shmem_internal_put_wait(ret);
            }
        }

    } else {
        int parent, num_children, *children;
        const void *send_buf = source;

        if (PE_size == shmem_n_pes()) {
            /* we're the full tree, use the binomial tree */
            parent = full_tree_parent;
            num_children = full_tree_num_children;
            children = full_tree_children;
        } else {
            children = alloca(sizeof(int) * tree_radix);
            build_kary_tree(PE_start, stride, PE_size, &parent, 
                            &num_children, children);
        }

        if (real_root != PE_start) {
            if (shmem_internal_my_pe == PE_start) {
                /* wait for data arrival message */
                shmem_long_wait(pSync, 0);

                /* Clear pSync */
                ret += shmem_internal_put(pSync, &zero, sizeof(zero), 
                                          shmem_internal_my_pe);
                shmem_internal_put_wait(ret);
                shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
                
                /* send buf is now in the target buffer */
                send_buf = target;
                
            } else if (shmem_internal_my_pe == real_root) {
                /* send data to 0th entry */
                ret = shmem_internal_put(target, source, len, PE_start);
                shmem_fence();
                ret += shmem_internal_put(pSync, &one, sizeof(long), PE_start);
                shmem_internal_put_wait(ret);
            }

        } else if (real_root == shmem_internal_my_pe && source != target) {
            memcpy(target, source, len);
        }

        if (0 != num_children) {
            int i;

            if (real_root != shmem_internal_my_pe) {
                /* wait for data arrival message if not the root */
                shmem_long_wait(pSync, 0);

                /* if complete, send ack */
                if (1 == complete) {
                    ret += shmem_internal_atomic(pSync, &one, sizeof(one),
                                                 parent,
                                                 PTL_SUM, DTYPE_LONG);
                    shmem_internal_put_wait(ret);
                }
            }

            /* send data to all leaves */
            for (i = 0 ; i < num_children ; ++i) {
                ret += shmem_internal_put(target, send_buf, len, children[i]);
            }
            shmem_internal_put_wait(ret);
            ret = 0;
    
            shmem_fence();
    
            /* send completion ack to all peers */
            for (i = 0 ; i < num_children ; ++i) {
                ret += shmem_internal_put(pSync, &one, sizeof(long), 
                                          children[i]);
            }
            shmem_internal_put_wait(ret);

            if (1 == complete) {
                /* wait for acks from everyone */
                shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 
                                      num_children  + 
                                      ((real_root == shmem_internal_my_pe) ?
                                       0 : 1));

                /* Clear pSync */
                ret += shmem_internal_put(pSync, &zero, sizeof(zero), 
                                          shmem_internal_my_pe);
                shmem_internal_put_wait(ret);
                shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
            }

        } else {
            /* wait for data arrival message */
            shmem_long_wait(pSync, 0);

            /* if complete, send ack */
            if (1 == complete) {
                ret += shmem_internal_atomic(pSync, &one, sizeof(one),
                                             parent,
                                             PTL_SUM, DTYPE_LONG);
                shmem_internal_put_wait(ret);
            }
            
            /* Clear pSync */
            ret += shmem_internal_put(pSync, &zero, sizeof(zero), 
                                      shmem_internal_my_pe);
            shmem_internal_put_wait(ret);
            shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
        }
    }
}


/* Simple fan-in with atomics with a twist.  PE_start must initialize
   its target before allowing any other rank to do the atomic
   operation.  PE_start does a put from source to target, waits for
   completion, then sends a ping to each peer's pSync.  At that point,
   the peers atomic into PE_start's target and send an atomic incr
   into PE_start's pSync and complete.  PE_start completes when it has
   PE_size - 1 counters in its pSync. */
static inline
void
shmem_internal_op_to_all(void *target, void *source, int count, int type_size,
                    int PE_start, int logPE_stride, int PE_size,
                    void *pWrk, long *pSync, 
                    ptl_op_t op, ptl_datatype_t datatype)
{
    int stride = (logPE_stride == 0) ? 1 : 1 << logPE_stride;
    int ret = 0;
    long zero = 0, one = 1;

    if (PE_start == shmem_internal_my_pe) {
        int pe, i;
        /* update our target buffer with our contribution */
        ret = shmem_internal_put(target, source, count * type_size, shmem_internal_my_pe);
        shmem_internal_put_wait(ret);
        shmem_fence();
        ret = shmem_internal_put(pSync, &one, sizeof(one), shmem_internal_my_pe);
        shmem_internal_put_wait(ret);
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 1);

        /* let everyone know that it's safe to send to us */
        ret = 0;
        for (pe = PE_start + stride, i = 1 ; 
             i < PE_size ;  
             i++, pe += stride) {
            ret += shmem_internal_put(pSync, &one, sizeof(one), pe);
        }
        shmem_internal_put_wait(ret);

        /* Wait for others to acknowledge sending data */
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, PE_size);

        /* reset pSync; atomics used, so have to use Portals op */
        ret = 0;
        ret += shmem_internal_put(pSync, &zero, sizeof(zero), shmem_internal_my_pe);
        shmem_internal_put_wait(ret);
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);

    } else {
        /* wait for clear to send */
        shmem_long_wait(pSync, 0);

        /* reset pSync */
        pSync[0] = 0;

        /* send data, ack, and wait for completion */
        ret = shmem_internal_atomic(target, source, count * type_size, PE_start, op, datatype);
        shmem_internal_put_wait(ret);

        shmem_fence();

        ret = shmem_internal_atomic(pSync, &one, sizeof(one), PE_start, PTL_SUM, DTYPE_LONG);
        shmem_internal_put_wait(ret);
    }

    /* broadcast out */
    shmem_internal_bcast(target, target, count * type_size, 0, PE_start, logPE_stride, PE_size, pSync, 0);
}


/* len can be different on each node, so this is semi-linear.
   PE_Start starts a copy of his data into target, and increments a
   length counter, sending that counter to the next PE.  That PE
   starts the transfer to counter offset, increments the counter, and
   send the counter to the next PE.  The sends are non-blocking to
   different peers (PE_start and next), so some overlap is likely.
   Finally, the last peer sends the counter back to PE_start so that
   the size of the data to broadcast is known. */
static inline
void
shmem_internal_collect(void *target, const void *source, size_t len,
                  int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    int ret = 0;
    long tmp[2] = {0, 1};
    int stride = (logPE_stride == 0) ? 1 : 1 << logPE_stride;
    int pe;
    int bcast_len = 0;

    if (PE_size == 1) {
        if (target != source) memcpy(target, source, len);
        return;
    }

    /* collect in PE_start */
    if (PE_start == shmem_internal_my_pe) {
        if (target != source) {
            ret += shmem_internal_put(target, source, len, PE_start);
        }
        tmp[0] = len;
        ret += shmem_internal_put(pSync, tmp, 2 * sizeof(long), PE_start + stride);
        shmem_internal_put_wait(ret);
        shmem_long_wait(&pSync[1], 0);
        bcast_len = pSync[0];
        pSync[0] = pSync[1] = 0;

        /* make sure put to ourselves is completed before broadcast */
        shmem_quiet();

    } else {
        /* wait for send data */
        shmem_long_wait(&pSync[1], 0);

        /* send data to root */
        ret = shmem_internal_put((char*) target + pSync[0], source, len, PE_start);
        if (shmem_internal_my_pe == PE_start + stride * (PE_size - 1)) {
            pe = PE_start;

            /* need to fence if sending completion to root to keep ordering */
            shmem_internal_put_wait(ret);
            ret = 0;
            shmem_fence();

        } else {
            pe = shmem_internal_my_pe + stride;
        }
        tmp[0] = pSync[0] + len;
        pSync[0] = pSync[1] = 0;
        ret += shmem_internal_put(pSync, tmp, 2 * sizeof(long), pe);
        shmem_internal_put_wait(ret);
    }

    /* broadcast out */
    shmem_internal_bcast(target, target, bcast_len, 0, PE_start, logPE_stride, PE_size, pSync, 0);
}


/* Since offsets can be directly computed, each rank puts directly
   into their offset in PE_Start's target, then increments a counter
   in pSync.  PE_start then broadcasts to everyone. */
static inline
void
shmem_internal_fcollect(void *target, const void *source, size_t len,
                   int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    int ret = 0;
    long tmp = 1;
    int stride = (logPE_stride == 0) ? 1 : 1 << logPE_stride;

    if (PE_start == shmem_internal_my_pe) {
        if (source != target) {
            ret = shmem_internal_put(target, source, len, PE_start);
            shmem_internal_put_wait(ret);
            shmem_fence();
            ret = shmem_internal_atomic(pSync, &tmp, sizeof(long), PE_start, PTL_SUM, DTYPE_LONG);
        }
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, PE_size);
        tmp = 0;
        ret += shmem_internal_put(pSync, &tmp, sizeof(tmp), PE_start);
        shmem_internal_put_wait(ret);
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, 0);
    } else {
        size_t offset = ((shmem_internal_my_pe - PE_start) / stride) * len;
        ret = shmem_internal_put((char*) target + offset, source, len, PE_start);
        shmem_internal_put_wait(ret);
        shmem_fence();
        ret = shmem_internal_atomic(pSync, &tmp, sizeof(long), PE_start, PTL_SUM, DTYPE_LONG);
        shmem_internal_put_wait(ret);
    }
    
    shmem_internal_bcast(target, target, len * PE_size, 0, PE_start, logPE_stride, PE_size, pSync, 0);
}


void
shmem_short_and_to_all(short *target, short *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size,
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LAND, DTYPE_SHORT);
}


void
shmem_int_and_to_all(int *target, int *source, int nreduce,
                     int PE_start, int logPE_stride, int PE_size,
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LAND, DTYPE_INT);
}


void
shmem_long_and_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size,
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LAND, DTYPE_LONG);
}


void
shmem_longlong_and_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size,
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LAND, DTYPE_LONG);
}


void
shmem_short_or_to_all(short *target, short *source, int nreduce, 
                      int PE_start, int logPE_stride, int PE_size, 
                      short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BOR, DTYPE_SHORT);
}


void
shmem_int_or_to_all(int *target, int *source, int nreduce, 
                    int PE_start, int logPE_stride, int PE_size, 
                    int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BOR, DTYPE_INT);
}


void
shmem_long_or_to_all(long *target, long *source, int nreduce,
                     int PE_start, int logPE_stride, int PE_size, 
                     long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BOR, DTYPE_LONG);
}


void
shmem_longlong_or_to_all(long long *target, long long *source, int nreduce,
                         int PE_start, int logPE_stride, int PE_size, 
                         long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BOR, DTYPE_LONG);
}


void
shmem_short_xor_to_all(short *target, short *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size, 
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BXOR, DTYPE_SHORT);
}

void
shmem_int_xor_to_all(int *target, int *source, int nreduce, 
                     int PE_start, int logPE_stride, int PE_size, 
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BXOR, DTYPE_INT);
}


void
shmem_long_xor_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BXOR, DTYPE_LONG);
}


void
shmem_longlong_xor_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size, 
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_BXOR, DTYPE_LONG);
}


void
shmem_float_min_to_all(float *target, float *source, int nreduce, 
                       int PE_start, int logPE_stride, int PE_size, 
                       float *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_FLOAT);
}


void
shmem_double_min_to_all(double *target, double *source, int nreduce,
                        int PE_start, int logPE_stride, int PE_size, 
                        double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_DOUBLE);
}


void
shmem_longdouble_min_to_all(long double *target, long double *source, int nreduce, 
                            int PE_start, int logPE_stride, int PE_size,
                            long double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_LONG_DOUBLE);
}


void
shmem_short_min_to_all(short *target, short *source, int nreduce, 
                       int PE_start, int logPE_stride, int PE_size, 
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, DTYPE_SHORT);
}


void
shmem_int_min_to_all(int *target, int *source, int nreduce, 
                     int PE_start, int logPE_stride, int PE_size, 
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, DTYPE_INT);
}


void
shmem_long_min_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, DTYPE_LONG);
}


void
shmem_longlong_min_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size,
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, DTYPE_LONG);
}


void
shmem_float_max_to_all(float *target, float *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size,
                       float *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_FLOAT);
}


void
shmem_double_max_to_all(double *target, double *source, int nreduce,
                        int PE_start, int logPE_stride, int PE_size,
                        double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_DOUBLE);
}


void
shmem_longdouble_max_to_all(long double *target, long double *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size,
                            long double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_LONG_DOUBLE);
}


void
shmem_short_max_to_all(short *target, short *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size,
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, DTYPE_SHORT);
}


void
shmem_int_max_to_all(int *target, int *source, int nreduce,
                     int PE_start, int logPE_stride, int PE_size,
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, DTYPE_INT);
}


void
shmem_long_max_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size,
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, DTYPE_LONG);
}


void 
shmem_longlong_max_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size,
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, DTYPE_LONG);
}


void
shmem_float_sum_to_all(float *target, float *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size, 
                       float *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_FLOAT);
}


void
shmem_double_sum_to_all(double *target, double *source, int nreduce, 
                        int PE_start, int logPE_stride, int PE_size, 
                        double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_DOUBLE);
}


void
shmem_longdouble_sum_to_all(long double *target, long double *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size,
                            long double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_LONG_DOUBLE);
}


void
shmem_complexf_sum_to_all(float complex *target, float complex *source, int nreduce, 
                          int PE_start, int logPE_stride, int PE_size,
                          float complex *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float complex),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_FLOAT_COMPLEX);
}


void 
shmem_complexd_sum_to_all(double complex *target, double complex *source, int nreduce, 
                          int PE_start, int logPE_stride, int PE_size, 
                          double complex *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double complex),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_DOUBLE_COMPLEX);
}


void
shmem_short_sum_to_all(short *target, short *source, int nreduce, 
                       int PE_start, int logPE_stride, int PE_size, 
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, DTYPE_SHORT);
}


void
shmem_int_sum_to_all(int *target, int *source, int nreduce, 
                     int PE_start, int logPE_stride, int PE_size, 
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, DTYPE_INT);
}


void
shmem_long_sum_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, DTYPE_LONG);
}


void
shmem_longlong_sum_to_all(long long *target, long long *source, int nreduce, 
                          int PE_start, int logPE_stride, int PE_size, 
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, DTYPE_LONG);
}


void
shmem_float_prod_to_all(float *target, float *source, int nreduce, 
                        int PE_start, int logPE_stride, int PE_size, 
                        float *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_FLOAT);
}


void
shmem_double_prod_to_all(double *target, double *source, int nreduce, 
                         int PE_start, int logPE_stride, int PE_size,
                         double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_DOUBLE);
}


void
shmem_longdouble_prod_to_all(long double *target, long double *source, int nreduce, 
                             int PE_start, int logPE_stride, int PE_size,
                             long double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_LONG_DOUBLE);
}


void
shmem_complexf_prod_to_all(float complex *target, float complex *source, int nreduce, 
                           int PE_start, int logPE_stride, int PE_size,
                           float complex *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float complex),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_FLOAT_COMPLEX);
}


void
shmem_complexd_prod_to_all(double complex *target, double complex *source, int nreduce, 
                           int PE_start, int logPE_stride, int PE_size, 
                           double complex *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double complex),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_DOUBLE_COMPLEX);
}


void
shmem_short_prod_to_all(short *target, short *source, int nreduce, 
                        int PE_start, int logPE_stride, int PE_size, 
                        short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, DTYPE_SHORT);
}


void
shmem_int_prod_to_all(int *target, int *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, DTYPE_INT);
}


void
shmem_long_prod_to_all(long *target, long *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size, 
                       long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, DTYPE_LONG);
}


void
shmem_longlong_prod_to_all(long long *target, long long *source, int nreduce, 
                           int PE_start, int logPE_stride, int PE_size,
                           long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, DTYPE_LONG);
}


void
shmem_broadcast32(void *target, const void *source, size_t nlong, 
                  int PE_root, int PE_start, int logPE_stride, int PE_size,
                  long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_bcast(target, source, nlong * 4,
                         PE_root, PE_start, logPE_stride, PE_size,
                         pSync, 1);
}


void
shmem_broadcast64(void *target, const void *source, size_t nlong,
                  int PE_root, int PE_start, int logPE_stride, int PE_size,
                  long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_bcast(target, source, nlong * 8,
                         PE_root, PE_start, logPE_stride, PE_size,
                         pSync, 1);
}


void
shmem_collect32(void *target, const void *source, size_t nlong,
                int PE_start, int logPE_stride, int PE_size, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_collect(target, source, nlong * 4,
                      PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_collect64(void *target, const void *source, size_t nlong,
                int PE_start, int logPE_stride, int PE_size, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_collect(target, source, nlong * 8,
                      PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_fcollect32(void *target, const void *source, size_t nlong,
                 int PE_start, int logPE_stride, int PE_size, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fcollect(target, source, nlong * 4,
                       PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_fcollect64(void *target, const void *source, size_t nlong,
                 int PE_start, int logPE_stride, int PE_size, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fcollect(target, source, nlong * 8,
                       PE_start, logPE_stride, PE_size, pSync);
}

