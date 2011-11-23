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

#define MAX(A, B) ((A > B) ? A : B)
#define MIN(A, B) ((A < B) ? A : B)

static long *barrier_work_array;

int
shmem_internal_barrier_init(void)
{
    barrier_work_array = shmalloc(sizeof(long) * _SHMEM_BARRIER_SYNC_SIZE);
    if (NULL == barrier_work_array) return -1;
    bzero(barrier_work_array, sizeof(long) * _SHMEM_BARRIER_SYNC_SIZE);
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
    shmem_barrier(0, 0, shmem_internal_num_pes, barrier_work_array);
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

    if (PE_start == shmem_internal_my_pe) {
        int pe, i;
        /* wait for N - 1 callins up the tree */
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, PE_size - 1);

        /* Clear pSync; have to do local assignment to clear out NIC atomic cache */
        ret += shmem_internal_put(pSync, &zero, sizeof(zero), shmem_internal_my_pe);
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
        /* send message up psync tree */
        ret += shmem_internal_atomic(pSync, &one, sizeof(one), PE_start, PTL_SUM, DTYPE_LONG);
        shmem_internal_put_wait(ret);
        /* wait for ack down psync tree */
        shmem_long_wait(pSync, 0);
        /* Clear pSync; never atomically incremented, ok to direct assign */
        pSync[0] = 0;
    }
}


/* Logically map an N-ary (N==radix arg) tree on to the PE active set, always
 * assume start_PE == root_PE.
 * 'active set' is defined as [start_pe ... start_pe+PE_size by stride].
 * The 'root_PE' # is within the active set and is where the broadcast message
 * cascade originates from.
 * If the 'root_PE' were not the start_pe, then the pe numbering sequence would
 * contain a hole where root_PE should have been as the root PE has been switched
 * to be the first PE (aka root of the tree). Computing the children of a specific PE
 * becomes overly complex in having to account for the root_PE hole in the PE
 * numbering sequence.
 * In order to simplify the computation (aka make the child nodes computation
 * faster) the tree_root is ALWAYS the start_pe with root_PE in it's expected
 * position in the n_ary tree; as if root_PE == start_PE.
 * For those cases where start_PE != root_PE then prior to returning, switch 
 * root_PE value for start_PE and vice-a-versa.
 *
 * Return:
 *   0 == Success, else error code.
 *
 * Output argument 'children' is an int vector of radix elements; each element
 * is initialized to (-1) which represents no child PE (leaf).
 * children elements which are not euqal to -1 represent the children PEs #.
 */

static int
find_my_children( int target_pe, int start_pe, int num_pe, int pe_stride,
                  int radix, int root_PE, int *children )
{
    int j, i, find_pe, level, pe, pe_cnt, pe_end, lvl_cnt, lvl_offset;
    int nxl_offset, nxl_pe, nxl_pe_cnt, nxl_pe_end, child_start, child, tmp;

    for(j=0; j < radix; j++)
        children[j] = -1;   // (-1) implies Leaf node.

    /* if real_root != start_pe then logically swap the root_PE & start_pe.
     * Such that if target_pe = root_PE, then compute for 'start_pe' value.
     * If target_pe == start_pe, then compute for root_PE value.
     */
    find_pe = target_pe;
    if (root_PE != start_pe) {
        if (target_pe == root_PE)
            find_pe = start_pe;
        else if (target_pe == start_pe)
            find_pe = root_PE;
    }

    pe_cnt = num_pe;
    pe = start_pe;

    for(i=0,level=0,lvl_cnt=1; i < num_pe; level++,i++ ) {
        pe_end = pe + ((lvl_cnt - 1) * pe_stride); // pow(radix,level)
        if ( pe <= find_pe && find_pe <= pe_end ) {
            lvl_offset = find_pe - pe;
            if ( lvl_offset >= pe_stride && pe_stride > 1 )
                lvl_offset /= pe_stride;
            pe_cnt -= lvl_cnt; // account for nodes @ current level.
            if ( pe_cnt <= 0 )
                return 0; // LEAF node.
            nxl_pe_cnt = lvl_cnt * radix;// nxt tree level: num pes
            nxl_pe = pe_end + pe_stride;
            nxl_pe_end = nxl_pe + ((nxl_pe_cnt-1) * pe_stride);

            nxl_offset = lvl_offset * radix;
            child_start = nxl_pe + (nxl_offset * pe_stride);
            tmp = pe_cnt - nxl_offset;
            if ( tmp <= 0 || (tmp >= radix && child_start >= nxl_pe_end) )
                return 0; // LEAF node.
            tmp = (tmp > radix ? radix : tmp);
            /* record child pe #'s for caller */
            for( j=0; j < tmp; j++ ) {
                child = child_start;
                if (root_PE != start_pe) {
                    if (child == root_PE)
                        child = start_pe;
                    else if (child == start_pe)
                            child = root_PE;
                }
                children[j] = child;
                child_start += pe_stride;
            }
            return 0;
        }
        /* advance to next tree level */
        pe_cnt -= lvl_cnt;
        pe = pe_end + pe_stride;
        lvl_cnt *= radix;   // # of pes at next tree level
    }
    return -1;  /* unable to find PE ? */
}

int shmem_tree_threshold=8; // env 'SHMEM_TREE_THRESHOLD' runtime.c
int shmem_tree_radix=3; // env 'SHMEM_TREE_RADIX'

static inline
void
shmem_internal_bcast(void *target, const void *source, size_t len,
                     int PE_root, int PE_start, int logPE_stride, int PE_size,
                     long *pSync)
{
    int stride = (logPE_stride == 0) ? 1 : 1 << logPE_stride;
    int i, pe, ret = 0;
    long one = 1;
    int real_root = PE_start + PE_root * stride;
    int *kids;

    if ( PE_size < shmem_tree_threshold ) {
        if (real_root == shmem_internal_my_pe) {
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
                ret += shmem_internal_put(pSync, &one, sizeof(long), pe);
            }
            shmem_internal_put_wait(ret);
        }

        /* wait for data arrival message */
        shmem_long_wait(pSync, 0);
        /* Clear pSync ; never atomically incremented, ok to direct assign */
        pSync[0] = 0;
        return;
    }

    /* Cascade broadcast */
    kids = alloca( shmem_tree_radix * sizeof(int) );
    if ( !kids )
        RAISE_ERROR_STR("alloca() failed?");

    ret = find_my_children( shmem_internal_my_pe, PE_start, PE_size, stride,
                            shmem_tree_radix, real_root, kids );
    if ( ret )
        RAISE_ERROR_STR("Unable to find broadcast children?");

    if (real_root == shmem_internal_my_pe) {

        /* real_root starts the data cascade: Tx BC data to it's children */
        for (i = 0,ret = 0; i < shmem_tree_radix && kids[i] != -1; i++)
            ret += shmem_internal_put(target, source, len, kids[i]);

        shmem_internal_put_wait(ret);
        shmem_fence();

        /* send completion ack to my children */
        for (i = 0,ret = 0; i < shmem_tree_radix && kids[i] != -1; i++)
            ret += shmem_internal_put(pSync, &one, sizeof(long), kids[i]);

        shmem_internal_put_wait(ret);

        if ( target != source )
            memcpy(target, source, len);
    }
    else {
        /* non root_PE - wait for data arrival message */
        shmem_long_wait(pSync, 0);

        /* cascade broadcast data to my children */
        for (i = 0,ret = 0; i < shmem_tree_radix && kids[i] != -1; i++)
            ret += shmem_internal_put(target, target, len, kids[i]);

        if ( ret ) {
            shmem_internal_put_wait(ret);
            shmem_fence();

            /* send completion ack to my children */
            for (i = 0,ret = 0; i < shmem_tree_radix && kids[i] != -1; i++)
                ret += shmem_internal_put(pSync, &one, sizeof(long), kids[i]);

            shmem_internal_put_wait(ret); 
        }
    }
    /* Clear pSync ; never atomically incremented, ok to direct assign */
    pSync[0] = _SHMEM_SYNC_VALUE;
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
    shmem_internal_bcast(target, target, count * type_size, 0, PE_start, logPE_stride, PE_size, pSync);
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
    shmem_internal_bcast(target, target, bcast_len, 0, PE_start, logPE_stride, PE_size, pSync);
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
    
    shmem_internal_bcast(target, target, len * PE_size, 0, PE_start, logPE_stride, PE_size, pSync);
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
                    pSync);
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
                    pSync);
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

