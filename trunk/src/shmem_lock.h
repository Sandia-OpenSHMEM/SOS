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

#ifndef SHMEM_LOCK_H
#define SHMEM_LOCK_H

#include "shmem_comm.h"
#include "shmem_synchronization.h"


/*
 * Use basic MCS distributed lock algorithm for lock
 */
struct lock_t {
    int last; /* has meaning only on PE 0 */
    int data; /* has meaning on all PEs */
};
typedef struct lock_t lock_t;

#define NEXT_MASK   0x7FFFFFFFU
#define SIGNAL_MASK 0x80000000U
#define NEXT(A)   (A & NEXT_MASK)
#define SIGNAL(A) (A & SIGNAL_MASK)


static inline void
shmem_internal_clear_lock(long *lockp)
{
    lock_t *lock = (lock_t*) lockp;
    int curr, cond, zero = 0, sig = SIGNAL_MASK;

    shmem_internal_quiet();

    /* release the lock if I'm the last to try to obtain it */
    cond = shmem_internal_my_pe + 1;
    shmem_internal_cswap(&(lock->last), &zero, &curr, &cond, sizeof(int), 0, DTYPE_INT);
    shmem_internal_get_wait();
    /* if local PE was not the last to hold the lock, have to look for the next in line */
    if (curr != shmem_internal_my_pe + 1) {
        /* wait for next part of the data block to be non-zero */
        while (NEXT(lock->data) == 0) {
            shmem_int_wait(&(lock->data), SIGNAL(lock->data));
        }
        /* set the signal bit on new lock holder */
        shmem_internal_mswap(&(lock->data), &sig, &curr, &sig, sizeof(int), NEXT(lock->data) - 1, DTYPE_INT);
        shmem_internal_get_wait();
    }
}


static inline void
shmem_internal_set_lock(long *lockp)
{
    lock_t *lock = (lock_t*) lockp;
    int curr, zero = 0, me = shmem_internal_my_pe + 1, next_mask = NEXT_MASK;

    /* initialize my elements to zero */
    shmem_internal_put_small(&(lock->data), &zero, sizeof(zero), shmem_internal_my_pe);
    shmem_internal_quiet();

    /* update last with my value to add me to the queue */
    shmem_internal_swap(&(lock->last), &me, &curr, sizeof(int), 0, DTYPE_INT);
    shmem_internal_get_wait();
    /* If I wasn't the first, need to add myself to the previous last's next */
    if (0 != curr) {
        shmem_internal_mswap(&(lock->data), &me, &curr, &next_mask, sizeof(int), curr - 1, DTYPE_INT);
        shmem_internal_get_wait();
        /* now wait for the signal part of data to be non-zero */
        while (SIGNAL(lock->data) == 0) {
            shmem_int_wait(&(lock->data), NEXT(lock->data));
        }
    }
}


static inline int
shmem_internal_test_lock(long *lockp)
{
    lock_t *lock = (lock_t*) lockp;
    int curr, me = shmem_internal_my_pe + 1, zero = 0;

    /* initialize my elements to zero */
    shmem_internal_put_small(&(lock->data), &zero, sizeof(zero), shmem_internal_my_pe);
    shmem_internal_quiet();

    /* add self to last if and only if the lock is zero (ie, no one has the lock) */
    shmem_internal_cswap(&(lock->last), &me, &curr, &zero, sizeof(int), 0, DTYPE_INT);
    shmem_internal_get_wait();
    if (0 == curr) {
        return 0;
    }
    return 1;
}


#endif /* #ifndef SHMEM_LOCK_H */
