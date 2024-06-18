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

#ifndef SHMEM_LOCK_H
#define SHMEM_LOCK_H

#include "shmem_comm.h"
#include "shmem_synchronization.h"
#include "shmem_atomic.h"


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
shmem_internal_clear_lock(long *lockp, size_t nic_idx)
{
    lock_t *lock = (lock_t*) lockp;
    int curr, cond, zero = 0, sig = SIGNAL_MASK;

    shmem_internal_quiet(SHMEM_CTX_DEFAULT);

    /* release the lock if I'm the last to try to obtain it */
    cond = shmem_internal_my_pe + 1;
    shmem_internal_cswap(SHMEM_CTX_DEFAULT, &(lock->last), &zero, &curr, &cond,
                         sizeof(int), 0, SHM_INTERNAL_INT, nic_idx); // Multiplex across NICs?
    shmem_internal_get_wait(SHMEM_CTX_DEFAULT, nic_idx); // Multiplex across NICs?

    /* if local PE was not the last to hold the lock, look for the next in line */
    if (curr != shmem_internal_my_pe + 1) {
        int cur_data;

        /* wait for next part of the data block to be non-zero */
        for (;;) {
            shmem_internal_atomic_fetch(SHMEM_CTX_DEFAULT, &cur_data, &(lock->data),
                                        sizeof(int), shmem_internal_my_pe,
                                        SHM_INTERNAL_INT, nic_idx); // Multiplex across NICs?
            shmem_internal_get_wait(SHMEM_CTX_DEFAULT, nic_idx); // Multiplex across NICs?

            if (NEXT(cur_data) != 0)
                break;

            SHMEM_WAIT(&(lock->data), cur_data);
        }

        /* set the signal bit on new lock holder */
        shmem_internal_mswap(SHMEM_CTX_DEFAULT, &(lock->data), &sig, &curr,
                             &sig, sizeof(int), NEXT(cur_data) - 1, SHM_INTERNAL_INT, nic_idx);// Multiplex across NICs?
        shmem_internal_get_wait(SHMEM_CTX_DEFAULT, nic_idx); // Multiplex across NICs?
    }
}


static inline void
shmem_internal_set_lock(long *lockp, size_t nic_idx)
{
    lock_t *lock = (lock_t*) lockp;
    int curr, zero = 0, me = shmem_internal_my_pe + 1;

    /* initialize my elements to zero */
    shmem_internal_atomic_set(SHMEM_CTX_DEFAULT, &(lock->data), &zero,
                              sizeof(zero), shmem_internal_my_pe, SHM_INTERNAL_INT, nic_idx);
    shmem_internal_quiet(SHMEM_CTX_DEFAULT);

    /* update last with my value to add me to the queue */
    shmem_internal_swap(SHMEM_CTX_DEFAULT, &(lock->last), &me, &curr,
                        sizeof(int), 0, SHM_INTERNAL_INT, 0); // Multiplex across NICs?
    shmem_internal_get_wait(SHMEM_CTX_DEFAULT, 0); // Multiplex across NICs?

    /* If I wasn't the first, need to add myself to the previous last's next */
    if (0 != curr) {
        int next_mask = NEXT_MASK;

        shmem_internal_mswap(SHMEM_CTX_DEFAULT, &(lock->data), &me, &curr,
                             &next_mask, sizeof(int), curr - 1, SHM_INTERNAL_INT, nic_idx); // Multiplex across NICs?
        shmem_internal_get_wait(SHMEM_CTX_DEFAULT, nic_idx); // Multiplex across NICs?

        /* now wait for the signal part of data to be non-zero */
        for (;;) {
            int cur_data;

            shmem_internal_atomic_fetch(SHMEM_CTX_DEFAULT, &cur_data, &(lock->data),
                                        sizeof(int), shmem_internal_my_pe, SHM_INTERNAL_INT, nic_idx); // Multiplex across NICs?
            shmem_internal_get_wait(SHMEM_CTX_DEFAULT, nic_idx); // Multiplex across NICs?

            if (SIGNAL(cur_data) != 0)
                break;

            SHMEM_WAIT(&(lock->data), cur_data);
        }
    }

    shmem_internal_membar_acquire();
    /* Transport level memory flush is required to make memory changes (i.e.
     * operations performed within a previous critical section) visible */
    shmem_transport_syncmem();
}


static inline int
shmem_internal_test_lock(long *lockp, size_t nic_idx)
{
    lock_t *lock = (lock_t*) lockp;
    int curr, me = shmem_internal_my_pe + 1, zero = 0;

    /* initialize my elements to zero */
    shmem_internal_atomic_set(SHMEM_CTX_DEFAULT, &(lock->data), &zero,
                              sizeof(zero), shmem_internal_my_pe, SHM_INTERNAL_INT, nic_idx);
    shmem_internal_quiet(SHMEM_CTX_DEFAULT);

    /* add self to last if and only if the lock is zero (ie, no one has the lock) */
    shmem_internal_cswap(SHMEM_CTX_DEFAULT, &(lock->last), &me, &curr, &zero,
                         sizeof(int), 0, SHM_INTERNAL_INT, nic_idx); // Multiplex across NICs?
    shmem_internal_get_wait(SHMEM_CTX_DEFAULT, nic_idx); // Multiplex across NICs?

    if (0 == curr) {
        shmem_internal_membar_acquire();
        /* Transport level memory flush is required to make memory changes
         * (i.e. operations performed within a previous critical section) visible */
        shmem_transport_syncmem();
        return 0;
    }

    return 1;
}


#endif /* #ifndef SHMEM_LOCK_H */
