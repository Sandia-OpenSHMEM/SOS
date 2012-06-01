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

#ifndef SHMEM_ATOMIC_H
#define SHMEM_ATOMIC_H

#include <assert.h>

/* Compiler Barriers and stuff */

#if defined(__i386__) || defined(__x86_64__)
# define SPINLOCK_BODY() do { __asm__ __volatile__ ("pause" ::: "memory"); } while (0)
#else
# define SPINLOCK_BODY() do { __asm__ __volatile__ (::: "memory"); } while (0)
#endif

#define COMPILER_FENCE() do { __asm__ __volatile__ ("" ::: "memory"); } while (0)

#define likely(x)       __builtin_expect((x),1)
#define unlikely(x)     __builtin_expect((x),0)

/* Spinlocks */

struct shmem_spinlock_t {
    long enter;
    long exit;
};
typedef struct shmem_spinlock_t shmem_spinlock_t;


static inline
void
shmem_spinlock_init(shmem_spinlock_t *lock)
{
    lock->enter = lock->exit = 0;
    COMPILER_FENCE();
}


static inline
void
shmem_spinlock_lock(shmem_spinlock_t *lock)
{
    long val = __sync_fetch_and_add(&lock->enter, 1);
    while (val != (lock->exit)) {
        SPINLOCK_BODY();
    }
}


static inline
void
shmem_spinlock_unlock(shmem_spinlock_t *lock)
{
    COMPILER_FENCE();
    lock->exit++;
}


static inline
void
shmem_spinlock_fini(shmem_spinlock_t *lock)
{
    assert(lock->enter == lock->exit);
}

#endif
