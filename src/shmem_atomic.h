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

#ifndef SHMEM_ATOMIC_H
#define SHMEM_ATOMIC_H

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
    shmem_internal_assertp(lock->enter == lock->exit);
}

/* Atomics */
#  ifdef ENABLE_THREADS

#    if (defined(__STDC_NO_ATOMICS__) || !defined(HAVE_STD_ATOMICS_HEADER))

typedef uint64_t shmem_atomic_uint64_t;

static inline
void
shmem_internal_atomic_init(shmem_atomic_uint64_t *val) {
    __sync_lock_test_and_set(val, 0);
    return;
}

static inline
shmem_atomic_uint64_t
shmem_internal_atomic_read(shmem_atomic_uint64_t *val) {
    return __sync_fetch_and_add(val, 0);
}

static inline
void
shmem_internal_atomic_inc(shmem_atomic_uint64_t *val) {
    __sync_fetch_and_add(val, 1);
    return;
}

static inline
void
shmem_internal_atomic_dec(shmem_atomic_uint64_t *val) {
    __sync_fetch_and_sub(val, 1);
    return;
}

#    else

#include <stdatomic.h>

typedef _Atomic uint64_t shmem_atomic_uint64_t;

static inline
void
shmem_internal_atomic_init(shmem_atomic_uint64_t *val) {
    atomic_store(val, 0);
    return;
}

static inline
shmem_atomic_uint64_t
shmem_internal_atomic_read(shmem_atomic_uint64_t *val) {
    return atomic_load(val);
}

static inline
void
shmem_internal_atomic_inc(shmem_atomic_uint64_t *val) {
    atomic_fetch_add(val, 1);
    return;
}

static inline
void
shmem_internal_atomic_dec(shmem_atomic_uint64_t *val) {
    atomic_fetch_sub(val, 1);
    return;
}
#    endif

#  else /* !define( ENABLE_THREADS ) */

typedef uint64_t shmem_atomic_uint64_t;

static inline
void
shmem_internal_atomic_init(shmem_atomic_uint64_t *val) {
    *val = 0;
    return;
}

static inline
shmem_atomic_uint64_t
shmem_internal_atomic_read(shmem_atomic_uint64_t *val) {
    return *val;
}

static inline
void
shmem_internal_atomic_inc(shmem_atomic_uint64_t *val) {
    *val = *val+1;
    return;
}

static inline
void
shmem_internal_atomic_dec(shmem_atomic_uint64_t *val) {
    *val = *val-1;
    return;
}
#  endif /* ENABLE_THREADS */

#endif
