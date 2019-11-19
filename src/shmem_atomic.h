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

#ifndef SHMEM_ATOMIC_H
#define SHMEM_ATOMIC_H

/* Compiler Barriers and stuff */

#if defined(__i386__) || defined(__x86_64__)
# define SPINLOCK_BODY() do { __asm__ __volatile__ ("pause" ::: "memory"); } while (0)
#elif defined(__aarch64__)
# define SPINLOCK_BODY() do { __asm__ __volatile__ ("dsb ld" ::: "memory"); } while (0)
#else
# define SPINLOCK_BODY() do { __asm__ __volatile__ ("" ::: "memory"); } while (0)
#endif

#define COMPILER_FENCE() do { __asm__ __volatile__ ("" ::: "memory"); } while (0)

#define likely(x)       __builtin_expect((x),1)
#define unlikely(x)     __builtin_expect((x),0)

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"

/* Internal flag to identify whether a memory barrier is needed */

#if defined(USE_XPMEM)
# define SHMEM_INTERNAL_NEED_MEMBAR 1
#elif defined(ENABLE_THREADS)
# define SHMEM_INTERNAL_NEED_MEMBAR (shmem_internal_thread_level != SHMEM_THREAD_SINGLE)
#else
# define SHMEM_INTERNAL_NEED_MEMBAR 0
#endif

/* Spinlocks */

struct shmem_spinlock_t {
    unsigned long enter;
    unsigned long exit;
};
typedef struct shmem_spinlock_t shmem_spinlock_t;


static inline
void
shmem_spinlock_init(shmem_spinlock_t *lock)
{
    __atomic_store_n(&lock->exit, 0, __ATOMIC_RELEASE);
    __atomic_store_n(&lock->enter, 0, __ATOMIC_RELEASE);
}


static inline
void
shmem_spinlock_lock(shmem_spinlock_t *lock)
{
    long val = __atomic_fetch_add(&lock->enter, 1, __ATOMIC_ACQ_REL);
    while (val != __atomic_load_n(&lock->exit, __ATOMIC_ACQUIRE)) {
        SPINLOCK_BODY();
    }
}


static inline
void
shmem_spinlock_unlock(shmem_spinlock_t *lock)
{
    __atomic_fetch_add(&lock->exit, 1, __ATOMIC_RELEASE);
}


static inline
void
shmem_spinlock_fini(shmem_spinlock_t *lock)
{
    shmem_internal_assertp(__atomic_load_n(&lock->enter, __ATOMIC_ACQUIRE) ==
                           __atomic_load_n(&lock->exit, __ATOMIC_ACQUIRE));
}


/* The full memory barrier is used in cases where global ordering is required,
 * and thus requires sequential consistency.  For example, PE 0 performs
 * updates followed by a quiet.  PE 1 observes PE 0's updates and informs PE 2
 * that PE 0's updates are available.  PE 2 must also see PE 0's updates.
 */
static inline
void
shmem_internal_membar(void) {
    if (SHMEM_INTERNAL_NEED_MEMBAR)
        __atomic_thread_fence(__ATOMIC_SEQ_CST);
    return;
}

static inline
void
shmem_internal_membar_release(void) {
    if (SHMEM_INTERNAL_NEED_MEMBAR)
        __atomic_thread_fence(__ATOMIC_RELEASE);
    return;
}

static inline
void
shmem_internal_membar_acquire(void) {
    if (SHMEM_INTERNAL_NEED_MEMBAR)
        __atomic_thread_fence(__ATOMIC_ACQUIRE);
    return;
}


static inline
void
shmem_internal_membar_acq_rel(void) {
    if (SHMEM_INTERNAL_NEED_MEMBAR)
        __atomic_thread_fence(__ATOMIC_ACQ_REL);
    return;
}


/* Atomics */
#  ifdef ENABLE_THREADS
#    if (defined(__STDC_NO_ATOMICS__) || !defined(HAVE_STDATOMIC_H))

#include <stdint.h>

typedef uint64_t shmem_internal_cntr_t;

static inline
void
shmem_internal_cntr_write(shmem_internal_cntr_t *ptr, uint64_t value) {
    __atomic_store_n(ptr, value, __ATOMIC_RELEASE);
    return;
}

static inline
uint64_t
shmem_internal_cntr_read(shmem_internal_cntr_t *val) {
    return __atomic_load_n(val, __ATOMIC_ACQUIRE);
}

static inline
void
shmem_internal_cntr_inc(shmem_internal_cntr_t *val) {
    __atomic_fetch_add(val, 1, __ATOMIC_RELEASE);
    return;
}

static inline
void
shmem_internal_cntr_dec(shmem_internal_cntr_t *val) {
    __atomic_fetch_sub(val, 1, __ATOMIC_RELEASE);
    return;
}

#    else /* HAVE_STDATOMIC_H */

#include <stdatomic.h>

typedef atomic_uint_fast64_t shmem_internal_cntr_t;

static inline
void
shmem_internal_cntr_write(shmem_internal_cntr_t *ptr, uint64_t value) {
    atomic_store(ptr, value);
    return;
}

static inline
uint64_t
shmem_internal_cntr_read(shmem_internal_cntr_t *val) {
    return (uint64_t)atomic_load(val);
}

static inline
void
shmem_internal_cntr_inc(shmem_internal_cntr_t *val) {
    atomic_fetch_add(val, 1);
    return;
}

static inline
void
shmem_internal_cntr_dec(shmem_internal_cntr_t *val) {
    atomic_fetch_sub(val, 1);
    return;
}

#    endif
#  else /* !define( ENABLE_THREADS ) */

typedef uint64_t shmem_internal_cntr_t;

static inline
void
shmem_internal_cntr_write(shmem_internal_cntr_t *ptr, uint64_t value) {
    *ptr = value;
    return;
}

static inline
uint64_t
shmem_internal_cntr_read(shmem_internal_cntr_t *val) {
    return *val;
}

static inline
void
shmem_internal_cntr_inc(shmem_internal_cntr_t *val) {
    *val = *val+1;
    return;
}

static inline
void
shmem_internal_cntr_dec(shmem_internal_cntr_t *val) {
    *val = *val-1;
    return;
}
#  endif /* ENABLE_THREADS */

#endif
