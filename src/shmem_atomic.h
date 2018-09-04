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

/* Internal variable defined to identify whether a memory barrier is needed */

#if defined(USE_XPMEM)
# define SHMEM_INTERNAL_NEED_MEMBAR 1
#elif defined(ENABLE_THREADS)
# define SHMEM_INTERNAL_NEED_MEMBAR (shmem_internal_thread_level != SHMEM_THREAD_SINGLE)
#else
# define SHMEM_INTERNAL_NEED_MEMBAR 0
#endif

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


#if (defined(__STDC_NO_ATOMICS__) || !defined(HAVE_STDATOMIC_H))

static inline
void
shmem_internal_membar(void) {
    if (SHMEM_INTERNAL_NEED_MEMBAR)
        __sync_synchronize();
    return;
}

static inline
void
shmem_internal_membar_load(void) {
#if defined(__i386__) || defined(__x86_64__)
    if (SHMEM_INTERNAL_NEED_MEMBAR) 
        __asm__ __volatile__ ("lfence" ::: "memory"); 
#else
    if (SHMEM_INTERNAL_NEED_MEMBAR) 
        __sync_synchronize();
#endif
    return;
}

static inline
void
shmem_internal_membar_store(void) {
#if defined(__i386__) || defined(__x86_64__)
    if (SHMEM_INTERNAL_NEED_MEMBAR) 
        __asm__ __volatile__ ("sfence" ::: "memory");
#else
    if (SHMEM_INTERNAL_NEED_MEMBAR) 
        __sync_synchronize();
#endif
    return;
}

#else
#include <stdatomic.h>

static inline
void
shmem_internal_membar(void) {
    if (SHMEM_INTERNAL_NEED_MEMBAR) 
        atomic_thread_fence(memory_order_seq_cst);
    return;
}

static inline
void
shmem_internal_membar_load(void) {
    if (SHMEM_INTERNAL_NEED_MEMBAR) 
        atomic_thread_fence(memory_order_acquire);
    return;
}

static inline
void
shmem_internal_membar_store(void) {
    if (SHMEM_INTERNAL_NEED_MEMBAR) 
        atomic_thread_fence(memory_order_release);
    return;
}

#endif

/* Atomics */
#  ifdef ENABLE_THREADS

#    if (defined(__STDC_NO_ATOMICS__) || !defined(HAVE_STDATOMIC_H))

#include <stdint.h>

typedef uint64_t shmem_internal_atomic_uint64_t;

static inline
void
shmem_internal_atomic_write(shmem_internal_atomic_uint64_t *ptr, uint64_t value) {
    __sync_lock_test_and_set(ptr, value);
    return;
}

static inline
uint64_t
shmem_internal_atomic_read(shmem_internal_atomic_uint64_t *val) {
    return __sync_fetch_and_add(val, 0);
}

static inline
void
shmem_internal_atomic_inc(shmem_internal_atomic_uint64_t *val) {
    __sync_fetch_and_add(val, 1);
    return;
}

static inline
void
shmem_internal_atomic_dec(shmem_internal_atomic_uint64_t *val) {
    __sync_fetch_and_sub(val, 1);
    return;
}

#    else

#include <stdatomic.h>

typedef atomic_uint_fast64_t shmem_internal_atomic_uint64_t;

static inline
void
shmem_internal_atomic_write(shmem_internal_atomic_uint64_t *ptr, uint64_t value) {
    atomic_store(ptr, value);
    return;
}

static inline
uint64_t
shmem_internal_atomic_read(shmem_internal_atomic_uint64_t *val) {
    return (uint64_t)atomic_load(val);
}

static inline
void
shmem_internal_atomic_inc(shmem_internal_atomic_uint64_t *val) {
    atomic_fetch_add(val, 1);
    return;
}

static inline
void
shmem_internal_atomic_dec(shmem_internal_atomic_uint64_t *val) {
    atomic_fetch_sub(val, 1);
    return;
}
#    endif

#  else /* !define( ENABLE_THREADS ) */

typedef uint64_t shmem_internal_atomic_uint64_t;

static inline
void
shmem_internal_atomic_write(shmem_internal_atomic_uint64_t *ptr, uint64_t value) {
    *ptr = value;
    return;
}

static inline
uint64_t
shmem_internal_atomic_read(shmem_internal_atomic_uint64_t *val) {
    return *val;
}

static inline
void
shmem_internal_atomic_inc(shmem_internal_atomic_uint64_t *val) {
    *val = *val+1;
    return;
}

static inline
void
shmem_internal_atomic_dec(shmem_internal_atomic_uint64_t *val) {
    *val = *val-1;
    return;
}
#  endif /* ENABLE_THREADS */

#endif
