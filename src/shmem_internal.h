/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 * 
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef PORTALS_SHMEM_INTERNAL_H
#define PORTALS_SHMEM_INTERNAL_H

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "shmemx.h"
#include "runtime.h"
#include "config.h"

extern int shmem_internal_my_pe;
extern int shmem_internal_num_pes;

extern int shmem_internal_initialized;
extern int shmem_internal_finalized;
extern int shmem_internal_thread_level;
extern int shmem_internal_debug;

#define RAISE_WARN(ret)                                                 \
    do {                                                                \
        fprintf(stderr, "[%03d] WARN: %s:%d return code %d\n",         \
                shmem_internal_my_pe, __FILE__, __LINE__, (int) ret);   \
    } while (0)


#define RAISE_ERROR(ret)                                                \
    do {                                                                \
        fprintf(stderr, "[%03d] ERROR: %s:%d return code %d\n",         \
                shmem_internal_my_pe, __FILE__, __LINE__, (int) ret);   \
        shmem_runtime_abort(1, PACKAGE_NAME " exited in error");        \
    } while (0)


#define RAISE_ERROR_STR(str)                                            \
    do {                                                                \
        fprintf(stderr, "[%03d] ERROR: %s:%d: %s\n",                    \
                shmem_internal_my_pe, __FILE__, __LINE__, str);         \
        shmem_runtime_abort(1, PACKAGE_NAME " exited in error");        \
    } while (0)


#define RAISE_WARN_STR(str)                                             \
    do {                                                                \
        fprintf(stderr, "[%03d] WARN: %s:%d: %s\n",                    \
                shmem_internal_my_pe, __FILE__, __LINE__, str);         \
    } while (0)

#ifdef ENABLE_ERROR_CHECKING
#define SHMEM_ERR_CHECK_INITIALIZED()                                   \
    do {                                                                \
        if (!shmem_internal_initialized) {                              \
            fprintf(stderr, "ERROR: %s(): " PACKAGE_NAME " library not initialized\n", \
                    __func__);                                          \
            abort();                                                    \
        }                                                               \
    } while (0)

#define SHMEM_ERR_CHECK_ARG_POSITIVE(arg)                               \
    do {                                                                \
        if ((arg) <= 0) {                                               \
            fprintf(stderr, "ERROR: %s(): Argument %s must be positive (%ld)\n", \
                    __func__, #arg, (long) arg);                        \
            shmem_runtime_abort(100, PACKAGE_NAME " exited in error");  \
        }                                                               \
    } while (0)

#define SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size)                                     \
    do {                                                                                                \
        int shmem_err_check_active_stride = 1 << logPE_stride;                                          \
        if (PE_start < 0 || logPE_stride < 0 || PE_size < 0 ||                                          \
            PE_start + (PE_size - 1) * shmem_err_check_active_stride > shmem_internal_num_pes) {        \
            fprintf(stderr, "ERROR: %s(): Invalid active set (PE_start = %d, logPE_stride = %d, PE_size = %d)\n", \
                    __func__, PE_start, logPE_stride, PE_size);                                         \
            shmem_runtime_abort(100, PACKAGE_NAME " exited in error");                                  \
        }                                                                                               \
        if (! (shmem_internal_my_pe >= PE_start &&                                                      \
               shmem_internal_my_pe <= PE_start + (PE_size-1) * shmem_err_check_active_stride &&        \
               (shmem_internal_my_pe - PE_start) % shmem_err_check_active_stride == 0)) {               \
            fprintf(stderr, "ERROR: %s(): Calling PE (%d) is not a member of the active set\n",         \
                    __func__, shmem_internal_my_pe);                                                    \
            shmem_runtime_abort(100, PACKAGE_NAME " exited in error");                                  \
        }                                                                                               \
    } while (0)


#else
#define SHMEM_ERR_CHECK_INITIALIZED()
#define SHMEM_ERR_CHECK_ARG_POSITIVE(arg)
#define SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size)

#endif /* ENABLE_ERROR_CHECKING */


/*
 * Internal Assertions
 *
 * Assertions are not compiled unless ENABLE_ERROR_CHECKING is defined.  The
 * "persistent" assertion, shmem_internal_assertp is always compiled.
 */
#define shmem_internal_assert_fail(file_, line_, cond_)                 \
    do {                                                                \
        fprintf(stderr, "[%03d] Assertion Failed: %s:%d: %s\n",         \
                shmem_internal_my_pe, file_, line_, cond_);             \
        shmem_runtime_abort(1, PACKAGE_NAME " exited in error");        \
    } while (0)

#define shmem_internal_assertp(cond)                                    \
    do {                                                                \
        if (!(cond)) {                                                  \
            shmem_internal_assert_fail(__FILE__, __LINE__, #cond);      \
        }                                                               \
    } while (0)

#ifdef ENABLE_ERROR_CHECKING
#define shmem_internal_assert(cond)                                     \
    do {                                                                \
        if (!(cond)) {                                                  \
            shmem_internal_assert_fail(__FILE__, __LINE__, #cond);      \
        }                                                               \
    } while (0)
#else
#define shmem_internal_assert(cond)
#endif


#ifdef ENABLE_THREADS

#   ifdef ENABLE_PTHREAD_MUTEX
#include <pthread.h>
typedef pthread_mutex_t shmem_internal_mutex_t;

#   define SHMEM_MUTEX_INIT(_mutex)                                     \
    do {                                                                \
        if (shmem_internal_thread_level > SHMEMX_THREAD_SINGLE)          \
            pthread_mutex_init(&_mutex, NULL); \
    } while (0)
#   define SHMEM_MUTEX_DESTROY(_mutex)                                  \
    do {                                                                \
        if (shmem_internal_thread_level > SHMEMX_THREAD_SINGLE)          \
            pthread_mutex_destroy(&_mutex);                             \
    } while (0)
#   define SHMEM_MUTEX_LOCK(_mutex)                                     \
    do {                                                                \
        if (shmem_internal_thread_level > SHMEMX_THREAD_SINGLE)          \
            pthread_mutex_lock(&_mutex);                                \
    } while (0)
#   define SHMEM_MUTEX_UNLOCK(_mutex)                                   \
    do {                                                                \
        if (shmem_internal_thread_level > SHMEMX_THREAD_SINGLE)          \
            pthread_mutex_unlock(&_mutex);                              \
    } while (0)

#   else  /* !ENABLE_PTHREAD_MUTEX */
#include <shmem_atomic.h>
typedef shmem_spinlock_t shmem_internal_mutex_t;

#   define SHMEM_MUTEX_INIT(_mutex)                                     \
    do {                                                                \
        if (shmem_internal_thread_level > SHMEMX_THREAD_SINGLE)          \
            shmem_spinlock_init(&_mutex);                               \
    } while (0)
#   define SHMEM_MUTEX_DESTROY(_mutex)                                  \
    do {                                                                \
        if (shmem_internal_thread_level > SHMEMX_THREAD_SINGLE)          \
            shmem_spinlock_fini(&_mutex);                               \
    } while (0)
#   define SHMEM_MUTEX_LOCK(_mutex)                                     \
    do {                                                                \
        if (shmem_internal_thread_level > SHMEMX_THREAD_SINGLE)          \
            shmem_spinlock_lock(&_mutex);                               \
    } while (0)
#   define SHMEM_MUTEX_UNLOCK(_mutex)                                   \
    do {                                                                \
        if (shmem_internal_thread_level > SHMEMX_THREAD_SINGLE)          \
            shmem_spinlock_unlock(&_mutex);                             \
    } while (0)

#   endif /* ENABLE_PTHREAD_MUTEX */

extern shmem_internal_mutex_t shmem_internal_mutex_alloc;

#else
#   define SHMEM_MUTEX_INIT(_mutex)
#   define SHMEM_MUTEX_DESTROY(_mutex)
#   define SHMEM_MUTEX_LOCK(_mutex)
#   define SHMEM_MUTEX_UNLOCK(_mutex)

#endif /* ENABLE_THREADS */

void shmem_internal_start_pes(int npes);
void shmem_internal_init(int tl_requested, int *tl_provided);
void shmem_internal_finalize(void);
void shmem_internal_global_exit(int status);
char *shmem_internal_nodename(void);

int shmem_internal_symmetric_init(size_t requested_length, int use_malloc);
int shmem_internal_symmetric_fini(void);
int shmem_internal_collectives_init(int requested_crossover, 
                                    int requested_radix);

/* internal allocation, without a barrier */
void *shmem_internal_shmalloc(size_t size);
void* shmem_internal_get_next(intptr_t incr);

/* Utility functions */
long shmem_util_getenv_long(const char* name, int is_sized, long default_value);
char *shmem_util_getenv_str(const char* name);

#endif
