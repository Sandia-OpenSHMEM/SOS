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

#ifndef PORTALS_SHMEM_INTERNAL_H
#define PORTALS_SHMEM_INTERNAL_H

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmemx.h"
#include "runtime.h"
#include "config.h"

extern int shmem_internal_my_pe;
extern int shmem_internal_num_pes;

extern int shmem_internal_initialized;
extern int shmem_internal_finalized;
extern int shmem_internal_thread_level;
extern int shmem_internal_debug;
extern int shmem_internal_trap_on_abort;

extern void *shmem_internal_heap_base;
extern long shmem_internal_heap_length;
extern void *shmem_internal_data_base;
extern long shmem_internal_data_length;
extern int shmem_internal_heap_use_huge_pages;
extern long shmem_internal_heap_huge_page_size;

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


#define RAISE_ERROR_MSG(...)                                            \
    do {                                                                \
        char str[256];                                                  \
        size_t off;                                                     \
        off = snprintf(str, sizeof(str), "[%03d] ERROR: %s:%d:\n",      \
                       shmem_internal_my_pe, __FILE__, __LINE__);       \
        off+= snprintf(str+off, sizeof(str)-off, "[%03d]        ",      \
                       shmem_internal_my_pe);                           \
        off+= snprintf(str+off, sizeof(str)-off, __VA_ARGS__);          \
        fprintf(stderr, "%s", str);                                     \
        shmem_runtime_abort(1, PACKAGE_NAME " exited in error");        \
    } while (0)


#define RAISE_WARN_MSG(...)                                             \
    do {                                                                \
        char str[256];                                                  \
        size_t off;                                                     \
        off = snprintf(str, sizeof(str), "[%03d] WARN: %s:%d:\n",       \
                       shmem_internal_my_pe, __FILE__, __LINE__);       \
        off+= snprintf(str+off, sizeof(str)-off, "[%03d]       ",       \
                       shmem_internal_my_pe);                           \
        off+= snprintf(str+off, sizeof(str)-off, __VA_ARGS__);          \
        fprintf(stderr, "%s", str);                                     \
    } while (0)


#define DEBUG_STR(str)                                                  \
    do {                                                                \
        if(shmem_internal_debug) {                                      \
            fprintf(stderr, "[%03d] DEBUG: %s:%d: %s\n",                \
                    shmem_internal_my_pe, __FILE__, __LINE__, str);     \
        }                                                               \
    } while(0)

#define DEBUG_MSG(...)                                                  \
    do {                                                                \
        if(shmem_internal_debug) {                                      \
            char str[256];                                              \
            size_t off;                                                 \
            off = snprintf(str, sizeof(str), "[%03d] DEBUG: %s:%d: %s\n", \
                           shmem_internal_my_pe, __FILE__, __LINE__,    \
                           __func__);                                   \
            off+= snprintf(str+off, sizeof(str)-off, "[%03d]        ",  \
                           shmem_internal_my_pe);                       \
            off+= snprintf(str+off, sizeof(str)-off, __VA_ARGS__);      \
            fprintf(stderr, "%s", str);                                 \
        }                                                               \
    } while(0)

#ifdef ENABLE_ERROR_CHECKING
#define SHMEM_ERR_CHECK_INITIALIZED()                                   \
    do {                                                                \
        if (!shmem_internal_initialized) {                              \
            fprintf(stderr, "ERROR: %s(): " PACKAGE_NAME " library not initialized\n", \
                    __func__);                                          \
            abort();                                                    \
        }                                                               \
    } while (0)

#define SHMEM_ERR_CHECK_POSITIVE(arg)                                   \
    do {                                                                \
        if ((arg) <= 0) {                                               \
            fprintf(stderr, "ERROR: %s(): Argument %s must be positive (%ld)\n", \
                    __func__, #arg, (long) arg);                        \
            shmem_runtime_abort(100, PACKAGE_NAME " exited in error");  \
        }                                                               \
    } while (0)

#define SHMEM_ERR_CHECK_NON_NEGATIVE(arg)                               \
    do {                                                                \
        if ((arg) < 0) {                                                \
            fprintf(stderr, "ERROR: %s(): Argument %s must be greater or equal to zero (%ld)\n", \
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

#define SHMEM_ERR_CHECK_PE(pe)                                          \
    do {                                                                \
        if ((pe) < 0 || (pe) >= shmem_internal_num_pes) {               \
            fprintf(stderr, "ERROR: %s(): PE argument (%d) is invalid\n", \
                    __func__, (pe));                                    \
            shmem_runtime_abort(100, PACKAGE_NAME " exited in error");  \
        }                                                               \
    } while (0)

#define SHMEM_ERR_CHECK_SYMMETRIC(ptr_in, len)                                          \
    do {                                                                                \
        const void *ptr_base = (void*)(ptr_in);                                         \
        const void *ptr_ext  = (void*)((uint8_t *) (ptr_base) + (len));                 \
        const void *data_ext = (void*)((uint8_t *) shmem_internal_data_base +           \
                                                   shmem_internal_data_length);         \
        const void *heap_ext = (void*)((uint8_t *) shmem_internal_heap_base +           \
                                                   shmem_internal_heap_length);         \
        if (len == 0) {                                                                 \
            break; /* Skip this check when the length is 0 */                           \
        }                                                                               \
        else if (ptr_base >= shmem_internal_data_base && ptr_base < data_ext) {         \
            if (ptr_ext > data_ext) {                                                   \
                fprintf(stderr, "ERROR: %s(): Argument \"%s\" [%p..%p) exceeds "        \
                                "sym. data region [%p..%p)\n",                          \
                        __func__, #ptr_in, ptr_base, ptr_ext,                           \
                        shmem_internal_data_base, data_ext);                            \
                shmem_runtime_abort(100, PACKAGE_NAME " exited in error");              \
            }                                                                           \
        }                                                                               \
        else if (ptr_base >= shmem_internal_heap_base && ptr_base < heap_ext) {         \
            if (ptr_ext > heap_ext) {                                                   \
                fprintf(stderr, "ERROR: %s(): Argument \"%s\" [%p..%p) exceeds "        \
                                "sym. heap region [%p..%p)\n",                          \
                        __func__, #ptr_in, ptr_base, ptr_ext,                           \
                        shmem_internal_heap_base, heap_ext);                            \
                shmem_runtime_abort(100, PACKAGE_NAME " exited in error");              \
            }                                                                           \
        }                                                                               \
        else {                                                                          \
            fprintf(stderr, "ERROR: %s(): Argument \"%s\" is not symmetric (%p)\n",     \
                    __func__, #ptr_in, ptr_base);                                       \
            shmem_runtime_abort(100, PACKAGE_NAME " exited in error");                  \
        }                                                                               \
    } while (0)

#define SHMEM_ERR_CHECK_SYMMETRIC_HEAP(ptr_in)                                          \
    do {                                                                                \
        const void *ptr_base      = (void*)(ptr_in);                                    \
        const void *heap_ext = (void*)((uint8_t *) shmem_internal_heap_base +           \
                                                   shmem_internal_heap_length);         \
        if (! (ptr_base >= shmem_internal_heap_base && ptr_base < heap_ext)) {          \
            fprintf(stderr, "ERROR: %s(): Argument \"%s\" is not in symm. heap (%p), "  \
                            "[%p..%p)\n",                                               \
                    __func__, #ptr_in, ptr_base, shmem_internal_heap_base, heap_ext);   \
            shmem_runtime_abort(100, PACKAGE_NAME " exited in error");                  \
        }                                                                               \
    } while (0)

#define SHMEM_ERR_CHECK_NULL(ptr, nelems)                                               \
    do {                                                                                \
        if (nelems > 0 && (ptr) == NULL) {                                              \
                fprintf(stderr, "ERROR: %s(): Argument \"%s\" is NULL\n",               \
                        __func__, #ptr);                                                \
                shmem_runtime_abort(100, PACKAGE_NAME " exited in error");              \
        }                                                                               \
    } while(0)

#define SHMEM_ERR_CHECK_CMP_OP(op)                                                      \
    do {                                                                                \
        switch(op) {                                                                    \
            case SHMEM_CMP_EQ:                                                          \
            case SHMEM_CMP_NE:                                                          \
            case SHMEM_CMP_GT:                                                          \
            case SHMEM_CMP_GE:                                                          \
            case SHMEM_CMP_LT:                                                          \
            case SHMEM_CMP_LE:                                                          \
                break;                                                                  \
            default:                                                                    \
                fprintf(stderr, "ERROR: %s(): Argument \"%s\", "                        \
                                "invalid comparison operation (%d)\n",                  \
                        __func__, #op, (int) (op));                                     \
                shmem_runtime_abort(100, PACKAGE_NAME " exited in error");              \
        }                                                                               \
    } while (0)

#else
#define SHMEM_ERR_CHECK_INITIALIZED()
#define SHMEM_ERR_CHECK_POSITIVE(arg)
#define SHMEM_ERR_CHECK_NON_NEGATIVE(arg)
#define SHMEM_ERR_CHECK_ACTIVE_SET(PE_start, logPE_stride, PE_size)
#define SHMEM_ERR_CHECK_PE(pe)
#define SHMEM_ERR_CHECK_SYMMETRIC(ptr, len)
#define SHMEM_ERR_CHECK_SYMMETRIC_HEAP(ptr)
#define SHMEM_ERR_CHECK_NULL(ptr, nelems)
#define SHMEM_ERR_CHECK_CMP_OP(op)

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
        if (shmem_internal_thread_level != SHMEMX_THREAD_SINGLE)          \
            pthread_mutex_init(&_mutex, NULL); \
    } while (0)
#   define SHMEM_MUTEX_DESTROY(_mutex)                                  \
    do {                                                                \
        if (shmem_internal_thread_level != SHMEMX_THREAD_SINGLE)          \
            pthread_mutex_destroy(&_mutex);                             \
    } while (0)
#   define SHMEM_MUTEX_LOCK(_mutex)                                     \
    do {                                                                \
        if (shmem_internal_thread_level != SHMEMX_THREAD_SINGLE)          \
            pthread_mutex_lock(&_mutex);                                \
    } while (0)
#   define SHMEM_MUTEX_UNLOCK(_mutex)                                   \
    do {                                                                \
        if (shmem_internal_thread_level != SHMEMX_THREAD_SINGLE)          \
            pthread_mutex_unlock(&_mutex);                              \
    } while (0)

#   else  /* !ENABLE_PTHREAD_MUTEX */
#include <shmem_atomic.h>
typedef shmem_spinlock_t shmem_internal_mutex_t;

#   define SHMEM_MUTEX_INIT(_mutex)                                     \
    do {                                                                \
        if (shmem_internal_thread_level != SHMEMX_THREAD_SINGLE)          \
            shmem_spinlock_init(&_mutex);                               \
    } while (0)
#   define SHMEM_MUTEX_DESTROY(_mutex)                                  \
    do {                                                                \
        if (shmem_internal_thread_level != SHMEMX_THREAD_SINGLE)          \
            shmem_spinlock_fini(&_mutex);                               \
    } while (0)
#   define SHMEM_MUTEX_LOCK(_mutex)                                     \
    do {                                                                \
        if (shmem_internal_thread_level != SHMEMX_THREAD_SINGLE)          \
            shmem_spinlock_lock(&_mutex);                               \
    } while (0)
#   define SHMEM_MUTEX_UNLOCK(_mutex)                                   \
    do {                                                                \
        if (shmem_internal_thread_level != SHMEMX_THREAD_SINGLE)          \
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

static inline double shmem_internal_wtime(void) {
    double wtime = 0.0;

#ifdef HAVE_CLOCK_GETTIME
    struct timespec tv;
    clock_gettime(CLOCK_MONOTONIC, &tv);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_nsec / 1.0e9;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1.0e6;
#endif
    return wtime;
}

/* Utility functions */
long shmem_util_getenv_long(const char* name, int is_sized, long default_value);
char *shmem_util_getenv_str(const char* name);

#ifndef MAX
#define MAX(A,B) (A) > (B) ? (A) : (B)
#endif

#endif
