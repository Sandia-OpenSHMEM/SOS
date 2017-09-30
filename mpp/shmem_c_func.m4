dnl Copyright (c) 2016 Intel Corporation. All rights reserved.
dnl This software is available to you under the BSD license.
dnl
dnl This file is part of the Sandia OpenSHMEM software package. For license
dnl information, see the LICENSE file in the top level directory of the
dnl distribution.
dnl

#ifndef SHMEM_FUNCTION_ATTRIBUTES
#  if SHMEM_HAVE_VISIBILITY == 1
#     define SHMEM_FUNCTION_ATTRIBUTES __attribute__((visibility("default")))
#  else
#     define SHMEM_FUNCTION_ATTRIBUTES
#  endif
#endif

/* Library Setup, Exit, and Query Routines */
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_init(void);
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_finalize(void);
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_global_exit(int status);
SHMEM_FUNCTION_ATTRIBUTES int SHPRE()shmem_n_pes(void);
SHMEM_FUNCTION_ATTRIBUTES int SHPRE()shmem_my_pe(void);

SHMEM_FUNCTION_ATTRIBUTES int SHPRE()shmem_pe_accessible(int pe);
SHMEM_FUNCTION_ATTRIBUTES int SHPRE()shmem_addr_accessible(const void *addr, int pe);
SHMEM_FUNCTION_ATTRIBUTES void *SHPRE()shmem_ptr(const void *target, int pe);

SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_info_get_version(int *major, int *minor);
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_info_get_name(char *name);

/* Memory Management Routines */
SHMEM_FUNCTION_ATTRIBUTES void *SHPRE()shmem_malloc(size_t size);
SHMEM_FUNCTION_ATTRIBUTES void *SHPRE()shmem_align(size_t alignment, size_t size);
SHMEM_FUNCTION_ATTRIBUTES void *SHPRE()shmem_realloc(void *ptr, size_t size);
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_free(void *ptr);

/* RMA: Elemental Data Put Routines */
define(`SHMEM_C_P',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_p($2 *addr, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_P')

/* RMA: Contiguous Data Put Routines */
define(`SHMEM_C_PUT',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_put($2 *target, const $2 *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_PUT')

define(`SHMEM_C_PUT_N',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_put$1(void* target, const void *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_PUT_N')
SHMEM_C_PUT_N(mem,1);

/* RMA: Strided Put Routines */
define(`SHMEM_C_IPUT',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_iput($2 *target, const $2 *source,
SH_PAD(`$1')                 ptrdiff_t tst, ptrdiff_t sst,
SH_PAD(`$1')                 size_t len, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_IPUT')

define(`SHMEM_C_IPUT_N',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_iput$1(void *target, const void *source,
SH_PAD(`$1')                ptrdiff_t tst, ptrdiff_t sst, size_t len,
SH_PAD(`$1')                int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_IPUT_N')

/* RMA: Elemental Data Get Routines */
define(`SHMEM_C_G',
`SHMEM_FUNCTION_ATTRIBUTES $2 SHPRE()shmem_$1_g(const $2 *addr, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_G')

/* RMA: Contiguous Data Get Routines */
define(`SHMEM_C_GET',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_get($2 *target, const $2 *source, size_t nelems,int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_GET')

define(`SHMEM_C_GET_N',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_get$1(void* target, const void *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_GET_N')
SHMEM_C_GET_N(mem,1);

/* RMA: Strided Get Routines */
define(`SHMEM_C_IGET',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_iget($2 *target, const $2 *source,
SH_PAD(`$1')                 ptrdiff_t tst, ptrdiff_t sst,
SH_PAD(`$1')                 size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_IGET')

define(`SHMEM_C_IGET_N',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_iget$1(void* target, const void *source,
SH_PAD(`$1')                ptrdiff_t tst, ptrdiff_t sst,
SH_PAD(`$1')                size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_IGET_N')

/* RMA: Nonblocking Contiguous Data Put Routines */
define(`SHMEM_C_PUT_NBI',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_put_nbi($2 *target, const $2 *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_PUT_NBI')

define(`SHMEM_C_PUT_N_NBI',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_put$1_nbi(void* target, const void *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_PUT_N_NBI')
SHMEM_C_PUT_N_NBI(mem,1);

/* RMA: Nonblocking Contiguous Data Get Routines */
define(`SHMEM_C_GET_NBI',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_get_nbi($2 *target, const $2 *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_GET_NBI')

define(`SHMEM_C_GET_N_NBI',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_get$1_nbi(void* target, const void *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_GET_N_NBI')
SHMEM_C_GET_N_NBI(mem,1);

/* AMO: Atomic Swap Routines */
define(`SHMEM_C_SWAP',
`SHMEM_FUNCTION_ATTRIBUTES $2 SHPRE()shmem_$1_swap($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_EXTENDED_AMO(`SHMEM_C_SWAP')

/* Special case, only enabled when C++ and C11 bindings are disabled */
#if !defined(__cplusplus) && \
    !(defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L)
long SHPRE()shmem_swap(long *target, long value, int pe);
#endif

/* AMO: Atomic Conditional Swap Routines */
define(`SHMEM_C_CSWAP',
`SHMEM_FUNCTION_ATTRIBUTES $2 SHPRE()shmem_$1_cswap($2 *target, $2 cond, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_CSWAP')

/* AMO: Atomic Fetch-and-Add Routines */
define(`SHMEM_C_FADD',
`SHMEM_FUNCTION_ATTRIBUTES $2 SHPRE()shmem_$1_fadd($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_FADD')

/* AMO: Atomic Fetch-and-Increment Routines */
define(`SHMEM_C_FINC',
`SHMEM_FUNCTION_ATTRIBUTES $2 SHPRE()shmem_$1_finc($2 *target, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_FINC')

/* AMO: Atomic Add Routines */
define(`SHMEM_C_ADD',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_add($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_ADD')

/* AMO: Atomic Increment Routines */
define(`SHMEM_C_INC',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_inc($2 *target, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_INC')

/* AMO: Atomic Fetch Routines */
define(`SHMEM_C_FETCH',
`SHMEM_FUNCTION_ATTRIBUTES $2 SHPRE()shmem_$1_fetch(const $2 *target, int pe)')dnl
SHMEM_DECLARE_FOR_EXTENDED_AMO(`SHMEM_C_FETCH')

/* AMO: Atomic Set Routines */
define(`SHMEM_C_SET',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_set($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_EXTENDED_AMO(`SHMEM_C_SET')

/* AMO: Atomic Bitwise Routines */
define(`SHMEM_C_XOR',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_atomic_xor($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_BITWISE_AMO(`SHMEM_C_XOR')

define(`SHMEM_C_AND',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_atomic_and($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_BITWISE_AMO(`SHMEM_C_AND')

define(`SHMEM_C_OR',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_atomic_or($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_BITWISE_AMO(`SHMEM_C_OR')

/* AMO: Fetching atomic Bitwise Routines */
define(`SHMEM_C_FETCH_XOR',
`SHMEM_FUNCTION_ATTRIBUTES $2 SHPRE()shmem_$1_atomic_fetch_xor($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_BITWISE_AMO(`SHMEM_C_FETCH_XOR')

define(`SHMEM_C_FETCH_AND',
`SHMEM_FUNCTION_ATTRIBUTES $2 SHPRE()shmem_$1_atomic_fetch_and($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_BITWISE_AMO(`SHMEM_C_FETCH_AND')

define(`SHMEM_C_FETCH_OR',
`SHMEM_FUNCTION_ATTRIBUTES $2 SHPRE()shmem_$1_atomic_fetch_or($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_BITWISE_AMO(`SHMEM_C_FETCH_OR')

/* COLL: Barrier Synchronization Routines */
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync);
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_barrier_all(void);

/* COLL: Reduction Routines */
define(`SHMEM_C_TO_ALL',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_$4_to_all($2 *target,
SH_PAD(`$1',`$4')                    const $2 *source, int nreduce,
SH_PAD(`$1',`$4')                    int PE_start, int logPE_stride,
SH_PAD(`$1',`$4')                    int PE_size, $2 *pWrk,
SH_PAD(`$1',`$4')                    long *pSync);')dnl
dnl
SHMEM_BIND_C_COLL_INTS(`SHMEM_C_TO_ALL', `and')

SHMEM_BIND_C_COLL_INTS(`SHMEM_C_TO_ALL', `or')

SHMEM_BIND_C_COLL_INTS(`SHMEM_C_TO_ALL', `xor')

SHMEM_BIND_C_COLL_INTS(`SHMEM_C_TO_ALL', `min')
SHMEM_BIND_C_COLL_FLOATS(`SHMEM_C_TO_ALL', `min')

SHMEM_BIND_C_COLL_INTS(`SHMEM_C_TO_ALL', `max')
SHMEM_BIND_C_COLL_FLOATS(`SHMEM_C_TO_ALL', `max')

SHMEM_BIND_C_COLL_INTS(`SHMEM_C_TO_ALL', `sum')
SHMEM_BIND_C_COLL_FLOATS(`SHMEM_C_TO_ALL', `sum')
SHMEM_BIND_C_COLL_CMPLX(`SHMEM_C_TO_ALL', `sum')

SHMEM_BIND_C_COLL_INTS(`SHMEM_C_TO_ALL', `prod')
SHMEM_BIND_C_COLL_FLOATS(`SHMEM_C_TO_ALL', `prod')
SHMEM_BIND_C_COLL_CMPLX(`SHMEM_C_TO_ALL', `prod')

/* COLL: Collect Routines */
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_collect32(void *target, const void *source, size_t nlong,
                     int PE_start, int logPE_stride, int PE_size, long *pSync);
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_collect64(void *target, const void *source, size_t nlong,
                     int PE_start, int logPE_stride, int PE_size, long *pSync);
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_fcollect32(void *target, const void *source, size_t nlong,
                      int PE_start, int logPE_stride, int PE_size,
                      long *pSync);
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_fcollect64(void *target, const void *source, size_t nlong,
                      int PE_start, int logPE_stride, int PE_size,
                      long *pSync);

/* COLL: Broadcast Routines */
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_broadcast32(void *target, const void *source, size_t nlong,
                       int PE_root, int PE_start, int logPE_stride,
                       int PE_size, long *pSync);
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_broadcast64(void *target, const void *source, size_t nlong,
                       int PE_root, int PE_start, int logPE_stride,
                       int PE_size, long *pSync);

/* COLL: All-to-All routines */
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_alltoall32(void *dest, const void *source, size_t nelems, int PE_start,
                      int logPE_stride, int PE_size, long *pSync);
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_alltoall64(void *dest, const void *source, size_t nelems, int PE_start,
                      int logPE_stride, int PE_size, long *pSync);

SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_alltoalls32(void *dest, const void *source, ptrdiff_t dst,
                       ptrdiff_t sst, size_t nelems, int PE_start,
                       int logPE_stride, int PE_size, long *pSync);
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_alltoalls64(void *dest, const void *source, ptrdiff_t dst,
                       ptrdiff_t sst, size_t nelems, int PE_start,
                       int logPE_stride, int PE_size, long *pSync);

/* Point-to-Point Synchronization Routines */
define(`SHMEM_C_WAIT',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_wait(volatile $2 *var, $2 value);')dnl
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_wait(volatile long *ivar, long cmp_value);
SHMEM_BIND_C_WAIT(`SHMEM_C_WAIT')

define(`SHMEM_C_WAIT_UNTIL',
`SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_$1_wait_until(volatile $2 *var, int cond, $2 value);')dnl
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_wait_until(volatile long *ivar, int cmp, long value);
SHMEM_BIND_C_WAIT(`SHMEM_C_WAIT_UNTIL')

/* Memory Ordering Routines */
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_quiet(void);
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_fence(void);

/* Distributed Locking Routines */
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_set_lock(volatile long *lock);
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_clear_lock(volatile long *lock);
SHMEM_FUNCTION_ATTRIBUTES int SHPRE()shmem_test_lock(volatile long *lock);

/* Cache Management Routines (deprecated) */
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_set_cache_inv(void) __attribute__ ((deprecated));
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_set_cache_line_inv(void *target) __attribute__ ((deprecated));
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_clear_cache_inv(void) __attribute__ ((deprecated));
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_clear_cache_line_inv(void *target) __attribute__ ((deprecated));
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_udcflush(void) __attribute__ ((deprecated));
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shmem_udcflush_line(void *target) __attribute__ ((deprecated));

/* Deprecated API */
SHMEM_FUNCTION_ATTRIBUTES int SHPRE()_num_pes(void) __attribute__ ((deprecated));
SHMEM_FUNCTION_ATTRIBUTES int SHPRE()_my_pe(void) __attribute__ ((deprecated));
SHMEM_FUNCTION_ATTRIBUTES void *SHPRE()shmalloc(size_t size) __attribute__ ((deprecated));
SHMEM_FUNCTION_ATTRIBUTES void *SHPRE()shmemalign(size_t alignment, size_t size) __attribute__ ((deprecated));
SHMEM_FUNCTION_ATTRIBUTES void *SHPRE()shrealloc(void *ptr, size_t size) __attribute__ ((deprecated));
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()shfree(void *ptr) __attribute__ ((deprecated));
SHMEM_FUNCTION_ATTRIBUTES void SHPRE()start_pes(int npes) __attribute__ ((deprecated));

