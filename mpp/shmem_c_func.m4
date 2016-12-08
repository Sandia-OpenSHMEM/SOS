dnl Copyright (c) 2016 Intel Corporation. All rights reserved.
dnl This software is available to you under the BSD license.
dnl
dnl This file is part of the Sandia OpenSHMEM software package. For license
dnl information, see the LICENSE file in the top level directory of the
dnl distribution.
dnl
/* Library Setup, Exit, and Query Routines */
void SHPRE()shmem_init(void);
void SHPRE()shmem_finalize(void);
void SHPRE()shmem_global_exit(int status);
int SHPRE()shmem_n_pes(void);
int SHPRE()shmem_my_pe(void);

int SHPRE()shmem_pe_accessible(int pe);
int SHPRE()shmem_addr_accessible(const void *addr, int pe);
void *SHPRE()shmem_ptr(const void *target, int pe);

void SHPRE()shmem_info_get_version(int *major, int *minor);
void SHPRE()shmem_info_get_name(char *name);

/* Memory Management Routines */
void *SHPRE()shmem_malloc(size_t size);
void *SHPRE()shmem_align(size_t alignment, size_t size);
void *SHPRE()shmem_realloc(void *ptr, size_t size);
void SHPRE()shmem_free(void *ptr);

/* RMA: Elemental Data Put Routines */
define(`SHMEM_C_P',
`void SHPRE()shmem_$1_p($2 *addr, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_P')

/* RMA: Contiguous Data Put Routines */
define(`SHMEM_C_PUT',
`void SHPRE()shmem_$1_put($2 *target, const $2 *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_PUT')

define(`SHMEM_C_PUT_N',
`void SHPRE()shmem_put$1(void* target, const void *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_PUT_N')
SHMEM_C_PUT_N(mem,1);

/* RMA: Strided Put Routines */
define(`SHMEM_C_IPUT',
`void SHPRE()shmem_$1_iput($2 *target, const $2 *source,
SH_PAD(`$1')                 ptrdiff_t tst, ptrdiff_t sst,
SH_PAD(`$1')                 size_t len, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_IPUT')

define(`SHMEM_C_IPUT_N',
`void SHPRE()shmem_iput$1(void *target, const void *source,
SH_PAD(`$1')                ptrdiff_t tst, ptrdiff_t sst, size_t len,
SH_PAD(`$1')                int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_IPUT_N')

/* RMA: Elemental Data Get Routines */
define(`SHMEM_C_G',
`$2 SHPRE()shmem_$1_g(const $2 *addr, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_G')

/* RMA: Contiguous Data Get Routines */
define(`SHMEM_C_GET',
`void SHPRE()shmem_$1_get($2 *target, const $2 *source, size_t nelems,int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_GET')

define(`SHMEM_C_GET_N',
`void SHPRE()shmem_get$1(void* target, const void *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_GET_N')
SHMEM_C_GET_N(mem,1);

/* RMA: Strided Get Routines */
define(`SHMEM_C_IGET',
`void SHPRE()shmem_$1_iget($2 *target, const $2 *source,
SH_PAD(`$1')                 ptrdiff_t tst, ptrdiff_t sst,
SH_PAD(`$1')                 size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_IGET')

define(`SHMEM_C_IGET_N',
`void SHPRE()shmem_iget$1(void* target, const void *source,
SH_PAD(`$1')                ptrdiff_t tst, ptrdiff_t sst,
SH_PAD(`$1')                size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_IGET_N')

/* RMA: Nonblocking Contiguous Data Put Routines */
define(`SHMEM_C_PUT_NBI',
`void SHPRE()shmem_$1_put_nbi($2 *target, const $2 *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_PUT_NBI')

define(`SHMEM_C_PUT_N_NBI',
`void SHPRE()shmem_put$1_nbi(void* target, const void *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_PUT_N_NBI')
SHMEM_C_PUT_N_NBI(mem,1);

/* RMA: Nonblocking Contiguous Data Get Routines */
define(`SHMEM_C_GET_NBI',
`void SHPRE()shmem_$1_get_nbi($2 *target, const $2 *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_GET_NBI')

define(`SHMEM_C_GET_N_NBI',
`void SHPRE()shmem_get$1_nbi(void* target, const void *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_GET_N_NBI')
SHMEM_C_GET_N_NBI(mem,1);

/* AMO: Atomic Swap Routines */
define(`SHMEM_C_SWAP',
`$2 SHPRE()shmem_$1_swap($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_EXTENDED_AMO(`SHMEM_C_SWAP')

/* Special case, only enabled when C++ and C11 bindings are disabled */
#if !defined(__cplusplus) && \
    !(defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L)
long SHPRE()shmem_swap(long *target, long value, int pe);
#endif

/* AMO: Atomic Conditional Swap Routines */
define(`SHMEM_C_CSWAP',
`$2 SHPRE()shmem_$1_cswap($2 *target, $2 cond, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_CSWAP')

/* AMO: Atomic Fetch-and-Add Routines */
define(`SHMEM_C_FADD',
`$2 SHPRE()shmem_$1_fadd($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_FADD')

/* AMO: Atomic Fetch-and-Increment Routines */
define(`SHMEM_C_FINC',
`$2 SHPRE()shmem_$1_finc($2 *target, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_FINC')

/* AMO: Atomic Add Routines */
define(`SHMEM_C_ADD',
`void SHPRE()shmem_$1_add($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_ADD')

/* AMO: Atomic Increment Routines */
define(`SHMEM_C_INC',
`void SHPRE()shmem_$1_inc($2 *target, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_INC')

/* AMO: Atomic Fetch Routines */
define(`SHMEM_C_FETCH',
`$2 SHPRE()shmem_$1_fetch(const $2 *target, int pe)')dnl
SHMEM_DECLARE_FOR_EXTENDED_AMO(`SHMEM_C_FETCH')

/* AMO: Atomic Set Routines */
define(`SHMEM_C_SET',
`void SHPRE()shmem_$1_set($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_EXTENDED_AMO(`SHMEM_C_SET')

/* COLL: Barrier Synchronization Routines */
void SHPRE()shmem_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync);
void SHPRE()shmem_barrier_all(void);

/* COLL: Reduction Routines */
define(`SHMEM_C_TO_ALL',
`void SHPRE()shmem_$1_$4_to_all($2 *target,
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
void SHPRE()shmem_collect32(void *target, const void *source, size_t nlong,
                     int PE_start, int logPE_stride, int PE_size, long *pSync);
void SHPRE()shmem_collect64(void *target, const void *source, size_t nlong,
                     int PE_start, int logPE_stride, int PE_size, long *pSync);
void SHPRE()shmem_fcollect32(void *target, const void *source, size_t nlong,
                      int PE_start, int logPE_stride, int PE_size,
                      long *pSync);
void SHPRE()shmem_fcollect64(void *target, const void *source, size_t nlong,
                      int PE_start, int logPE_stride, int PE_size,
                      long *pSync);

/* COLL: Broadcast Routines */
void SHPRE()shmem_broadcast32(void *target, const void *source, size_t nlong,
                       int PE_root, int PE_start, int logPE_stride,
                       int PE_size, long *pSync);
void SHPRE()shmem_broadcast64(void *target, const void *source, size_t nlong,
                       int PE_root, int PE_start, int logPE_stride,
                       int PE_size, long *pSync);

/* COLL: All-to-All routines */
void SHPRE()shmem_alltoall32(void *dest, const void *source, size_t nelems, int PE_start,
                      int logPE_stride, int PE_size, long *pSync);
void SHPRE()shmem_alltoall64(void *dest, const void *source, size_t nelems, int PE_start,
                      int logPE_stride, int PE_size, long *pSync);

void SHPRE()shmem_alltoalls32(void *dest, const void *source, ptrdiff_t dst,
                       ptrdiff_t sst, size_t nelems, int PE_start,
                       int logPE_stride, int PE_size, long *pSync);
void SHPRE()shmem_alltoalls64(void *dest, const void *source, ptrdiff_t dst,
                       ptrdiff_t sst, size_t nelems, int PE_start,
                       int logPE_stride, int PE_size, long *pSync);

/* Point-to-Point Synchronization Routines */
define(`SHMEM_C_WAIT',
`void SHPRE()shmem_$1_wait(volatile $2 *var, $2 value);')dnl
void SHPRE()shmem_wait(volatile long *ivar, long cmp_value);
SHMEM_BIND_C_WAIT(`SHMEM_C_WAIT')

define(`SHMEM_C_WAIT_UNTIL',
`void SHPRE()shmem_$1_wait_until(volatile $2 *var, int cond, $2 value);')dnl
void SHPRE()shmem_wait_until(volatile long *ivar, int cmp, long value);
SHMEM_BIND_C_WAIT(`SHMEM_C_WAIT_UNTIL')

/* Memory Ordering Routines */
void SHPRE()shmem_quiet(void);
void SHPRE()shmem_fence(void);

/* Distributed Locking Routines */
void SHPRE()shmem_set_lock(volatile long *lock);
void SHPRE()shmem_clear_lock(volatile long *lock);
int SHPRE()shmem_test_lock(volatile long *lock);

/* Cache Management Routines (deprecated) */
void SHPRE()shmem_set_cache_inv(void) __attribute__ ((deprecated));
void SHPRE()shmem_set_cache_line_inv(void *target) __attribute__ ((deprecated));
void SHPRE()shmem_clear_cache_inv(void) __attribute__ ((deprecated));
void SHPRE()shmem_clear_cache_line_inv(void *target) __attribute__ ((deprecated));
void SHPRE()shmem_udcflush(void) __attribute__ ((deprecated));
void SHPRE()shmem_udcflush_line(void *target) __attribute__ ((deprecated));

/* Deprecated API */
int SHPRE()_num_pes(void) __attribute__ ((deprecated));
int SHPRE()_my_pe(void) __attribute__ ((deprecated));
void *SHPRE()shmalloc(size_t size) __attribute__ ((deprecated));
void *SHPRE()shmemalign(size_t alignment, size_t size) __attribute__ ((deprecated));
void *SHPRE()shrealloc(void *ptr, size_t size) __attribute__ ((deprecated));
void SHPRE()shfree(void *ptr) __attribute__ ((deprecated));
void SHPRE()start_pes(int npes) __attribute__ ((deprecated));

