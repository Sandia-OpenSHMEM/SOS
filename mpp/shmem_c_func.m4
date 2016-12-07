/* 8.1: OpenSHMEM Library API Library Setup, Exit, and Query Routines */
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

/* 8.2: Memory Management Routines */
void *SHPRE()shmem_malloc(size_t size);
void *SHPRE()shmem_align(size_t alignment, size_t size);
void *SHPRE()shmem_realloc(void *ptr, size_t size);
void SHPRE()shmem_free(void *ptr);

/* 8.3: Elemental Data Put Routines */
define(`SHMEM_C_P',
`void SHPRE()shmem_$1_p($2 *addr, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_P')

/* 8.3: Block Data Put Routines */
define(`SHMEM_C_PUT',
`void SHPRE()shmem_$1_put($2 *target, const $2 *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_PUT')

define(`SHMEM_C_PUT_N',
`void SHPRE()shmem_put$1(void* target, const void *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_PUT_N')
SHMEM_C_PUT_N(mem,1);

/* 8.3: Strided Put Routines */
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

/* 8.3: Elemental Data Get Routines */
define(`SHMEM_C_G',
`$2 SHPRE()shmem_$1_g(const $2 *addr, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_G')

/* 8.3: Block Data Get Routines */
define(`SHMEM_C_GET',
`void SHPRE()shmem_$1_get($2 *target, const $2 *source, size_t nelems,int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_GET')

define(`SHMEM_C_GET_N',
`void SHPRE()shmem_get$1(void* target, const void *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_GET_N')
SHMEM_C_GET_N(mem,1);

/* 8.3: Strided Get Routines */
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

/* 8.4: Nonblocking remote memory access routines -- Put */
define(`SHMEM_C_PUT_NBI',
`void SHPRE()shmem_$1_put_nbi($2 *target, const $2 *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_PUT_NBI')

define(`SHMEM_C_PUT_N_NBI',
`void SHPRE()shmem_put$1_nbi(void* target, const void *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_PUT_N_NBI')
SHMEM_C_PUT_N_NBI(mem,1);

/* 8.4: Nonblocking remote memory access routines -- Get */
define(`SHMEM_C_GET_NBI',
`void SHPRE()shmem_$1_get_nbi($2 *target, const $2 *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEM_C_GET_NBI')

define(`SHMEM_C_GET_N_NBI',
`void SHPRE()shmem_get$1_nbi(void* target, const void *source, size_t nelems, int pe)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEM_C_GET_N_NBI')
SHMEM_C_GET_N_NBI(mem,1);

/* 8.4: Atomic Memory fetch-and-operate Routines -- Swap */
define(`SHMEM_C_SWAP',
`$2 SHPRE()shmem_$1_swap($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_EXTENDED_AMO(`SHMEM_C_SWAP')

/* Special case, only enabled when C++ and C11 bindings are disabled */
#if !defined(__cplusplus) && \
    !(defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L && !defined(SHMEM_INTERNAL_INCLUDE))
long SHPRE()shmem_swap(long *target, long value, int pe);
#endif

/* 8.4: Atomic Memory fetch-and-operate Routines -- Cswap */
define(`SHMEM_C_CSWAP',
`$2 SHPRE()shmem_$1_cswap($2 *target, $2 cond, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_CSWAP')

/* 8.4: Atomic Memory fetch-and-operate Routines -- Fetch and Add */
define(`SHMEM_C_FADD',
`$2 SHPRE()shmem_$1_fadd($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_FADD')

/* 8.4: Atomic Memory fetch-and-operate Routines -- Fetch and Increment */
define(`SHMEM_C_FINC',
`$2 SHPRE()shmem_$1_finc($2 *target, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_FINC')

/* 8.4: Atomic Memory Operation Routines -- Add */
define(`SHMEM_C_ADD',
`void SHPRE()shmem_$1_add($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_ADD')

/* 8.4: Atomic Memory Operation Routines -- Increment */
define(`SHMEM_C_INC',
`void SHPRE()shmem_$1_inc($2 *target, int pe)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEM_C_INC')

/* 8.4: Atomic fetch */
define(`SHMEM_C_FETCH',
`$2 SHPRE()shmem_$1_fetch(const $2 *target, int pe)')dnl
SHMEM_DECLARE_FOR_EXTENDED_AMO(`SHMEM_C_FETCH')

/* 8.4: Atomic set */
define(`SHMEM_C_SET',
`void SHPRE()shmem_$1_set($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_EXTENDED_AMO(`SHMEM_C_SET')

/* 8.5: Barrier Synchronization Routines */
void SHPRE()shmem_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync);
void SHPRE()shmem_barrier_all(void);

/* 8.5: Reduction Routines */
define(`SHMEM_C_TO_ALL',
`void SHPRE()shmem_$1_$4_to_all($2 *target,
SH_PAD(`$1',`$4')                    const $2 *source, int nreduce,
SH_PAD(`$1',`$4')                    int PE_start, int logPE_stride,
SH_PAD(`$1',`$4')                    int PE_size, $2 *pWrk,
SH_PAD(`$1',`$4')                    long *pSync);')dnl
dnl
SHMEM_BIND_C_INTS_OP(`SHMEM_C_TO_ALL', `and')

SHMEM_BIND_C_INTS_OP(`SHMEM_C_TO_ALL', `or')

SHMEM_BIND_C_INTS_OP(`SHMEM_C_TO_ALL', `xor')

SHMEM_BIND_C_INTS_OP(`SHMEM_C_TO_ALL', `min')
SHMEM_BIND_C_FLOATS_OP(`SHMEM_C_TO_ALL', `min')

SHMEM_BIND_C_INTS_OP(`SHMEM_C_TO_ALL', `max')
SHMEM_BIND_C_FLOATS_OP(`SHMEM_C_TO_ALL', `max')

SHMEM_BIND_C_INTS_OP(`SHMEM_C_TO_ALL', `sum')
SHMEM_BIND_C_FLOATS_OP(`SHMEM_C_TO_ALL', `sum')
SHMEM_BIND_C_CMPLX_OP(`SHMEM_C_TO_ALL', `sum')

SHMEM_BIND_C_INTS_OP(`SHMEM_C_TO_ALL', `prod')
SHMEM_BIND_C_FLOATS_OP(`SHMEM_C_TO_ALL', `prod')
SHMEM_BIND_C_CMPLX_OP(`SHMEM_C_TO_ALL', `prod')

/* 8.5: Collect Routines */
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

/* 8.5: Broadcast Routines */
void SHPRE()shmem_broadcast32(void *target, const void *source, size_t nlong,
                       int PE_root, int PE_start, int logPE_stride,
                       int PE_size, long *pSync);
void SHPRE()shmem_broadcast64(void *target, const void *source, size_t nlong,
                       int PE_root, int PE_start, int logPE_stride,
                       int PE_size, long *pSync);

/* 8.6: All-to-all routines */
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

/* 8.6: Point-to-Point Synchronization Routines -- Wait */
void SHPRE()shmem_short_wait(volatile short *var, short value);
void SHPRE()shmem_int_wait(volatile int *var, int value);
void SHPRE()shmem_long_wait(volatile long *var, long value);
void SHPRE()shmem_longlong_wait(volatile long long *var, long long value);
void SHPRE()shmem_wait(volatile long *ivar, long cmp_value);

/* 8.6: Point-to-Point Synchronization Routines -- Wait Until */
void SHPRE()shmem_short_wait_until(volatile short *var, int cond, short value);
void SHPRE()shmem_int_wait_until(volatile int *var, int cond, int value);
void SHPRE()shmem_long_wait_until(volatile long *var, int cond, long value);
void SHPRE()shmem_longlong_wait_until(volatile long long *var, int cond,
                               long long value);
void SHPRE()shmem_wait_until(volatile long *ivar, int cmp, long value);

/* 8.7: Memory Ordering Routines */
void SHPRE()shmem_quiet(void);
void SHPRE()shmem_fence(void);

/* 8.8: Lock Routines */
void SHPRE()shmem_set_lock(volatile long *lock);
void SHPRE()shmem_clear_lock(volatile long *lock);
int SHPRE()shmem_test_lock(volatile long *lock);

/* 8.9: Cache Management Routines (deprecated) */
void SHPRE()shmem_set_cache_inv(void) __attribute__ ((deprecated));
void SHPRE()shmem_set_cache_line_inv(void *target) __attribute__ ((deprecated));
void SHPRE()shmem_clear_cache_inv(void) __attribute__ ((deprecated));
void SHPRE()shmem_clear_cache_line_inv(void *target) __attribute__ ((deprecated));
void SHPRE()shmem_udcflush(void) __attribute__ ((deprecated));
void SHPRE()shmem_udcflush_line(void *target) __attribute__ ((deprecated));

/* F: Deprecated API */
int SHPRE()_num_pes(void) __attribute__ ((deprecated));
int SHPRE()_my_pe(void) __attribute__ ((deprecated));
void *SHPRE()shmalloc(size_t size) __attribute__ ((deprecated));
void *SHPRE()shmemalign(size_t alignment, size_t size) __attribute__ ((deprecated));
void *SHPRE()shrealloc(void *ptr, size_t size) __attribute__ ((deprecated));
void SHPRE()shfree(void *ptr) __attribute__ ((deprecated));
void SHPRE()start_pes(int npes) __attribute__ ((deprecated));

