dnl Copyright (c) 2016 Intel Corporation. All rights reserved.
dnl This software is available to you under the BSD license.
dnl
dnl This file is part of the Sandia OpenSHMEM software package. For license
dnl information, see the LICENSE file in the top level directory of the
dnl distribution.
dnl
void SHPRE()shmemx_init_thread(int tl_requested, int *tl_provided);
double SHPRE()shmemx_wtime(void);
char* SHPRE()shmemx_nodename(void);

void SHPRE()shmemx_pcontrol(int level, ...);

/* Counting Operations */
void SHPRE()shmemx_getmem_ct(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe);
void SHPRE()shmemx_putmem_ct(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe);
void SHPRE()shmemx_ct_create(shmemx_ct_t *ct);
void SHPRE()shmemx_ct_free(shmemx_ct_t *ct);
long SHPRE()shmemx_ct_get(shmemx_ct_t ct);
void SHPRE()shmemx_ct_set(shmemx_ct_t ct, long value);
void SHPRE()shmemx_ct_wait(shmemx_ct_t ct, long wait_for);

/* "Loud" Barrier */
void SHPRE()shmemx_sync(int PE_start, int logPE_stride, int PE_size, long *pSync);
void SHPRE()shmemx_sync_all(void);

/* Domains */
int SHPRE()shmemx_domain_create(int thread_level, int num_domains, shmemx_domain_t domains[]);
void SHPRE()shmemx_domain_destroy(int num_domains, shmemx_domain_t domains[]);

/* Contexts */
int SHPRE()shmemx_ctx_create(shmemx_domain_t domain, shmemx_ctx_t *ctx);
void SHPRE()shmemx_ctx_destroy(shmemx_ctx_t ctx);
void SHPRE()shmemx_ctx_fence(shmemx_ctx_t ctx);
void SHPRE()shmemx_ctx_quiet(shmemx_ctx_t ctx);

/* RMA: Elemental Data Put Routines */
define(`SHMEMX_CTX_C_P',
`void SHPRE()shmemx_ctx_$1_p($2 *addr, $2 value, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEMX_CTX_C_P')

/* RMA: Contiguous Data Put Routines */
define(`SHMEMX_CTX_C_PUT',
`void SHPRE()shmemx_ctx_$1_put($2 *target, const $2 *source, size_t nelems, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEMX_CTX_C_PUT')

define(`SHMEMX_CTX_C_PUT_N',
`void SHPRE()shmemx_ctx_put$1(void* target, const void *source, size_t nelems, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEMX_CTX_C_PUT_N')
SHMEMX_CTX_C_PUT_N(mem,1);

/* RMA: Strided Put Routines */
define(`SHMEMX_CTX_C_IPUT',
`void SHPRE()shmemx_ctx_$1_iput($2 *target, const $2 *source,
SH_PAD(`$1')                 ptrdiff_t tst, ptrdiff_t sst,
SH_PAD(`$1')                 size_t len, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEMX_CTX_C_IPUT')

define(`SHMEMX_CTX_C_IPUT_N',
`void SHPRE()shmemx_ctx_iput$1(void *target, const void *source,
SH_PAD(`$1')                ptrdiff_t tst, ptrdiff_t sst, size_t len,
SH_PAD(`$1')                int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEMX_CTX_C_IPUT_N')

/* RMA: Elemental Data Get Routines */
define(`SHMEMX_CTX_C_G',
`$2 SHPRE()shmemx_ctx_$1_g(const $2 *addr, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEMX_CTX_C_G')

/* RMA: Contiguous Data Get Routines */
define(`SHMEMX_CTX_C_GET',
`void SHPRE()shmemx_ctx_$1_get($2 *target, const $2 *source, size_t nelems,int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEMX_CTX_C_GET')

define(`SHMEMX_CTX_C_GET_N',
`void SHPRE()shmemx_ctx_get$1(void* target, const void *source, size_t nelems, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEMX_CTX_C_GET_N')
SHMEMX_CTX_C_GET_N(mem,1);

/* RMA: Strided Get Routines */
define(`SHMEMX_CTX_C_IGET',
`void SHPRE()shmemx_ctx_$1_iget($2 *target, const $2 *source,
SH_PAD(`$1')                 ptrdiff_t tst, ptrdiff_t sst,
SH_PAD(`$1')                 size_t nelems, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEMX_CTX_C_IGET')

define(`SHMEMX_CTX_C_IGET_N',
`void SHPRE()shmemx_ctx_iget$1(void* target, const void *source,
SH_PAD(`$1')                ptrdiff_t tst, ptrdiff_t sst,
SH_PAD(`$1')                size_t nelems, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEMX_CTX_C_IGET_N')

/* RMA: Nonblocking Contiguous Data Put Routines */
define(`SHMEMX_CTX_C_PUT_NBI',
`void SHPRE()shmemx_ctx_$1_put_nbi($2 *target, const $2 *source, size_t nelems, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEMX_CTX_C_PUT_NBI')

define(`SHMEMX_CTX_C_PUT_N_NBI',
`void SHPRE()shmemx_ctx_put$1_nbi(void* target, const void *source, size_t nelems, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEMX_CTX_C_PUT_N_NBI')
SHMEMX_CTX_C_PUT_N_NBI(mem,1);

/* RMA: Nonblocking Contiguous Data Get Routines */
define(`SHMEMX_CTX_C_GET_NBI',
`void SHPRE()shmemx_ctx_$1_get_nbi($2 *target, const $2 *source, size_t nelems, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_RMA(`SHMEMX_CTX_C_GET_NBI')

define(`SHMEMX_CTX_C_GET_N_NBI',
`void SHPRE()shmemx_ctx_get$1_nbi(void* target, const void *source, size_t nelems, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_SIZES(`SHMEMX_CTX_C_GET_N_NBI')
SHMEMX_CTX_C_GET_N_NBI(mem,1);

/* AMO: Atomic Swap Routines */
define(`SHMEMX_CTX_C_SWAP',
`$2 SHPRE()shmemx_ctx_$1_swap($2 *target, $2 value, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_EXTENDED_AMO(`SHMEMX_CTX_C_SWAP')

/* Special case, only enabled when C++ and C11 bindings are disabled */
#if !defined(__cplusplus) && \
    !(defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L)
long SHPRE()shmemx_ctx_swap(long *target, long value, int pe, shmemx_ctx_t ctx);
#endif

/* AMO: Atomic Conditional Swap Routines */
define(`SHMEMX_CTX_C_CSWAP',
`$2 SHPRE()shmemx_ctx_$1_cswap($2 *target, $2 cond, $2 value, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEMX_CTX_C_CSWAP')

/* AMO: Atomic Fetch-and-Add Routines */
define(`SHMEMX_CTX_C_FADD',
`$2 SHPRE()shmemx_ctx_$1_fadd($2 *target, $2 value, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEMX_CTX_C_FADD')

/* AMO: Atomic Fetch-and-Increment Routines */
define(`SHMEMX_CTX_C_FINC',
`$2 SHPRE()shmemx_ctx_$1_finc($2 *target, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEMX_CTX_C_FINC')

/* AMO: Atomic Add Routines */
define(`SHMEMX_CTX_C_ADD',
`void SHPRE()shmemx_ctx_$1_add($2 *target, $2 value, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEMX_CTX_C_ADD')

/* AMO: Atomic Increment Routines */
define(`SHMEMX_CTX_C_INC',
`void SHPRE()shmemx_ctx_$1_inc($2 *target, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_AMO(`SHMEMX_CTX_C_INC')

/* AMO: Atomic Fetch Routines */
define(`SHMEMX_CTX_C_FETCH',
`$2 SHPRE()shmemx_ctx_$1_fetch(const $2 *target, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_EXTENDED_AMO(`SHMEMX_CTX_C_FETCH')

/* AMO: Atomic Set Routines */
define(`SHMEMX_CTX_C_SET',
`void SHPRE()shmemx_ctx_$1_set($2 *target, $2 value, int pe, shmemx_ctx_t ctx)')dnl
SHMEM_DECLARE_FOR_EXTENDED_AMO(`SHMEMX_CTX_C_SET')
