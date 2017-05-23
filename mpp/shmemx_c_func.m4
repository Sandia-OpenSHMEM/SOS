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

void SHPRE()shmemx_getmem_ct(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe);
void SHPRE()shmemx_putmem_ct(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe);
void SHPRE()shmemx_ct_create(shmemx_ct_t *ct);
void SHPRE()shmemx_ct_free(shmemx_ct_t *ct);
long SHPRE()shmemx_ct_get(shmemx_ct_t ct);
void SHPRE()shmemx_ct_set(shmemx_ct_t ct, long value);
void SHPRE()shmemx_ct_wait(shmemx_ct_t ct, long wait_for);

void SHPRE()shmemx_pcontrol(int level, ...);

/* AMO: Atomic Bitwise Routines */
define(`SHMEM_C_XOR',
`void SHPRE()shmemx_$1_atomic_xor($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_BITWISE_AMO(`SHMEM_C_XOR')

define(`SHMEM_C_AND',
`void SHPRE()shmemx_$1_atomic_and($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_BITWISE_AMO(`SHMEM_C_AND')

define(`SHMEM_C_OR',
`void SHPRE()shmemx_$1_atomic_or($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_BITWISE_AMO(`SHMEM_C_OR')

define(`SHMEM_C_FETCH_XOR',
`$2 SHPRE()shmemx_$1_atomic_fetch_xor($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_BITWISE_AMO(`SHMEM_C_FETCH_XOR')

define(`SHMEM_C_FETCH_AND',
`$2 SHPRE()shmemx_$1_atomic_fetch_and($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_BITWISE_AMO(`SHMEM_C_FETCH_AND')

define(`SHMEM_C_FETCH_OR',
`$2 SHPRE()shmemx_$1_atomic_fetch_or($2 *target, $2 value, int pe)')dnl
SHMEM_DECLARE_FOR_BITWISE_AMO(`SHMEM_C_FETCH_OR')
