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

void SHPRE()shmemx_int_bxor(int *target, int value, int pe);
void SHPRE()shmemx_long_bxor(long *target, long value, int pe);
void SHPRE()shmemx_longlong_bxor(long long *target, long long value, int pe);
