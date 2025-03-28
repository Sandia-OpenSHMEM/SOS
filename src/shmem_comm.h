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

#ifndef PORTALS_SHMEM_DATA_H
#define PORTALS_SHMEM_DATA_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmemx.h"

#include "shmem_atomic.h"

#include "transport.h"
#include "shr_transport.h"

static inline
void
shmem_internal_put_nb(shmem_ctx_t ctx, void *target, const void *source, size_t len, int pe,
                      long *completion)
{
    if (len == 0)
        return;

    if (shmem_shr_transport_use_write(ctx, target, source, len, pe)) {
        shmem_shr_transport_put(ctx, target, source, len, pe);
    } else {
        shmem_transport_put_nb((shmem_transport_ctx_t *)ctx, target, source, len, pe, completion);
    }
}


static inline
void
shmem_internal_put_wait(shmem_ctx_t ctx, long *completion)
{
    shmem_transport_put_wait((shmem_transport_ctx_t *)ctx, completion);
    /* on-node is always blocking, so this is a no-op for them */
}


static inline
void
shmem_internal_put_scalar(shmem_ctx_t ctx, void *target, const void *source, size_t len, int pe)
{
    shmem_internal_assert(len > 0);

    if (shmem_shr_transport_use_write(ctx, target, source, len, pe)) {
        shmem_shr_transport_put_scalar(ctx, target, source, len, pe);
    } else {
#ifndef DISABLE_OFI_INJECT
        shmem_transport_put_scalar((shmem_transport_ctx_t *)ctx, target, source, len, pe);
#else
        long completion = 0;
        shmem_transport_put_nb((shmem_transport_ctx_t *)ctx, target, source, len, pe, &completion);
	shmem_internal_put_wait(ctx, &completion);
#endif
    }
}

static inline
void
shmem_internal_put_signal_nbi(shmem_ctx_t ctx, void *target, const void *source, size_t len,
                              uint64_t *sig_addr, uint64_t signal, int sig_op, int pe)
{
    if (len == 0) {
        if (sig_op == SHMEM_SIGNAL_ADD)
            shmem_transport_atomic((shmem_transport_ctx_t *) ctx, sig_addr, &signal, sizeof(uint64_t),
                                   pe, SHM_INTERNAL_SUM, SHM_INTERNAL_UINT64);
        else
            shmem_transport_atomic_set((shmem_transport_ctx_t *) ctx, sig_addr, &signal,
                                      sizeof(uint64_t), pe, SHM_INTERNAL_UINT64);
        return;
    }

    if (shmem_shr_transport_use_write(ctx, target, source, len, pe)) {
        shmem_shr_transport_put_signal(ctx, target, source, len, sig_addr, signal, sig_op, pe);
    } else {
        shmem_transport_put_signal_nbi((shmem_transport_ctx_t *) ctx, target, source, len, sig_addr, signal, sig_op, pe);
    }
}

static inline
void
shmem_internal_put_nbi(shmem_ctx_t ctx, void *target, const void *source, size_t len, int pe)
{
    if (len == 0) return;

    if (shmem_shr_transport_use_write(ctx, target, source, len, pe)) {
        shmem_shr_transport_put(ctx, target, source, len, pe);
    } else {
        shmem_transport_put_nbi((shmem_transport_ctx_t *)ctx, target, source, len, pe);
    }
}


static inline
void
shmem_internal_put_ct_nb(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe,
                      long *completion)
{
    /* TODO: add shortcut for on-node-comms */
    shmem_transport_put_ct_nb((shmem_transport_ct_t *)
                              ct, target, source, len, pe, completion);
}


static inline
void
shmem_internal_get(shmem_ctx_t ctx, void *target, const void *source, size_t len, int pe)
{
    if (len == 0) return;

    if (shmem_shr_transport_use_read(ctx, target, source, len, pe)) {
        shmem_shr_transport_get(ctx, target, source, len, pe);
    } else {
        shmem_transport_get((shmem_transport_ctx_t *)ctx, target, source, len, pe);
    }
}


static inline
void
shmem_internal_get_ct(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe)
{
    /* TODO: add shortcut for on-node-comms */
    shmem_transport_get_ct((shmem_transport_ct_t *) ct,
                           target, source, len, pe);
}


static inline
void
shmem_internal_get_wait(shmem_ctx_t ctx)
{
    shmem_transport_get_wait((shmem_transport_ctx_t *)ctx);
    /* on-node is always blocking, so this is a no-op for them */
}

static inline
void
shmem_internal_swap(shmem_ctx_t ctx, void *target, void *source, void *dest, size_t len,
                    int pe, shm_internal_datatype_t datatype)
{
    shmem_internal_assert(len > 0);

    if (shmem_shr_transport_use_atomic(ctx, target, len, pe, datatype)) {
        shmem_shr_transport_swap(ctx, target, source, dest, len, pe, datatype);
    } else {
        shmem_transport_swap((shmem_transport_ctx_t *)ctx, target, source, dest, len, pe, datatype);
    }
}


static inline
void
shmem_internal_swap_nbi(shmem_ctx_t ctx, void *target, void *source,
                        void *dest, size_t len, int pe,
                        shm_internal_datatype_t datatype)
{
    shmem_internal_assert(len > 0);

    if (shmem_shr_transport_use_atomic(ctx, target, len, pe, datatype)) {
        shmem_shr_transport_swap(ctx, target, source, dest, len, pe, datatype);
    } else {
        shmem_transport_swap_nbi((shmem_transport_ctx_t *)ctx, target, source,
                                 dest, len, pe, datatype);
    }
}


static inline
void
shmem_internal_cswap(shmem_ctx_t ctx, void *target, void *source, void *dest, void *operand, size_t len,
                    int pe, shm_internal_datatype_t datatype)
{
    shmem_internal_assert(len > 0);

    if (shmem_shr_transport_use_atomic(ctx, target, len, pe, datatype)) {
        shmem_shr_transport_cswap(ctx, target, source, dest, operand, len, pe, datatype);
    } else {
        shmem_transport_cswap((shmem_transport_ctx_t *)ctx, target, source,
                              dest, operand, len, pe, datatype);
    }
}


static inline
void
shmem_internal_cswap_nbi(shmem_ctx_t ctx, void *target, void *source,
                         void *dest, void *operand, size_t len, int pe,
                         shm_internal_datatype_t datatype)
{
    shmem_internal_assert(len > 0);

    if (shmem_shr_transport_use_atomic(ctx, target, len, pe, datatype)) {
        shmem_shr_transport_cswap(ctx, target, source, dest, operand, len, pe, datatype);
    } else {
        shmem_transport_cswap_nbi((shmem_transport_ctx_t *)ctx, target, source,
                                  dest, operand, len, pe, datatype);
    }
}


static inline
void
shmem_internal_mswap(shmem_ctx_t ctx, void *target, void *source, void *dest, void *mask, size_t len,
                    int pe, shm_internal_datatype_t datatype)
{
    shmem_internal_assert(len > 0);

    if (shmem_shr_transport_use_atomic(ctx, target, len, pe, datatype)) {
        shmem_shr_transport_mswap(ctx, target, source, dest, mask, len, pe, datatype);
    } else {
        shmem_transport_mswap((shmem_transport_ctx_t *)ctx, target, source,
                              dest, mask, len, pe, datatype);
    }
}


static inline
void
shmem_internal_atomic(shmem_ctx_t ctx, void *target, const void *source, size_t len,
                      int pe, shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    shmem_internal_assert(len > 0);

    if (shmem_shr_transport_use_atomic(ctx, target, len, pe, datatype)) {
        shmem_shr_transport_atomic(ctx, target, source, len, pe, op, datatype);
    } else {
#ifdef DISABLE_NONFETCH_AMO
        /* FIXME: This is a temporary workaround to resolve a known issue with non-fetching AMOs when using
           the CXI provider */
        unsigned long long tmp_fetch = 0;
        shmem_transport_fetch_atomic((shmem_transport_ctx_t *)ctx, target,
                                     source, &tmp_fetch, len, pe, op, datatype);
        shmem_transport_get_wait((shmem_transport_ctx_t *)ctx);
#else
        shmem_transport_atomic((shmem_transport_ctx_t *)ctx, target, source,
                               len, pe, op, datatype);
#endif
    }
}


static inline
void
shmem_internal_atomic_fetch(shmem_ctx_t ctx, void *target, const void *source, size_t len,
                            int pe, shm_internal_datatype_t datatype)
{
    shmem_internal_assert(len > 0);

    if (shmem_shr_transport_use_atomic(ctx, target, len, pe, datatype)) {
        shmem_shr_transport_atomic_fetch(ctx, target, source, len, pe, datatype);
    } else {
        shmem_transport_atomic_fetch((shmem_transport_ctx_t *)ctx, target,
                                     source, len, pe, datatype);
    }
}

static inline
void
shmem_internal_atomic_set(shmem_ctx_t ctx, void *target, const void *source, size_t len,
                          int pe, shm_internal_datatype_t datatype)
{
    shmem_internal_assert(len > 0);

    if (shmem_shr_transport_use_atomic(ctx, target, len, pe, datatype)) {
        shmem_shr_transport_atomic_set(ctx, target, source, len, pe, datatype);
    } else {
#ifdef DISABLE_NONFETCH_AMO
        /* FIXME: This is a temporary workaround to resolve a known issue with non-fetching AMOs when using
           the CXI provider */
        unsigned long long tmp_fetch = 0;
        shmem_transport_fetch_atomic((shmem_transport_ctx_t *)ctx, target,
                                     source, &tmp_fetch, len, pe, FI_ATOMIC_WRITE, datatype);
        shmem_transport_get_wait((shmem_transport_ctx_t *)ctx);
#else
        shmem_transport_atomic_set((shmem_transport_ctx_t *)ctx, target,
                                   source, len, pe, datatype);
#endif
    }
}


static inline
void
shmem_internal_fetch_atomic(shmem_ctx_t ctx, void *target, void *source, void *dest, size_t len,
                            int pe, shm_internal_op_t op,
                            shm_internal_datatype_t datatype)
{
    shmem_internal_assert(len > 0);

    if (shmem_shr_transport_use_atomic(ctx, target, len, pe, datatype)) {
        shmem_shr_transport_fetch_atomic(ctx, target, source, dest, len, pe,
                                         op, datatype);
    } else {
        shmem_transport_fetch_atomic((shmem_transport_ctx_t *)ctx, target,
                                     source, dest, len, pe, op, datatype);
    }
}


static inline
void
shmem_internal_atomicv(shmem_ctx_t ctx, void *target, const void *source,
                       size_t count, size_t type_size, int pe, shm_internal_op_t op,
                       shm_internal_datatype_t datatype, long *completion)
{
    size_t len = type_size * count;
    shmem_internal_assert(len > 0);

#ifdef DISABLE_NONFETCH_AMO
    /* FIXME: This is a temporary workaround to resolve a known issue with non-fetching AMOs when using
        the CXI provider */
    unsigned long long tmp_fetch = 0;
    for (size_t i = 0; i < count; i++) {
        shmem_internal_fetch_atomic(ctx, ((uint8_t *) target) + (i * type_size),
                                    ((uint8_t *) source) + (i * type_size), &tmp_fetch, type_size,
                                    pe, op, datatype);
        shmem_transport_get_wait((shmem_transport_ctx_t *)ctx);
    }
    *completion += 1;
#else
    if (shmem_shr_transport_use_atomic(ctx, target, len, pe, datatype)) {
        for (size_t i = 0; i < count; i++) {
            shmem_shr_transport_atomic(ctx, ((uint8_t *) target) + (i * type_size),
                                       ((uint8_t *) source) + (i * type_size), type_size,
                                       pe, op, datatype);
        }
    } else {
        shmem_transport_atomicv((shmem_transport_ctx_t *)ctx, target, source, count, type_size,
                                pe, op, datatype, completion);
    }
#endif
}


static inline
void
shmem_internal_fetch_atomic_nbi(shmem_ctx_t ctx, void *target, void *source,
                                void *dest, size_t len, int pe,
                                shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    shmem_internal_assert(len > 0);

    if (shmem_shr_transport_use_atomic(ctx, target, len, pe, datatype)) {
        shmem_shr_transport_fetch_atomic(ctx, target, source, dest, len, pe,
                                         op, datatype);
    } else {
        shmem_transport_fetch_atomic_nbi((shmem_transport_ctx_t *)ctx, target,
                                         source, dest, len, pe, op, datatype);
    }
}

static inline
void
shmem_internal_atomic_fetch_nbi(shmem_ctx_t ctx, void *target, const void *source,
                                size_t len, int pe, shm_internal_datatype_t datatype)
{
    shmem_internal_assert(len > 0);

    if (shmem_shr_transport_use_atomic(ctx, target, len, pe, datatype)) {
        shmem_shr_transport_atomic_fetch(ctx, target, source, len, pe, datatype);
    } else {
        shmem_transport_atomic_fetch_nbi((shmem_transport_ctx_t *)ctx, target,
                                         source, len, pe, datatype);
    }
}


static inline
void shmem_internal_ct_create(shmemx_ct_t *ct)
{
    shmem_transport_ct_create((shmem_transport_ct_t **) ct);
}


static inline
void shmem_internal_ct_free(shmemx_ct_t *ct)
{
    shmem_transport_ct_free((shmem_transport_ct_t **) ct);
}


static inline
long shmem_internal_ct_get(shmemx_ct_t ct)
{
    return shmem_transport_ct_get((shmem_transport_ct_t *) ct);
}


static inline
void shmem_internal_ct_set(shmemx_ct_t ct, long value)
{
    shmem_transport_ct_set((shmem_transport_ct_t *) ct, value);
}


static inline
void shmem_internal_ct_wait(shmemx_ct_t ct, long wait_for)
{
    shmem_transport_ct_wait((shmem_transport_ct_t *) ct, wait_for);
}


/* Uses internal put for external heap config; otherwise memcpy */
static inline
void shmem_internal_copy_self(void *dest, const void *source, size_t nelems)
{
#ifdef USE_FI_HMEM
    // "completion" set to 1 to wait for completion of put operation initiated
    // by shmem_internal_put_nb, even if "completion" not incremented in call 
    // to shmem_internal_put_nb.
    long completion = 1;
    shmem_internal_put_nb(SHMEM_CTX_DEFAULT, dest, source, nelems,
                          shmem_internal_my_pe, &completion);
    shmem_internal_put_wait(SHMEM_CTX_DEFAULT, &completion);
#else
    memcpy(dest, source, nelems);
#endif
}

#endif
