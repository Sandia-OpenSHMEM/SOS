
/* -*- C -*-
 *
 * Copyright (c) 2020 NVidia Corporation.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef TRANSPORT_UCX_H
#define TRANSPORT_UCX_H

#include "shmem_internal.h"
#include "transport.h"
#include <ucs/type/status.h>
#include <ucp/api/ucp_def.h>
#include <ucp/api/ucp.h>

/* Operations */
enum shm_internal_op_t {
    SHM_INTERNAL_BAND,
    SHM_INTERNAL_BOR,
    SHM_INTERNAL_BXOR,
    SHM_INTERNAL_MIN,
    SHM_INTERNAL_MAX,
    SHM_INTERNAL_SUM,
    SHM_INTERNAL_PROD
};

typedef enum shm_internal_op_t shm_internal_op_t;
typedef int shmem_transport_ct_t;

struct shmem_transport_ctx_t {
    long options;
    struct shmem_internal_team_t *team;
};
typedef struct shmem_transport_ctx_t shmem_transport_ctx_t;

typedef struct {
    size_t         addr_len;
    ucp_address_t *addr;
    ucp_ep_h       ep;
    /* FIXME: Add remote global addressing optimization */
    uint8_t       *data_base, *heap_base;
    ucp_rkey_h     data_rkey, heap_rkey;
} shmem_transport_peer_t;

extern shmem_transport_peer_t *shmem_transport_peers;
extern ucp_worker_h shmem_transport_ucp_worker;

#define UCX_CHECK_STATUS(status)                                                        \
    do {                                                                                \
        if (status != UCS_OK) {                                                         \
            RAISE_ERROR_MSG("UCX error %d: %s\n", status, ucs_status_string(status));   \
        }                                                                               \
    } while (0)

#define UCX_CHECK_STATUS_INPROGRESS(status)                                             \
    do {                                                                                \
        if (status != UCS_OK && status != UCS_INPROGRESS) {                             \
            RAISE_ERROR_MSG("UCX error %d: %s\n", status, ucs_status_string(status));   \
        }                                                                               \
    } while (0)

int shmem_transport_init(void);
int shmem_transport_startup(void);
int shmem_transport_fini(void);

static inline
void shmem_transport_ucx_get_mr(const void *addr, int dest_pe,
                                uint8_t **remote_addr, ucp_rkey_h *rkey) {
    if ((void*) addr >= shmem_internal_data_base &&
        (uint8_t*) addr < (uint8_t*) shmem_internal_data_base + shmem_internal_data_length) {

        *rkey = shmem_transport_peers[dest_pe].data_rkey;
        *remote_addr = (uint8_t*) (((uint8_t *) addr - (uint8_t *) shmem_internal_data_base) +
                       shmem_transport_peers[dest_pe].data_base);

    } else if ((void*) addr >= shmem_internal_heap_base &&
               (uint8_t*) addr < (uint8_t*) shmem_internal_heap_base + shmem_internal_heap_length) {

        *rkey = shmem_transport_peers[dest_pe].heap_rkey;
        *remote_addr = (uint8_t*) (((uint8_t *) addr - (uint8_t *) shmem_internal_heap_base) +
                       shmem_transport_peers[dest_pe].heap_base);
    } else {
        RAISE_ERROR_MSG("address (%p) outside of symmetric areas\n", addr);
    }
}

static inline
void
shmem_transport_probe(void)
{
    /* FIXME: Requires hard polling, manual progress, and likely separate
     * progress thread to meed progress requirements */
    ucp_worker_progress(shmem_transport_ucp_worker);
}

static inline
int
shmem_transport_ctx_create(struct shmem_internal_team_t *team, long options, shmem_transport_ctx_t **ctx)
{
    if (team == SHMEMX_TEAM_INVALID)
        return 1;

    *ctx = malloc(sizeof(shmem_transport_ctx_t));

    if (*ctx == NULL)
        return 1;

    (*ctx)->team = team;
    (*ctx)->options = 0;

    return 0;
}

static inline
void
shmem_transport_ctx_destroy(shmem_transport_ctx_t *ctx)
{
    if (ctx == SHMEMX_CTX_INVALID)
        return;
    else if (ctx == (shmem_transport_ctx_t *) SHMEM_CTX_DEFAULT)
        RAISE_ERROR_STR("Cannot destroy SHMEM_CTX_DEFAULT");
    else
        free(ctx);

    return;
}

static inline
int
shmem_transport_quiet(shmem_transport_ctx_t* ctx)
{
    ucs_status_t status;

    status = ucp_worker_flush(shmem_transport_ucp_worker);
    UCX_CHECK_STATUS(status);

    return 0;
}

static inline
int
shmem_transport_fence(shmem_transport_ctx_t* ctx)
{
    ucs_status_t status;

    status = ucp_worker_fence(shmem_transport_ucp_worker);
    UCX_CHECK_STATUS(status);

    return 0;
}

static inline
void
shmem_transport_put_scalar(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len, int pe)
{
    ucs_status_t status;
    ucp_rkey_h rkey;
    uint8_t *remote_addr;

    shmem_transport_ucx_get_mr(target, pe, &remote_addr, &rkey);

    /* FIXME: This needs to be a buffered put. Use bb here? */
    status = ucp_put_nbi(shmem_transport_peers[pe].ep, source, len, (uint64_t) remote_addr, rkey);
    UCX_CHECK_STATUS_INPROGRESS(status);

    /* FIXME: Remove when buffered */
    if (status != UCS_OK) {
        shmem_internal_assert(status == UCS_INPROGRESS);
        shmem_transport_quiet(ctx);
    }
}

static inline
void
shmem_transport_put_nb(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len,
                       int pe, long *completion)
{
    ucs_status_t status;
    ucp_rkey_h rkey;
    uint8_t *remote_addr;

    shmem_transport_ucx_get_mr(target, pe, &remote_addr, &rkey);

    /* FIXME: Could use ucp_put_nb here to take advantage of the completion flag */
    status = ucp_put_nbi(shmem_transport_peers[pe].ep, source, len, (uint64_t) remote_addr, rkey);
    UCX_CHECK_STATUS_INPROGRESS(status);
}

static inline
void
shmem_transport_put_wait(shmem_transport_ctx_t* ctx, long *completion)
{
    shmem_transport_quiet(ctx);
}

static inline
void
shmem_transport_put_nbi(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len,
                       int pe)
{
    ucs_status_t status;
    ucp_rkey_h rkey;
    uint8_t *remote_addr;

    shmem_transport_ucx_get_mr(target, pe, &remote_addr, &rkey);

    status = ucp_put_nbi(shmem_transport_peers[pe].ep, source, len, (uint64_t) remote_addr, rkey);
    UCX_CHECK_STATUS_INPROGRESS(status);
}

static inline
void
shmem_transport_put_signal_nbi(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len,
                               uint64_t *sig_addr, uint64_t signal, int pe)
{
    shmem_transport_put_nbi(ctx, target, source, len, pe);
    shmem_transport_fence(ctx);
    shmem_transport_put_scalar(ctx, sig_addr, &signal, sizeof(uint64_t), pe);
}

static inline
void
shmem_transport_get(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len, int pe)
{
    ucs_status_t status;
    ucp_rkey_h rkey;
    uint8_t *remote_addr;

    shmem_transport_ucx_get_mr(source, pe, &remote_addr, &rkey);

    status = ucp_get_nbi(shmem_transport_peers[pe].ep, target, len, (uint64_t) remote_addr, rkey);
    UCX_CHECK_STATUS_INPROGRESS(status);
}

static inline
void
shmem_transport_get_wait(shmem_transport_ctx_t* ctx)
{
    shmem_transport_quiet(ctx);
}


static inline
void
shmem_transport_swap(shmem_transport_ctx_t* ctx, void *target, const void *source, void *dest,
                     size_t len, int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_swap_nbi(shmem_transport_ctx_t* ctx, void *target, const void *source, void *dest,
                         size_t len, int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_cswap(shmem_transport_ctx_t* ctx, void *target, const void *source, void *dest,
                      const void *operand, size_t len, int pe,
                      shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_cswap_nbi(shmem_transport_ctx_t* ctx, void *target, const void *source, void *dest,
                          const void *operand, size_t len, int pe,
                          shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_mswap(shmem_transport_ctx_t* ctx, void *target, const void *source, void *dest,
                      const void *mask, size_t len, int pe,
                      shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_atomic(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len,
                       int pe, shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    if (datatype == SHM_INTERNAL_LONG && op == SHM_INTERNAL_SUM) {
        uint8_t *remote_addr;
        ucp_rkey_h rkey;
        ucs_status_t status;

        shmem_transport_ucx_get_mr(target, pe, &remote_addr, &rkey);

        status = ucp_atomic_post(shmem_transport_peers[pe].ep, UCP_ATOMIC_POST_OP_ADD,
                                 *(long*)source, sizeof(long), (uint64_t) remote_addr, rkey);
        UCX_CHECK_STATUS_INPROGRESS(status);
    }
    else {
        RAISE_ERROR_STR("No path to peer");
    }
}

static inline
void
shmem_transport_atomicv(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len,
                        int pe, shm_internal_op_t op, shm_internal_datatype_t datatype, long *completion)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_fetch_atomic(shmem_transport_ctx_t* ctx, void *target, const void *source, void *dest, size_t len,
                             int pe, shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_fetch_atomic_nbi(shmem_transport_ctx_t* ctx, void *target, const void *source, void *dest, size_t len,
                                 int pe, shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_atomic_fetch(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len,
                             int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_atomic_set(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len,
                             int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
int shmem_transport_atomic_supported(shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    /* Use software reductions, instead of atomic vector operation */
    return 0;
}

static inline
void shmem_transport_syncmem(void)
{
    return;
}

/*** Functions below are not supported ***/

static inline
void shmem_transport_ct_create(shmem_transport_ct_t **ct_ptr)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void shmem_transport_ct_free(shmem_transport_ct_t **ct_ptr)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
long shmem_transport_ct_get(shmem_transport_ct_t *ct)
{
    RAISE_ERROR_STR("No path to peer");
    return 0;
}

static inline
void shmem_transport_ct_set(shmem_transport_ct_t *ct, long value)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void shmem_transport_ct_wait(shmem_transport_ct_t *ct, long wait_for)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_put_ct_nb(shmem_transport_ct_t *ct, void *target, const void
                          *source, size_t len, int pe, long *completion)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void shmem_transport_get_ct(shmem_transport_ct_t *ct, void
                            *target, const void *source, size_t len, int pe)
{
    RAISE_ERROR_STR("No path to peer");
}

/**
 * Query the value of the transport's received messages counter.
 */
static inline
uint64_t shmem_transport_received_cntr_get(void)
{
    RAISE_ERROR_STR("Transport does not support received counter");
    return 0;
}

/**
 * Wait for the transport's received messages counter to be greater than or
 * equal to the given value.
 *
 * @param ge_val Function returns when received messages >= ge_val
 */
static inline
void shmem_transport_received_cntr_wait(uint64_t ge_val)
{
    RAISE_ERROR_STR("Transport does not support received counter");
}

static inline
uint64_t shmem_transport_pcntr_get_issued_write(shmem_transport_ctx_t *ctx)
{
    return 0;
}

static inline
uint64_t shmem_transport_pcntr_get_issued_read(shmem_transport_ctx_t *ctx)
{
    return 0;
}

static inline
uint64_t shmem_transport_pcntr_get_completed_write(shmem_transport_ctx_t *ctx)
{
    return 0;
}

static inline
uint64_t shmem_transport_pcntr_get_completed_read(shmem_transport_ctx_t *ctx)
{
    return 0;
}

static inline
uint64_t shmem_transport_pcntr_get_completed_target(void)
{
    return 0;
}

static inline
void shmem_transport_pcntr_get_all(shmem_transport_ctx_t *ctx, shmemx_pcntr_t *pcntr)
{
    return;
}

#endif /* TRANSPORT_UCX_H */
