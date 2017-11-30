/* -*- C -*-
 *
 * Copyright (c) 2016 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef TRANSPORT_OFI_H
#define TRANSPORT_OFI_H

#include <stdio.h>
#include <stdlib.h>
#include <rdma/fabric.h>
#include <rdma/fi_errno.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_tagged.h>
#include <rdma/fi_rma.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_atomic.h>
#include <netdb.h>
#if USE_PORTALS4
#include <portals4.h>
#endif
#include <string.h>
#include <unistd.h>
#include <stddef.h>
#include <inttypes.h>
#include "shmem_free_list.h"
#include "shmem_internal.h"
#include "shmem_atomic.h"

#ifndef ENABLE_HARD_POLLING
extern struct fid_cntr*                 shmem_transport_ofi_target_cntrfd;
#endif
#ifndef ENABLE_MR_SCALABLE
extern uint64_t*                        shmem_transport_ofi_target_heap_keys;
extern uint64_t*                        shmem_transport_ofi_target_data_keys;
#ifndef ENABLE_REMOTE_VIRTUAL_ADDRESSING
extern uint8_t**                       shmem_transport_ofi_target_heap_addrs;
extern uint8_t**                       shmem_transport_ofi_target_data_addrs;
#endif /* ENABLE_REMOTE_VIRTUAL_ADDRESSING */
#endif /* ENABLE_MR_SCALABLE */
extern uint64_t                         shmem_transport_ofi_max_poll;
extern long                             shmem_transport_ofi_put_poll_limit;
extern long                             shmem_transport_ofi_get_poll_limit;
extern size_t                           shmem_transport_ofi_max_buffered_send;
extern size_t                           shmem_transport_ofi_max_msg_size;
extern size_t                           shmem_transport_ofi_bounce_buffer_size;
extern long                             shmem_transport_ofi_max_bounce_buffers;

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#define OFI_CHECK_ERROR(ret)                                                    \
    do {                                                                        \
        if (ret) {                                                              \
            RAISE_ERROR_MSG("OFI error %d: %s\n", ret, fi_strerror(ret));       \
        }                                                                       \
    } while (0)

#define OFI_CTX_CHECK_ERROR(ctx, err)                                           \
    do {                                                                        \
        if ((err) == -FI_EAVAIL) {                                              \
            struct fi_cq_err_entry e = {0};                                     \
            ssize_t ret = fi_cq_readerr((ctx)->cq, (void *)&e, 0);              \
            if (ret == 1) {                                                     \
                const char *errmsg = fi_cq_strerror((ctx)->cq, e.prov_errno,    \
                                                    e.err_data, NULL, 0);       \
                RAISE_ERROR_MSG("Error in operation: %s\n", errmsg);            \
            } else {                                                            \
                RAISE_ERROR_MSG("Error reading from CQ (%zd)\n", ret);          \
            }                                                                   \
        } else if (err) {                                                       \
            RAISE_ERROR_MSG("OFI error %zd: %s\n", err, fi_strerror(err));      \
        }                                                                       \
    } while (0)

#define OFI_CHECK_ERROR_MSG(ret, ...)                                           \
    do {                                                                        \
        if (ret) {                                                              \
            RAISE_ERROR_MSG(__VA_ARGS__);                                       \
        }                                                                       \
    } while (0)

#define OFI_CHECK_RETURN_STR(ret, msg)                                          \
    do {                                                                        \
        if (ret) {                                                              \
            RAISE_WARN_STR(msg);                                                \
            return ret;                                                         \
        }                                                                       \
    } while (0)

#define OFI_CHECK_RETURN_MSG(ret, ...)                                          \
    do {                                                                        \
        if (ret) {                                                              \
            RAISE_WARN_MSG(__VA_ARGS__);                                        \
            return ret;                                                         \
        }                                                                       \
    } while (0)


#ifdef ENABLE_MR_SCALABLE
static inline
void shmem_transport_ofi_get_mr(const void *addr, int dest_pe,
                                uint8_t **mr_addr, uint64_t *key) {
#ifdef ENABLE_REMOTE_VIRTUAL_ADDRESSING
    *key = 0;
    *mr_addr = (uint8_t*) addr;
#else
    if ((void*) addr >= shmem_internal_data_base &&
        (uint8_t*) addr < (uint8_t*) shmem_internal_data_base + shmem_internal_data_length) {

        *key = 0;
        *mr_addr = (uint8_t*) ((uint8_t *) addr - (uint8_t *) shmem_internal_data_base);

    } else if ((void*) addr >= shmem_internal_heap_base &&
               (uint8_t*) addr < (uint8_t*) shmem_internal_heap_base + shmem_internal_heap_length) {

        *key = 1;
        *mr_addr = (uint8_t*) ((uint8_t *) addr - (uint8_t *) shmem_internal_heap_base);
    } else {
        *key = 0;
        *mr_addr = NULL;
        RAISE_ERROR_MSG("address (%p) outside of symmetric areas\n", addr);
    }
#endif /* ENABLE_REMOTE_VIRTUAL_ADDRESSING */

}

#else
static inline
void shmem_transport_ofi_get_mr(const void *addr, int dest_pe,
                                uint8_t **mr_addr, uint64_t *key) {
    if ((void*) addr >= shmem_internal_data_base &&
        (uint8_t*) addr < (uint8_t*) shmem_internal_data_base + shmem_internal_data_length) {
        *key = shmem_transport_ofi_target_data_keys[dest_pe];
#ifdef ENABLE_REMOTE_VIRTUAL_ADDRESSING
        *mr_addr = (uint8_t *) addr;
#else
        *mr_addr = shmem_transport_ofi_target_data_addrs[dest_pe] +
            ((uint8_t *) addr - (uint8_t *) shmem_internal_data_base);
#endif
    }

    else if ((void*) addr >= shmem_internal_heap_base &&
             (uint8_t*) addr < (uint8_t*) shmem_internal_heap_base + shmem_internal_heap_length) {
        *key = shmem_transport_ofi_target_heap_keys[dest_pe];
#ifdef ENABLE_REMOTE_VIRTUAL_ADDRESSING
        *mr_addr = (uint8_t *) addr;
#else
        *mr_addr = shmem_transport_ofi_target_heap_addrs[dest_pe] +
            ((uint8_t *) addr - (uint8_t *) shmem_internal_heap_base);
#endif
    }

    else {
        *key = -1;
        *mr_addr = NULL;
        RAISE_ERROR_MSG("address (%p) outside of symmetric areas\n", addr);
    }
}
#endif

typedef enum fi_datatype shm_internal_datatype_t;
typedef enum fi_op       shm_internal_op_t;

/* Datatypes */
#define SHM_INTERNAL_FLOAT           FI_FLOAT
#define SHM_INTERNAL_DOUBLE          FI_DOUBLE
#define SHM_INTERNAL_LONG_DOUBLE     FI_LONG_DOUBLE
#define SHM_INTERNAL_FLOAT_COMPLEX   FI_FLOAT_COMPLEX
#define SHM_INTERNAL_DOUBLE_COMPLEX  FI_DOUBLE_COMPLEX
#define SHM_INTERNAL_SIGNED_BYTE     FI_INT8
#define SHM_INTERNAL_INT8            FI_INT8
#define SHM_INTERNAL_INT16           FI_INT16
#define SHM_INTERNAL_INT32           FI_INT32
#define SHM_INTERNAL_INT64           FI_INT64
#define SHM_INTERNAL_SHORT           DTYPE_SHORT
#define SHM_INTERNAL_INT             DTYPE_INT
#define SHM_INTERNAL_LONG            DTYPE_LONG
#define SHM_INTERNAL_LONG_LONG       DTYPE_LONG_LONG
#define SHM_INTERNAL_FORTRAN_INTEGER DTYPE_FORTRAN_INTEGER
#define SHM_INTERNAL_UINT            DTYPE_UNSIGNED_INT
#define SHM_INTERNAL_ULONG           DTYPE_UNSIGNED_LONG
#define SHM_INTERNAL_ULONG_LONG      DTYPE_UNSIGNED_LONG_LONG
#define SHM_INTERNAL_SIZE_T          DTYPE_SIZE_T
#define SHM_INTERNAL_PTRDIFF_T       DTYPE_PTRDIFF_T
#define SHM_INTERNAL_INT32           FI_INT32
#define SHM_INTERNAL_INT64           FI_INT64
#define SHM_INTERNAL_UINT32          FI_UINT32
#define SHM_INTERNAL_UINT64          FI_UINT64

/* Operations */
#define SHM_INTERNAL_BAND            FI_BAND
#define SHM_INTERNAL_BOR             FI_BOR
#define SHM_INTERNAL_BXOR            FI_BXOR
#define SHM_INTERNAL_MIN             FI_MIN
#define SHM_INTERNAL_MAX             FI_MAX
#define SHM_INTERNAL_SUM             FI_SUM
#define SHM_INTERNAL_PROD            FI_PROD

#define SHMEM_TRANSPORT_OFI_TYPE_BOUNCE 0x01
#define SHMEM_TRANSPORT_OFI_TYPE_LONG   0x02


extern fi_addr_t *addr_table;

#ifdef USE_AV_MAP
#define GET_DEST(dest) ((fi_addr_t)(addr_table[(dest)]))
#else
#define GET_DEST(dest) ((fi_addr_t)(dest))
#endif


struct shmem_transport_ofi_frag_t {
    shmem_free_list_item_t item;
    uint8_t mytype;
};

typedef struct shmem_transport_ofi_frag_t shmem_transport_ofi_frag_t;

struct shmem_transport_ofi_bounce_buffer_t {
    shmem_transport_ofi_frag_t frag;
    uint8_t data[];
};

typedef struct shmem_transport_ofi_bounce_buffer_t shmem_transport_ofi_bounce_buffer_t;

typedef int shmem_transport_ct_t;

struct shmem_transport_ctx_t {
  int                             id;
#ifdef USE_CTX_LOCK
  shmem_internal_mutex_t          lock;
#endif
  long                            options;
  struct fid_ep*                  cntr_ep;
  struct fid_ep*                  cq_ep;
  struct fid_stx*                 stx;
  struct fid_cntr*                put_cntr;
  struct fid_cntr*                get_cntr;
  struct fid_cq*                  cq;
  shmem_internal_atomic_uint64_t  pending_put_cntr;
  shmem_internal_atomic_uint64_t  pending_get_cntr;
  shmem_free_list_t              *bounce_buffers;
};

typedef struct shmem_transport_ctx_t shmem_transport_ctx_t;
extern shmem_transport_ctx_t shmem_transport_ctx_default;

#ifdef USE_CTX_LOCK
#define SHMEM_TRANSPORT_OFI_CTX_LOCK(ctx) SHMEM_MUTEX_LOCK((ctx)->lock);
#define SHMEM_TRANSPORT_OFI_CTX_UNLOCK(ctx) SHMEM_MUTEX_UNLOCK((ctx)->lock);
#else
#define SHMEM_TRANSPORT_OFI_CTX_LOCK(ctx)
#define SHMEM_TRANSPORT_OFI_CTX_UNLOCK(ctx)
#endif

int shmem_transport_ctx_create(long options, shmem_transport_ctx_t **ctx);
void shmem_transport_ctx_destroy(shmem_transport_ctx_t *ctx);

int shmem_transport_init(void);
int shmem_transport_startup(void);
int shmem_transport_fini(void);

extern size_t SHMEM_Dtsize[FI_DATATYPE_LAST];

static inline void shmem_transport_get_wait(shmem_transport_ctx_t* ctx);

/* Drain all available events from the CQ.  Note, ctx->bounce_buffers must be
 * locked before calling this routine */
static inline
void shmem_transport_ofi_drain_cq(shmem_transport_ctx_t *ctx)
{
    ssize_t ret = 0;
    struct fi_cq_entry buf;

    for (;;) {
        ret = fi_cq_read(ctx->cq, (void *)&buf, 1);

        if (ret == -FI_EAGAIN) break; /* No events */

        else if (ret == 1) {
            shmem_transport_ofi_frag_t *frag =
                (shmem_transport_ofi_frag_t *) buf.op_context;

            if (SHMEM_TRANSPORT_OFI_TYPE_BOUNCE == frag->mytype) {
                shmem_free_list_free(ctx->bounce_buffers,
                                     (shmem_transport_ofi_bounce_buffer_t *) frag);
            } else {
                RAISE_ERROR_STR("Unrecognized completion object");
            }
        }

        else if (ret < 0) {
            OFI_CTX_CHECK_ERROR(ctx, ret);
        }

        else {
            RAISE_ERROR_MSG("fi_cq_read returned invalid event count (%zd)\n", ret);
        }
    }
}

static inline
shmem_transport_ofi_bounce_buffer_t * create_bounce_buffer(shmem_transport_ctx_t *ctx,
                                                           const void *source,
                                                           const size_t len)
{
    shmem_transport_ofi_bounce_buffer_t *buff;

    shmem_free_list_lock(ctx->bounce_buffers);

    while (ctx->bounce_buffers->nalloc > shmem_transport_ofi_max_bounce_buffers) {
        shmem_transport_ofi_drain_cq(ctx);
    }

    buff = (shmem_transport_ofi_bounce_buffer_t*) shmem_free_list_alloc(ctx->bounce_buffers);

    shmem_free_list_unlock(ctx->bounce_buffers);

    if (NULL == buff)
        RAISE_ERROR_STR("Bounce buffer allocation failed");

    shmem_internal_assert(buff->frag.mytype == SHMEM_TRANSPORT_OFI_TYPE_BOUNCE);

    memcpy(buff->data, source, len);

    return buff;
}

static inline
void shmem_transport_put_quiet(shmem_transport_ctx_t* ctx)
{
    SHMEM_TRANSPORT_OFI_CTX_LOCK(ctx);

    /* Wait for bounce buffered operations to complete */
    if (ctx->cq_ep) {
        shmem_free_list_lock(ctx->bounce_buffers);

        while (ctx->bounce_buffers->nalloc > 0) {
            shmem_transport_ofi_drain_cq(ctx);
        }

        shmem_free_list_unlock(ctx->bounce_buffers);
    }

    /* wait for put counter to meet outstanding count value */

    /* Note: the communication routines increment pending put counters before
     * each FI call (thus before the corresponding event counter is
     * incremented), but the completion routine below reads the counters in the
     * reverse order: first the fid_cntr event counter, then the put issued
     * counter.  We'll want to preserve this property in the future.
     */
    uint64_t success, fail, cnt, cnt_new;
    long poll_count = 0;
    while (poll_count < shmem_transport_ofi_put_poll_limit ||
           shmem_transport_ofi_put_poll_limit < 0) {
        success = fi_cntr_read(ctx->put_cntr);
        fail = fi_cntr_readerr(ctx->put_cntr);
        cnt = shmem_internal_atomic_read(&ctx->pending_put_cntr);

        if (success < cnt && fail == 0) {
            SPINLOCK_BODY();
        } else if (fail) {
            RAISE_ERROR_MSG("Operations completed in error (%" PRIu64 ")\n", fail);
        } else {
            SHMEM_TRANSPORT_OFI_CTX_UNLOCK(ctx);
            return;
        }
        poll_count++;
    }
    cnt_new = shmem_internal_atomic_read(&ctx->pending_put_cntr);
    do {
        cnt = cnt_new;
        int ret = fi_cntr_wait(ctx->put_cntr, cnt, -1);
        cnt_new = shmem_internal_atomic_read(&ctx->pending_put_cntr);
        OFI_CTX_CHECK_ERROR(ctx, ret);
    } while (cnt < cnt_new);
    shmem_internal_assert(cnt == cnt_new);

    SHMEM_TRANSPORT_OFI_CTX_UNLOCK(ctx);
}

static inline
int shmem_transport_quiet(shmem_transport_ctx_t* ctx)
{

    shmem_transport_put_quiet(ctx);
    shmem_transport_get_wait(ctx);

    return 0;
}


static inline
int shmem_transport_fence(shmem_transport_ctx_t* ctx)
{
#if WANT_TOTAL_DATA_ORDERING == 0
    /*unordered network model*/
    return shmem_transport_quiet(ctx);
#else
    return 0;
#endif

}


/* Process RMA operation return code.  If libfabric returned -FI_EAGAIN, attempt
 * to reclaim resources and indicate that the operation should be retried.  If
 * retry limit (ofi_max_poll) is exceeded, abort. */
static inline
int try_again(shmem_transport_ctx_t *ctx, const int ret, uint64_t *polled) {

    if (ret) {
        if (ret == -FI_EAGAIN) {
            if (ctx->cq_ep) {
                shmem_free_list_lock(ctx->bounce_buffers);
                shmem_transport_ofi_drain_cq(ctx);
                shmem_free_list_unlock(ctx->bounce_buffers);
            }
            else if (0) {
                /* Poke CQ for errors to encourage progress */
                struct fi_cq_err_entry e = {0};
                ssize_t ret = fi_cq_readerr(ctx->cq, (void *)&e, 0);
                if (ret == 1) {
                    const char *errmsg = fi_cq_strerror(ctx->cq, e.prov_errno,
                                                        e.err_data, NULL, 0);
                    RAISE_ERROR_MSG("Error in operation: %s\n", errmsg);
                } else if (ret && ret != -FI_EAGAIN) {
                    RAISE_ERROR_MSG("Error reading from CQ (%zd)\n", ret);
                }
            }

            (*polled)++;

            if ((*polled) <= shmem_transport_ofi_max_poll) {
                return 1;
            }
            else {
                RAISE_ERROR_MSG("Operation retry limit exceeded (%" PRIu64 ")\n",
                                shmem_transport_ofi_max_poll);
            }
        }
        else {
            OFI_CTX_CHECK_ERROR(ctx, ret);
        }
    }

    return 0;
}


static inline
void shmem_transport_put_small(shmem_transport_ctx_t* ctx, void *target, const
                               void *source, size_t len, int pe)
{
    int ret = 0;
    uint64_t dst = (uint64_t) pe;
    uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;

    shmem_transport_ofi_get_mr(target, pe, &addr, &key);

    shmem_internal_assert(len <= shmem_transport_ofi_max_buffered_send);

    SHMEM_TRANSPORT_OFI_CTX_LOCK(ctx);
    shmem_internal_atomic_inc(&ctx->pending_put_cntr);

    do {

        ret = fi_inject_write(ctx->cntr_ep,
                              source,
                              len,
                              GET_DEST(dst),
                              (uint64_t) addr,
                              key);

    } while (try_again(ctx, ret, &polled));
    SHMEM_TRANSPORT_OFI_CTX_UNLOCK(ctx);
}

static inline
void shmem_transport_ofi_put_large(shmem_transport_ctx_t* ctx, void *target, const void *source,
                                   size_t len, int pe)
{
    int ret = 0;
    uint64_t dst = (uint64_t) pe;
    uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;

    shmem_transport_ofi_get_mr(target, pe, &addr, &key);

    uint8_t *frag_source = (uint8_t *) source;
    uint64_t frag_target = (uint64_t) addr;
    size_t frag_len = len;

    /* operation generates counting events and must be completed by
     * quiet. */
    SHMEM_TRANSPORT_OFI_CTX_LOCK(ctx);
    while (frag_source < ((uint8_t *) source) + len) {
        frag_len = MIN(shmem_transport_ofi_max_msg_size,
                       (size_t) (((uint8_t *) source) + len - frag_source));
        polled = 0;

        shmem_internal_atomic_inc(&ctx->pending_put_cntr);

        do {
            ret = fi_write(ctx->cntr_ep,
                           frag_source, frag_len, NULL,
                           GET_DEST(dst), frag_target,
                           key, NULL);
        } while (try_again(ctx, ret, &polled));

        frag_source += frag_len;
        frag_target += frag_len;
    }
    SHMEM_TRANSPORT_OFI_CTX_UNLOCK(ctx);
}

static inline
void shmem_transport_put_nb(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len,
                            int pe, long *completion)
{
    int ret = 0;
    uint64_t dst = (uint64_t) pe;
    uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;

    shmem_internal_assert(completion != NULL);

    if (len <= shmem_transport_ofi_max_buffered_send) {

        shmem_transport_put_small(ctx, target, source, len, pe);

    } else if (len <= shmem_transport_ofi_bounce_buffer_size && ctx->cq_ep) {

        SHMEM_TRANSPORT_OFI_CTX_LOCK(ctx);
        shmem_transport_ofi_get_mr(target, pe, &addr, &key);

        shmem_transport_ofi_bounce_buffer_t *buff =
            create_bounce_buffer(ctx, source, len);
        polled = 0;

        do {
            ret = fi_write(ctx->cq_ep,
                           buff->data, len, NULL,
                           GET_DEST(dst), (uint64_t) addr,
                           key, buff);
        } while (try_again(ctx, ret, &polled));
        SHMEM_TRANSPORT_OFI_CTX_UNLOCK(ctx);

    } else {
        shmem_transport_ofi_put_large(ctx, target, source,len, pe);
        (*completion)++;
    }
}

/* compatibility with Portals transport */
static inline
void shmem_transport_put_wait(shmem_transport_ctx_t* ctx, long *completion) {

    shmem_internal_assert((*completion) >= 0);

    if((*completion) > 0) {
        shmem_transport_put_quiet(ctx);
        (*completion)--;
    }
}

static inline
void shmem_transport_put_nbi(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len,
                             int pe)
{
    if (len <= shmem_transport_ofi_max_buffered_send) {

        shmem_transport_put_small(ctx, target, source, len, pe);

    } else {

        shmem_transport_ofi_put_large(ctx, target, source, len, pe);
    }
}


static inline
void shmem_transport_get(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len, int pe)
{
    int ret = 0;
    uint64_t dst = (uint64_t) pe;
    uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;

    shmem_transport_ofi_get_mr(source, pe, &addr, &key);

    SHMEM_TRANSPORT_OFI_CTX_LOCK(ctx);
    if (len <= shmem_transport_ofi_max_msg_size) {

        shmem_internal_atomic_inc(&ctx->pending_get_cntr);
        do {
            ret = fi_read(ctx->cntr_ep,
                          target,
                          len,
                          NULL,
                          GET_DEST(dst),
                          (uint64_t) addr,
                          key,
                          NULL);
        } while (try_again(ctx, ret, &polled));
    }
    else {
        uint8_t *frag_target = (uint8_t *) target;
        uint64_t frag_source = (uint64_t) addr;
        size_t frag_len = len;

        while (frag_target < ((uint8_t *) target) + len) {
            frag_len = MIN(shmem_transport_ofi_max_msg_size,
                           (size_t) (((uint8_t *) target) + len - frag_target));
            polled = 0;

            shmem_internal_atomic_inc(&ctx->pending_get_cntr);

            do {
                ret = fi_read(ctx->cntr_ep,
                              frag_target, frag_len, NULL,
                              GET_DEST(dst), frag_source,
                              key, NULL);
            } while (try_again(ctx, ret, &polled));

            frag_source += frag_len;
            frag_target += frag_len;
        }
    }
    SHMEM_TRANSPORT_OFI_CTX_UNLOCK(ctx);
}


static inline
void shmem_transport_get_wait(shmem_transport_ctx_t* ctx)
{
    /* wait for get counter to meet outstanding count value */

    /* Note: the communication routines increment pending get counters before
     * each FI call (thus before the corresponding event counter is
     * incremented), but the completion routine below reads the counters in the
     * reverse order: first the fid_cntr event counter, then the get issued
     * counter.  We'll want to preserve this property in the future.
     */
    uint64_t success, fail, cnt, cnt_new;
    long poll_count = 0;

    SHMEM_TRANSPORT_OFI_CTX_LOCK(ctx);

    while (poll_count < shmem_transport_ofi_get_poll_limit ||
           shmem_transport_ofi_get_poll_limit < 0) {
        success = fi_cntr_read(ctx->get_cntr);
        fail = fi_cntr_readerr(ctx->get_cntr);
        cnt = shmem_internal_atomic_read(&ctx->pending_get_cntr);

        if (success < cnt && fail == 0) {
            SPINLOCK_BODY();
        } else if (fail) {
            RAISE_ERROR_MSG("Operations completed in error (%" PRIu64 ")\n", fail);
        } else {
            SHMEM_TRANSPORT_OFI_CTX_UNLOCK(ctx);
            return;
        }
        poll_count++;
    }
    cnt_new = shmem_internal_atomic_read(&ctx->pending_get_cntr);
    do {
        cnt = cnt_new;
        int ret = fi_cntr_wait(ctx->get_cntr, cnt, -1);
        cnt_new = shmem_internal_atomic_read(&ctx->pending_get_cntr);
        OFI_CTX_CHECK_ERROR(ctx, ret);
    } while (cnt < cnt_new);
    shmem_internal_assert(cnt == cnt_new);

    SHMEM_TRANSPORT_OFI_CTX_UNLOCK(ctx);
}


static inline
void shmem_transport_swap(shmem_transport_ctx_t* ctx, void *target, const void *source, void *dest,
                          size_t len, int pe, int datatype)
{
    int ret = 0;
    uint64_t dst = (uint64_t) pe;
    uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;

    shmem_transport_ofi_get_mr(target, pe, &addr, &key);

    shmem_internal_assert(len <= sizeof(double _Complex));
    shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

    SHMEM_TRANSPORT_OFI_CTX_LOCK(ctx);
    shmem_internal_atomic_inc(&ctx->pending_get_cntr);

    do {
        ret = fi_fetch_atomic(ctx->cntr_ep,
                              source,
                              1,
                              NULL,
                              dest,
                              NULL,
                              GET_DEST(dst),
                              (uint64_t) addr,
                              key,
                              datatype,
                              FI_ATOMIC_WRITE,
                              NULL);
    } while (try_again(ctx, ret, &polled));
    SHMEM_TRANSPORT_OFI_CTX_UNLOCK(ctx);

}


static inline
void shmem_transport_cswap(shmem_transport_ctx_t* ctx, void *target, const void *source, void *dest,
                           const void *operand, size_t len, int pe, int datatype)
{
    int ret = 0;
    uint64_t dst = (uint64_t) pe;
    uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;

    shmem_transport_ofi_get_mr(target, pe, &addr, &key);

    shmem_internal_assert(len <= sizeof(double _Complex));
    shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

    shmem_internal_atomic_inc(&ctx->pending_get_cntr);

    do {
        ret = fi_compare_atomic(ctx->cntr_ep,
                                source,
                                1,
                                NULL,
                                operand,
                                NULL,
                                dest,
                                NULL,
                                GET_DEST(dst),
                                (uint64_t) addr,
                                key,
                                datatype,
                                FI_CSWAP,
                                NULL);
    } while (try_again(ctx, ret, &polled));
}


static inline
void shmem_transport_mswap(shmem_transport_ctx_t* ctx, void *target, const void *source, void *dest,
                           const void *mask, size_t len, int pe, int datatype)
{
    int ret = 0;
    uint64_t dst = (uint64_t) pe;
    uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;

    shmem_transport_ofi_get_mr(target, pe, &addr, &key);

    shmem_internal_assert(len <= sizeof(double _Complex));
    shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

    shmem_internal_atomic_inc(&ctx->pending_get_cntr);

    do {
        ret = fi_compare_atomic(ctx->cntr_ep,
                                source,
                                1,
                                NULL,
                                mask,
                                NULL,
                                dest,
                                NULL,
                                GET_DEST(dst),
                                (uint64_t) addr,
                                key,
                                datatype,
                                FI_MSWAP,
                                NULL);
    } while (try_again(ctx, ret, &polled));
}


static inline
void shmem_transport_atomic_small(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len,
                                  int pe, int op, int datatype)
{
    int ret = 0;
    uint64_t dst = (uint64_t) pe;
    uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;

    shmem_transport_ofi_get_mr(target, pe, &addr, &key);

    shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

    shmem_internal_atomic_inc(&ctx->pending_put_cntr);

    do {
        ret = fi_inject_atomic(ctx->cntr_ep,
                               source,
                               1,
                               GET_DEST(dst),
                               (uint64_t) addr,
                               key,
                               datatype,
                               op);
    } while (try_again(ctx, ret, &polled));
}


static inline
void shmem_transport_atomic_set(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len,
                                int pe, int datatype)
{
    int ret = 0;
    uint64_t dst = (uint64_t) pe;
    uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;

    shmem_transport_ofi_get_mr(target, pe, &addr, &key);

    shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

    shmem_internal_atomic_inc(&ctx->pending_put_cntr);
    do {
        ret = fi_inject_atomic(ctx->cntr_ep,
                               source,
                               1,
                               GET_DEST(dst),
                               (uint64_t) addr,
                               key,
                               datatype,
                               FI_ATOMIC_WRITE);
    } while (try_again(ctx, ret, &polled));
}


static inline
void shmem_transport_atomic_fetch(shmem_transport_ctx_t* ctx, void *target, const void *source, size_t len,
                                  int pe, int datatype)
{
    int ret = 0;
    uint64_t dst = (uint64_t) pe;
    uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;

    shmem_transport_ofi_get_mr(source, pe, &addr, &key);

    shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

    shmem_internal_atomic_inc(&ctx->pending_get_cntr);

    do {
        ret = fi_fetch_atomic(ctx->cntr_ep,
                              NULL,
                              1,
                              NULL,
                              (void *) target,
                              NULL,
                              GET_DEST(dst),
                              (uint64_t) addr,
                              key,
                              datatype,
                              FI_ATOMIC_READ,
                              NULL);
    } while (try_again(ctx, ret, &polled));
}


static inline
void shmem_transport_atomic_nb(shmem_transport_ctx_t* ctx, void *target, const void *source,
                               size_t full_len, int pe, int op, int datatype,
                               long *completion)
{
    int ret = 0;
    uint64_t dst = (uint64_t) pe;
    size_t len = full_len/SHMEM_Dtsize[datatype];
    uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;
    size_t max_atomic_size = 0;

    shmem_internal_assert(SHMEM_Dtsize[datatype] * len == full_len);

    ret = fi_atomicvalid(ctx->cntr_ep, datatype, op,
                         &max_atomic_size);
    max_atomic_size = max_atomic_size * SHMEM_Dtsize[datatype];
    if (max_atomic_size > shmem_transport_ofi_max_msg_size
        || ret || max_atomic_size == 0) {
        RAISE_ERROR_MSG("Atomic operation with datatype %d and op %d not supported\n",
                        datatype, op);
    }

    shmem_transport_ofi_get_mr(target, pe, &addr, &key);

    if ( full_len <= MIN(shmem_transport_ofi_max_buffered_send,
                         max_atomic_size)) {

        polled = 0;

        shmem_internal_atomic_inc(&ctx->pending_put_cntr);

        do {
            ret = fi_inject_atomic(ctx->cntr_ep,
                                   source,
                                   len,
                                   GET_DEST(dst),
                                   (uint64_t) addr,
                                   key,
                                   datatype,
                                   op);
        } while (try_again(ctx, ret, &polled));

    } else if (full_len <=
               MIN(shmem_transport_ofi_bounce_buffer_size, max_atomic_size) &&
               ctx->cq_ep) {

        shmem_transport_ofi_bounce_buffer_t *buff =
            create_bounce_buffer(ctx, source, full_len);

        polled = 0;

        do {
            ret = fi_atomic(ctx->cq_ep,
                            buff->data,
                            len,
                            NULL,
                            GET_DEST(dst),
                            (uint64_t) addr,
                            key,
                            datatype,
                            op,
                            buff);
        } while (try_again(ctx, ret, &polled));

    } else {
        size_t sent = 0;

        while (sent < len) {

            size_t chunksize = MIN((len-sent),
                                   (max_atomic_size/SHMEM_Dtsize[datatype]));
            polled = 0;
            shmem_internal_atomic_inc(&ctx->pending_put_cntr);
            do {
                ret = fi_atomic(ctx->cntr_ep,
                                (void *)((char *)source +
                                         (sent*SHMEM_Dtsize[datatype])),
                                chunksize,
                                NULL,
                                GET_DEST(dst),
                                ((uint64_t) addr +
                                 (sent*SHMEM_Dtsize[datatype])),
                                key,
                                datatype,
                                op,
                                NULL);
            } while (try_again(ctx, ret, &polled));

            sent += chunksize;
        }
    }
}


static inline
void shmem_transport_fetch_atomic(shmem_transport_ctx_t* ctx, void *target, const void *source, void *dest,
                                  size_t len, int pe, int op, int datatype)
{
    int ret = 0;
    uint64_t dst = (uint64_t) pe;
    uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;

    shmem_transport_ofi_get_mr(target, pe, &addr, &key);

    shmem_internal_assert(len <= sizeof(double _Complex));
    shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

    shmem_internal_atomic_inc(&ctx->pending_get_cntr);

    do {
        ret = fi_fetch_atomic(ctx->cntr_ep,
                              source,
                              1,
                              NULL,
                              dest,
                              NULL,
                              GET_DEST(dst),
                              (uint64_t) addr,
                              key,
                              datatype,
                              op,
                              NULL);
    } while (try_again(ctx, ret, &polled));
}


/* detect atomic limitation on the fly and provide software reduction support
   if needed */
static inline
int shmem_transport_atomic_supported(shm_internal_op_t op,
                                     shm_internal_datatype_t datatype)
{
    size_t size = 0;
    int ret = fi_atomicvalid(shmem_transport_ctx_default.cntr_ep,
                             datatype, op, &size);
    return !(ret != 0 || size == 0);
}


static inline
void shmem_transport_put_ct_nb(shmem_transport_ct_t *ct, void *target,
                               const void *source, size_t len, int pe,
                               long *completion)
{
    RAISE_ERROR_STR("OFI transport does not currently support CT operations");
}

static inline
void shmem_transport_get_ct(shmem_transport_ct_t *ct, void *target,
                            const void *source, size_t len, int pe)
{
    RAISE_ERROR_STR("OFI transport does not currently support CT operations");
}

static inline
void shmem_transport_ct_create(shmem_transport_ct_t **ct_ptr)
{
    RAISE_ERROR_STR("OFI transport does not currently support CT operations");
}

static inline
void shmem_transport_ct_free(shmem_transport_ct_t **ct_ptr)
{
    RAISE_ERROR_STR("OFI transport does not currently support CT operations");
}

static inline
long shmem_transport_ct_get(shmem_transport_ct_t *ct)
{
    RAISE_ERROR_STR("OFI transport does not currently support CT operations");
    return -1;
}

static inline
void shmem_transport_ct_set(shmem_transport_ct_t *ct, long value)
{
    RAISE_ERROR_STR("OFI transport does not currently support CT operations");
}

static inline
void shmem_transport_ct_wait(shmem_transport_ct_t *ct, long wait_for)
{
    RAISE_ERROR_STR("OFI transport does not currently support CT operations");
}

static inline
uint64_t shmem_transport_received_cntr_get(void)
{
#ifndef ENABLE_HARD_POLLING
    shmem_internal_assert(shmem_internal_thread_level == SHMEM_THREAD_SINGLE);
    return fi_cntr_read(shmem_transport_ofi_target_cntrfd);
#else
    RAISE_ERROR_STR("OFI transport configured for hard polling");
    return 0;
#endif
}

static inline
void shmem_transport_received_cntr_wait(uint64_t ge_val)
{
#ifndef ENABLE_HARD_POLLING
    shmem_internal_assert(shmem_internal_thread_level == SHMEM_THREAD_SINGLE);
    int ret = fi_cntr_wait(shmem_transport_ofi_target_cntrfd, ge_val, -1);

    OFI_CHECK_ERROR(ret);
#else
    RAISE_ERROR_STR("OFI transport configured for hard polling");
#endif
}

#endif /* TRANSPORT_OFI_H */
