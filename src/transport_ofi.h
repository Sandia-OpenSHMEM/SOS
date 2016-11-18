/*
 *
 *  Copyright (c) 2015 Intel Corporation. All rights reserved.
 *  This software is available to you under the BSD license. For
 *  license information, see the LICENSE file in the top level directory.
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
#include "shmem_free_list.h"
#include <string.h>
#include "shmem_internal.h"
#include "shmemx.h"
#include <unistd.h>
#include <rdma/fabric.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_endpoint.h>
#include <stddef.h>

typedef enum fi_datatype shm_internal_datatype_t;
typedef enum fi_op       shm_internal_op_t;

// Datatypes
#define SHM_INTERNAL_FLOAT           FI_FLOAT
#define SHM_INTERNAL_DOUBLE          FI_DOUBLE
#define SHM_INTERNAL_LONG_DOUBLE     FI_LONG_DOUBLE
#define SHM_INTERNAL_FLOAT_COMPLEX   FI_FLOAT_COMPLEX
#define SHM_INTERNAL_DOUBLE_COMPLEX  FI_DOUBLE_COMPLEX
#define SHM_INTERNAL_SIGNED_BYTE     FI_INT8
#define SHM_INTERNAL_INT32           FI_INT32
#define SHM_INTERNAL_INT64           FI_INT64
#define SHM_INTERNAL_SHORT           DTYPE_SHORT
#define SHM_INTERNAL_INT             DTYPE_INT
#define SHM_INTERNAL_LONG            DTYPE_LONG
#define SHM_INTERNAL_LONG_LONG       DTYPE_LONG_LONG
#define SHM_INTERNAL_FORTRAN_INTEGER DTYPE_FORTRAN_INTEGER

 // Operations
#define SHM_INTERNAL_BAND            FI_BAND
#define SHM_INTERNAL_BOR             FI_BOR
#define SHM_INTERNAL_BXOR            FI_BXOR
#define SHM_INTERNAL_MIN             FI_MIN
#define SHM_INTERNAL_MAX             FI_MAX
#define SHM_INTERNAL_SUM             FI_SUM
#define SHM_INTERNAL_PROD            FI_PROD

#define SHMEM_TRANSPORT_OFI_TYPE_BOUNCE 0x01
#define SHMEM_TRANSPORT_OFI_TYPE_LONG   0x02

typedef struct fi_context fi_context_t;

struct shmem_transport_ofi_frag_t {
    shmem_free_list_item_t item;
    uint8_t mytype;
    fi_context_t context;
};

typedef struct shmem_transport_ofi_frag_t shmem_transport_ofi_frag_t;

typedef int shmem_transport_ct_t;

typedef struct shmem_transport_cntr_ep_t {
  struct fid_cntr* counter;
  struct fid_ep* ep;
  uint64_t pending_count;
} shmem_transport_cntr_ep_t;

typedef struct shmem_transport_dom_t {
  int id;
  struct fid_stx* stx;
  shmem_internal_mutex_t lock;
  void (*take_lock)(struct shmem_transport_dom_t**);
  void (*release_lock)(struct shmem_transport_dom_t**);
  void (*free_lock)(struct shmem_transport_dom_t**);
  /* Each endpoint is dedicated to 1 or more contexts. Sharing a
   * counter leads to performance degradation
   */
  /* Completion queue for (non-blocking puts and) error reports.
   * Perhaps there should be one per endpoint?
   */
  struct fid_cq* cq;
  struct fid_ep* cq_ep;
  size_t num_active_contexts;
  /* Has shmem_domain_destroy been called on this? */
  int freed;
} shmem_transport_dom_t;

static inline void dom_lock_noop(shmem_transport_dom_t** dom) {}
static inline void dom_take_mutex(shmem_transport_dom_t** dom) {
  SHMEM_MUTEX_LOCK((*dom)->lock);
}
static inline void dom_release_mutex(shmem_transport_dom_t** dom) {
  SHMEM_MUTEX_UNLOCK((*dom)->lock);
}
static inline void dom_free_mutex(shmem_transport_dom_t** dom) {
  SHMEM_MUTEX_DESTROY((*dom)->lock);
}

typedef struct shmem_transport_ctx_t {
  shmem_transport_dom_t* domain;
  shmem_transport_cntr_ep_t endpoint;

  void (*take_lock)(shmem_transport_dom_t**);
  void (*release_lock)(shmem_transport_dom_t**);

  size_t id;
} shmem_transport_ctx_t;


#define TRANSP_FROM_DOM_T(domid) ((shmem_transport_dom_t*)domid)
#define TRANSP_FROM_CTX_T(ctxid) ((shmem_transport_ctx_t*)ctxid)

extern shmemx_domain_t shmem_transport_default_dom;
extern shmemx_ctx_t shmem_transport_default_ctx;
extern shmem_transport_dom_t* shmem_transport_dom;
extern shmem_transport_ctx_t* shmem_transport_ctx;

extern shmem_transport_dom_t** shmem_transport_ofi_domains;
extern shmem_transport_ctx_t** shmem_transport_ofi_contexts;

extern size_t shmem_transport_num_contexts;
extern size_t shmem_transport_num_domains;

extern size_t shmem_transport_available_contexts;
extern size_t shmem_transport_available_domains;

extern struct fid_fabric*       	shmem_transport_ofi_fabfd;
extern struct fid_domain*          	shmem_transport_ofi_domainfd;
extern struct fid_av*             	shmem_transport_ofi_avfd;
#ifndef ENABLE_HARD_POLLING
extern struct fid_cntr*            	shmem_transport_ofi_target_cntrfd;
#endif
#ifndef ENABLE_MR_SCALABLE
extern uint64_t*                        shmem_transport_ofi_target_heap_keys;
extern uint64_t*                        shmem_transport_ofi_target_data_keys;
#ifndef ENABLE_REMOTE_VIRTUAL_ADDRESSING
extern uint8_t**                       shmem_transport_ofi_target_heap_addrs;
extern uint8_t**                       shmem_transport_ofi_target_data_addrs;
#endif /* ENABLE_REMOTE_VIRTUAL_ADDRESSING */
#endif /* ENABLE_MR_SCALABLE */
extern uint64_t				shmem_transport_ofi_max_poll;
extern size_t          		 	shmem_transport_ofi_max_buffered_send;
extern size_t    			shmem_transport_ofi_max_msg_size;

extern fi_addr_t *addr_table;
extern size_t    			shmem_transport_ofi_bounce_buffer_size;
#ifdef ENABLE_THREADS
extern shmem_internal_mutex_t           shmem_transport_ofi_lock;
#endif


int shmem_transport_init(int thread_level,long eager_size);
int shmem_transport_startup(void);
void shmem_transport_print_info(void);
int shmem_transport_fini(void);

extern size_t SHMEM_Dtsize[FI_DATATYPE_LAST];

static inline
void shmem_transport_received_cntr_wait(uint64_t ge_val);

static inline
uint64_t shmem_transport_received_cntr_get(void);

static inline
void shmem_transport_ct_wait(shmem_transport_ct_t *ct, long wait_for);

static inline
void shmem_transport_ct_set(shmem_transport_ct_t *ct, long value);

static inline
long shmem_transport_ct_get(shmem_transport_ct_t *ct);

static inline
void shmem_transport_ct_free(shmem_transport_ct_t **ct_ptr);

static inline
void shmem_transport_ct_create(shmem_transport_ct_t **ct_ptr);

static inline
void shmem_transport_get_ct(shmem_transport_ct_t *ct, void *target,
                                const void *source, size_t len, int pe);

static inline
void
shmem_transport_put_ct_nb(shmem_transport_ct_t *ct, void *target,
                              const void *source, size_t len, int pe, long *completion);

static inline
int shmem_transport_atomic_supported(shm_internal_op_t op,
                                     shm_internal_datatype_t datatype);

static inline
void
shmem_transport_fetch_atomic(void *target, const void *source, void *dest,
    size_t len, int pe, int op, int datatype, shmemx_ctx_t c);

static inline
void
shmem_transport_atomic_nb(void *target, const void *source,
    size_t full_len, int pe, int op, int datatype, shmemx_ctx_t c);

static inline
void
shmem_transport_atomic_fetch(void *target, const void *source,
    size_t len, int pe, int datatype, shmemx_ctx_t c);

static inline
void
shmem_transport_atomic_set(void *target, const void *source,
    size_t len, int pe, int datatype, shmemx_ctx_t c);

static inline
void
shmem_transport_atomic_small(void *target, const void *source,
    size_t len, int pe, int op, int datatype, shmemx_ctx_t c);

static inline
void
shmem_transport_mswap(void *target, const void *source, void *dest,
                      const void *mask, size_t len, int pe,
                      int datatype, shmemx_ctx_t c);

static inline
void
shmem_transport_cswap(void *target, const void *source, void *dest,
                      const void *operand, size_t len, int pe,
                      int datatype, shmemx_ctx_t c);

static inline
void
shmem_transport_swap(void *target, const void *source, void *dest,
    size_t len, int pe, int datatype, shmemx_ctx_t c);

static inline
void
shmem_transport_get(void *target, const void *source, size_t len,
    int pe, shmemx_ctx_t c);

static inline
void
shmem_transport_put_nbi(void *target, const void *source, size_t len,
    int pe, shmemx_ctx_t c);

static inline
void
shmem_transport_put(void *target, const void *source, size_t len,
                       int pe, shmemx_ctx_t c);

static inline
void
shmem_transport_fetch_atomic(void *target, const void *source, void *dest,
    size_t len, int pe, int op, int datatype, shmemx_ctx_t c);


/* detect atomic limitation on the fly and provide software reduction support
    if needed */
static inline
int shmem_transport_atomic_supported(shm_internal_op_t op,
                                     shm_internal_datatype_t datatype);

static inline
void
shmem_transport_ofi_put_large(void *target, const void *source,
    size_t len, int pe, shmemx_ctx_t c);

static inline
void
shmem_transport_put_small(void *target, const void *source,
    size_t len, int pe, shmemx_ctx_t c);

static inline
void shmem_transport_ctx_fence(shmem_transport_ctx_t* ctx);

static inline void shmem_transport_ctx_quiet(shmem_transport_ctx_t* ctx);

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#define OFI_ERRMSG(...)                                                    \
    do {                                                                        \
        fprintf(stderr, "%s:%d: \n", __FILE__, __LINE__);                       \
        fprintf(stderr, __VA_ARGS__);                                      \
    } while (0)

#define OFI_RET_CHECK(ret)                                                      \
    do {                                                                        \
        if (ret) {                                                              \
            fprintf(stderr, "OFI error #%d: %s \n", ret, fi_strerror(ret));     \
            RAISE_ERROR(ret);                                                   \
        }                                                                       \
    } while (0)

#ifdef USE_AV_MAP
#define GET_DEST(dest) ((fi_addr_t)(addr_table[(dest)]))
#else
#define GET_DEST(dest) ((fi_addr_t)(dest))
#endif

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
        OFI_ERRMSG("[%03d] ERROR in %s: address (0x%p) outside of symmetric areas\n",
               shmem_internal_my_pe, __func__, addr);
        RAISE_ERROR(1);
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
        *mr_addr = NULL;
        OFI_ERRMSG("[%03d] ERROR in %s: address (0x%p) outside of symmetric areas\n",
               shmem_internal_my_pe, __func__, addr);
        RAISE_ERROR(1);
    }
}
#endif


/* assumes the associated domain is already locked if necessary */
static inline void shmem_transport_ctx_quiet(shmem_transport_ctx_t* ctx)
{
  int ret = 0;

  ret = fi_cntr_wait(ctx->endpoint.counter,
      ctx->endpoint.pending_count, -1);

  if(ret) {
    struct fi_cq_err_entry e = {0};
    fi_cq_readerr(ctx->domain->cq, (void *)&e, 0);
    RAISE_ERROR(e.err);
  }
}

static inline void shmem_transport_ctx_drain(shmem_transport_ctx_t* ctx)
{
  int ret = 0;

  uint64_t cnt = fi_cntr_read(ctx->endpoint.counter);
  if(cnt < ctx->endpoint.pending_count) {
    ret = fi_cntr_wait(ctx->endpoint.counter,
        cnt+1, -1);
  }

  if(ret) {
    struct fi_cq_err_entry e = {0};
    fi_cq_readerr(ctx->domain->cq, (void *)&e, 0);
    RAISE_ERROR(e.err);
  }
}

static inline
void shmem_transport_ctx_fence(shmem_transport_ctx_t* ctx)
{
#if WANT_TOTAL_DATA_ORDERING == 0
  /*unordered network model*/
  shmem_transport_ctx_quiet(ctx);
/* #else */
/*   return 0; */
#endif
}

/*RM requires polling until space is available*/
static inline int try_again(const int ret, uint64_t *polled,
    shmem_transport_ctx_t* ctx)
{

  if (ret) {
    if (ret == -FI_EAGAIN) {
      shmem_transport_ctx_drain(ctx);
      (*polled)++;
      if ((*polled) <= shmem_transport_ofi_max_poll) {
        return 1;
      }
    }
    OFI_RET_CHECK(ret);
  }

  return 0;
}

static inline
void
shmem_transport_put_small(void *target, const void *source,
    size_t len, int pe, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = TRANSP_FROM_CTX_T(c);
  /* printf("put_small to %d (len %lu) with %d\n",pe,len,c); */

  ctx->take_lock((shmem_transport_dom_t**)ctx);

  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key = 0;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  shmem_internal_assert(len <= shmem_transport_ofi_max_buffered_send);

  do {

    ret = fi_inject_write(ctx->endpoint.ep,
        source,
        len,
        GET_DEST(dst),
        (uint64_t) addr,
        key);

  } while(try_again(ret, &polled, ctx));

  shmem_internal_assert(ret == 0);
  /* automatically get local completion but need remote completion for fence/quiet*/
  ctx->endpoint.pending_count++;

  ctx->release_lock((shmem_transport_dom_t**)ctx);
}

<<<<<<< HEAD
static inline
void
shmem_transport_ofi_put_large(void *target, const void *source,
    size_t len, int pe, shmemx_ctx_t c)
>>>>>>> SOS/master
{
  shmem_transport_ctx_t* ctx = TRANSP_FROM_CTX_T(c);

  ctx->take_lock((shmem_transport_dom_t**)ctx);

  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key = 0;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  uint8_t *frag_source = (uint8_t *) source;
  uint64_t frag_target = (uint64_t) addr;
  size_t frag_len = len;

  /* operation generates counting events and must be completed by
   * quiet. */
  while (frag_source < ((uint8_t *) source) + len) {
    frag_len = MIN(shmem_transport_ofi_max_msg_size,
        (size_t) (((uint8_t *) source) + len - frag_source));
    polled = 0;

    do {
      ret = fi_write(ctx->endpoint.ep,
          frag_source, frag_len, NULL,
          GET_DEST(dst), frag_target,
          key, NULL);
    } while (try_again(ret, &polled, ctx));

    ctx->endpoint.pending_count++;

    frag_source += frag_len;
    frag_target += frag_len;
  }

  ctx->release_lock((shmem_transport_dom_t**)ctx);
}

static inline
void
shmem_transport_put(void *target, const void *source, size_t len,
                       int pe, shmemx_ctx_t c)
{
  if (len <= shmem_transport_ofi_max_buffered_send) {

    shmem_transport_put_small(target, source, len, pe, c);

  } else {
    shmem_transport_ofi_put_large(target, source,len, pe, c);
  }
}

static inline
void
shmem_transport_put_nbi(void *target, const void *source, size_t len,
    int pe, shmemx_ctx_t c)
{
  if (len <= shmem_transport_ofi_max_buffered_send) {

    shmem_transport_put_small(target, source, len, pe, c);

  } else {

    shmem_transport_ofi_put_large(target, source, len, pe, c);
  }
}


static inline
void
shmem_transport_get(void *target, const void *source, size_t len,
    int pe, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = TRANSP_FROM_CTX_T(c);

  ctx->take_lock((shmem_transport_dom_t**)ctx);

  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key = 0;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(source, pe, &addr, &key);

  if (len <= shmem_transport_ofi_max_msg_size) {
    do {
      ret = fi_read(ctx->endpoint.ep,
          target,
          len,
          NULL,
          GET_DEST(dst),
          (uint64_t) addr,
          key,
          NULL);
    } while (try_again(ret, &polled, ctx));

    ctx->endpoint.pending_count++;
  } else {
    uint8_t *frag_target = (uint8_t *) target;
    uint64_t frag_source = (uint64_t) addr;
    size_t frag_len = len;

    while (frag_target < ((uint8_t *) target) + len) {
      frag_len = MIN(shmem_transport_ofi_max_msg_size,
          (size_t) (((uint8_t *) target) + len - frag_target));
      polled = 0;

      do {
        ret = fi_read(ctx->endpoint.ep,
            frag_target, frag_len, NULL,
            GET_DEST(dst), frag_source,
            key, NULL);
      } while (try_again(ret, &polled, ctx));

    ctx->endpoint.pending_count++;

      frag_source += frag_len;
      frag_target += frag_len;
    }
  }
  ctx->release_lock((shmem_transport_dom_t**)ctx);
}

static inline
void
shmem_transport_swap(void *target, const void *source, void *dest,
                     size_t len, int pe, int datatype, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = TRANSP_FROM_CTX_T(c);

  ctx->take_lock((shmem_transport_dom_t**)ctx);

  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key = 0;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  shmem_internal_assert(len <= sizeof(double complex));
  shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

  do {
    ret = fi_fetch_atomic(ctx->endpoint.ep,
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
  } while(try_again(ret, &polled, ctx));

  ctx->endpoint.pending_count++;

  ctx->release_lock((shmem_transport_dom_t**)ctx);
}


static inline
void
shmem_transport_cswap(void *target, const void *source, void *dest,
                      const void *operand, size_t len, int pe,
                      int datatype, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = TRANSP_FROM_CTX_T(c);

  ctx->take_lock((shmem_transport_dom_t**)ctx);


  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key = 0;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  shmem_internal_assert(len <= sizeof(double complex));
  shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

  do {
    ret = fi_compare_atomic(ctx->endpoint.ep,
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
  } while(try_again(ret, &polled, ctx));

  ctx->endpoint.pending_count++;

  ctx->release_lock((shmem_transport_dom_t**)ctx);
}


static inline
void
shmem_transport_mswap(void *target, const void *source, void *dest,
                      const void *mask, size_t len, int pe,
                      int datatype, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = TRANSP_FROM_CTX_T(c);

  ctx->take_lock((shmem_transport_dom_t**)ctx);

  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key = 0;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  shmem_internal_assert(len <= sizeof(double complex));
  shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

  do {
    ret = fi_compare_atomic(ctx->endpoint.ep,
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
  } while(try_again(ret, &polled, ctx));


  ctx->endpoint.pending_count++;

  ctx->release_lock((shmem_transport_dom_t**)ctx);
}


static inline
void
shmem_transport_atomic_small(void *target, const void *source,
    size_t len, int pe, int op, int datatype, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = TRANSP_FROM_CTX_T(c);

  ctx->take_lock((shmem_transport_dom_t**)ctx);


  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key = 0;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

  do {
    ret = fi_inject_atomic(ctx->endpoint.ep,
        source,
        1,
        GET_DEST(dst),
        (uint64_t) addr,
        key,
        datatype,
        op);
  } while(try_again(ret, &polled, ctx));

  ctx->endpoint.pending_count++;

  ctx->release_lock((shmem_transport_dom_t**)ctx);
}


static inline
void
shmem_transport_atomic_set(void *target, const void *source,
    size_t len, int pe, int datatype, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = TRANSP_FROM_CTX_T(c);

  ctx->take_lock((shmem_transport_dom_t**)ctx);


  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key = 0;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

  do {
    ret = fi_inject_atomic(ctx->endpoint.ep,
        source,
        1,
        GET_DEST(dst),
        (uint64_t) addr,
        key,
        datatype,
        FI_ATOMIC_WRITE);
  } while (try_again(ret, &polled, ctx));

  ctx->endpoint.pending_count++;

  ctx->release_lock((shmem_transport_dom_t**)ctx);
}


static inline
void
shmem_transport_atomic_fetch(void *target, const void *source,
    size_t len, int pe, int datatype, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = TRANSP_FROM_CTX_T(c);

  ctx->take_lock((shmem_transport_dom_t**)ctx);

	shmem_transport_ofi_pending_put_counter++;
  SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
}

  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key = 0;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(source, pe, &addr, &key);

  shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

  do {
    ret = fi_fetch_atomic(ctx->endpoint.ep,
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
  } while (try_again(ret, &polled, ctx));


  ctx->endpoint.pending_count++;

  ctx->release_lock((shmem_transport_dom_t**)ctx);
}


static inline
void
shmem_transport_atomic_nb(void *target, const void *source,
    size_t full_len, int pe, int op, int datatype, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = TRANSP_FROM_CTX_T(c);

  ctx->take_lock((shmem_transport_dom_t**)ctx);

  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  size_t len = full_len/SHMEM_Dtsize[datatype];
  uint64_t polled = 0;
  uint64_t key = 0;
  uint8_t *addr;
  size_t max_atomic_size = 0;

  shmem_internal_assert(SHMEM_Dtsize[datatype] * len == full_len);

  ret = fi_atomicvalid(shmem_transport_ctx->endpoint.ep, datatype, op,
      &max_atomic_size);
  max_atomic_size = max_atomic_size * SHMEM_Dtsize[datatype];
  if (max_atomic_size > shmem_transport_ofi_max_msg_size
        || ret || max_atomic_size == 0) {
    OFI_ERRMSG("atomic_nb error: datatype %d x op %d not supported\n",
        datatype, op);
    RAISE_ERROR(-1);
  }

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  if ( full_len <= MIN(shmem_transport_ofi_max_buffered_send, max_atomic_size)) {

    polled = 0;

    do {
      ret = fi_inject_atomic(ctx->endpoint.ep,
          source,
          len, //count
          GET_DEST(dst),
          (uint64_t) addr,
          key,
          datatype,
          op);
    } while(try_again(ret, &polled, ctx));


    ctx->endpoint.pending_count++;

  } else {
    size_t sent = 0;

    while (sent < len) {

      size_t chunksize = MIN((len-sent),
          (max_atomic_size/SHMEM_Dtsize[datatype]));

      polled = 0;
      do {
        ret = fi_atomic(ctx->endpoint.ep,
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
      } while(try_again(ret, &polled, ctx));

      ctx->endpoint.pending_count++;
      sent += chunksize;
    }
  }

  ctx->release_lock((shmem_transport_dom_t**)ctx);
}


static inline
void
shmem_transport_fetch_atomic(void *target, const void *source, void *dest,
    size_t len, int pe, int op, int datatype, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = TRANSP_FROM_CTX_T(c);

  ctx->take_lock((shmem_transport_dom_t**)ctx);

  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key = 0;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  shmem_internal_assert(len <= sizeof(double complex));
  shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

  do {
    ret = fi_fetch_atomic(ctx->endpoint.ep,
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
  } while(try_again(ret, &polled, ctx));


  ctx->endpoint.pending_count++;

  ctx->release_lock((shmem_transport_dom_t**)ctx);
}


static inline
int shmem_transport_atomic_supported(shm_internal_op_t op,
                                     shm_internal_datatype_t datatype)
{
   size_t size = 0;
   int ret = fi_atomicvalid(shmem_transport_ctx->endpoint.ep, datatype, op, &size);
   return !(ret != 0 || size == 0);
}


static inline
void
shmem_transport_put_ct_nb(shmem_transport_ct_t *ct, void *target,
                              const void *source, size_t len, int pe, long *completion)
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
  int ret =  fi_cntr_read(shmem_transport_ofi_target_cntrfd);

  return ret;
#else
  RAISE_ERROR_STR("OFI transport configured for hard polling");
  return 0;
#endif
}

static inline
void shmem_transport_received_cntr_wait(uint64_t ge_val)
{
#ifndef ENABLE_HARD_POLLING
  /* FIXME: assumes counter waiting is thread-safe */
  int ret = 0;
  ret = fi_cntr_wait(shmem_transport_ofi_target_cntrfd, ge_val, -1);

  if (ret) {
    RAISE_ERROR(ret);
  }
#else
  RAISE_ERROR_STR("OFI transport configured for hard polling");
#endif
}


#endif /* TRANSPORT_OFI_H */
