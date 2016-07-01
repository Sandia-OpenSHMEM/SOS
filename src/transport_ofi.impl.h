#ifndef TRANSPORT_OFI_IMPL_H
#define TRANSPORT_OFI_IMPL_H

#include <stdio.h>
#include <stdlib.h>
#include <rdma/fi_errno.h>
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
#include <unistd.h>

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#define OFI_ERRMSG(...)                                                         \
    do {                                                                        \
        fprintf(stderr, __FILE__ ":%d: \n", __LINE__);                          \
        fprintf(stderr, __VA_ARGS__);                                           \
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

static inline void shmem_transport_ctx_quiet(shmemx_ctx_t c)
{
  int ret = 0;

  shmem_transport_ctx_t* ctx = shmem_transport_ofi_contexts[c];
  shem_transport_dom_t* dom = ctx->domain;

  if(dom->use_lock) {
    SHMEM_MUTEX_LOCK(dom->lock);
  }

  ret = fi_cntr_wait(ctx->endpoint->pending_count,
      ctx->endpoint->counter,-1);

  if(ret) {
    struct fi_cq_err_entry e = {0};
    fi_cq_readerr(ctx->domain->cq, (void *)&e, 0);
    RAISE_ERROR(e.err);
  }

  if(dom->use_lock) {
    SHMEM_MUTEX_UNLOCK(dom->lock);
  }
}

static inline
int shmem_transport_ctx_fence(shmemx_ctx_t ctx)
{
#if WANT_TOTAL_DATA_ORDERING == 0
  /*unordered network model*/
  return shmem_transport_ctx_quiet(ctx);
#else
  return 0;
#endif
}

/*RM requires polling until space is available*/
static inline int try_again(const int ret, uint64_t *polled) {

	if (ret) {
		if (ret == -FI_EAGAIN) {
			shmem_transport_ofi_drain_cq();
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
  shmem_transport_ctx_t* ctx = shmem_transport_ofi_contexts[c];
  shem_transport_dom_t* dom = ctx->domain;

  if(dom->use_lock) {
    SHMEM_MUTEX_LOCK(dom->lock);
  }

  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  shmem_internal_assert(len <= shmem_transport_ofi_max_buffered_send);

  do {

    ret = fi_inject_write(ctx->endpoint->ep,
        source,
        len,
        GET_DEST(dst),
        (uint64_t) addr,
        key);

  } while(try_again(ret,&polled));

  shmem_internal_assert(ret == 0);
  /* automatically get local completion but need remote completion for fence/quiet*/
  ctx->endpoint->pending_count++;

  if(dom->use_lock) {
    SHMEM_MUTEX_UNLOCK(dom->lock);
  }
}

static inline
void
shmem_transport_ofi_put_large(void *target, const void *source,
    size_t len, int pe, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = shmem_transport_ofi_contexts[c];
  shem_transport_dom_t* dom = ctx->domain;

  if(dom->use_lock) {
    SHMEM_MUTEX_LOCK(dom->lock);
  }

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
  while (frag_source < ((uint8_t *) source) + len) {
    frag_len = MIN(shmem_transport_ofi_max_msg_size,
        (size_t) (((uint8_t *) source) + len - frag_source));
    polled = 0;

    do {
      ret = fi_write(ctx->endpoint->ep,
          frag_source, frag_len, NULL,
          GET_DEST(dst), frag_target,
          key, NULL);
    } while (try_again(ret,&polled));

    ctx->endpoint->pending_count++;

    frag_source += frag_len;
    frag_target += frag_len;
  }

  if(dom->use_lock) {
    SHMEM_MUTEX_UNLOCK(dom->lock);
  }
}

static inline
void
shmem_transport_put_nb(void *target, const void *source, size_t len,
                       int pe, long *completion, shmemx_ctx_t c)
{
  if (len <= shmem_transport_ofi_max_buffered_send) {

    shmem_transport_put_small(target, source, len, pe, c);

  } else {
    shmem_transport_ofi_put_large(target, source,len, pe, c);
    (*completion)++;
  }
}

/*compatibility with Portals transport*/
static inline
void
shmem_transport_put_wait(long *completion) {

    if((*completion)-- > 0)
        shmem_transport_put_quiet();
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
  shmem_transport_ctx_t* ctx = shmem_transport_ofi_contexts[c];
  shem_transport_dom_t* dom = ctx->domain;

  if(dom->use_lock) {
    SHMEM_MUTEX_LOCK(dom->lock);
  }

  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(source, pe, &addr, &key);

  if (len <= shmem_transport_ofi_max_msg_size) {
    do {
      ret = fi_read(ctx->endpoint->ep,
          target,
          len,
          NULL,
          GET_DEST(dst),
          (uint64_t) addr,
          key,
          NULL);
    } while (try_again(ret,&polled));

    ctx->endpoint->pending_count++;
  } else {
    uint8_t *frag_target = (uint8_t *) target;
    uint64_t frag_source = (uint64_t) addr;
    size_t frag_len = len;

    while (frag_target < ((uint8_t *) target) + len) {
      frag_len = MIN(shmem_transport_ofi_max_msg_size,
          (size_t) (((uint8_t *) target) + len - frag_target));
      polled = 0;

      do {
        ret = fi_read(ctx->endpoint->ep,
            frag_target, frag_len, NULL,
            GET_DEST(dst), frag_source,
            key, NULL);
      } while (try_again(ret,&polled));

    ctx->endpoint->pending_count++;

      frag_source += frag_len;
      frag_target += frag_len;
    }
  }
  if(dom->use_lock) {
    SHMEM_MUTEX_UNLOCK(dom->lock);
  }
}


static inline
void
shmem_transport_get_wait(void)
{
	int ret = 0;

	/* wait for get counter to meet outstanding count value    */
	ret = fi_cntr_wait(shmem_transport_ofi_get_cntrfd,
			shmem_transport_ofi_pending_get_counter,-1);
    if(ret) {
	    struct fi_cq_err_entry e = {0};
        fi_cq_readerr(shmem_transport_ofi_put_nb_cqfd,
		           (void *)&e, 0);
		RAISE_ERROR(e.err);
    }
}


static inline
void
shmem_transport_swap(void *target, const void *source, void *dest,
                     size_t len, int pe, int datatype)
{
  shmem_transport_ctx_t* ctx = shmem_transport_ofi_contexts[c];
  shem_transport_dom_t* dom = ctx->domain;

  if(dom->use_lock) {
    SHMEM_MUTEX_LOCK(dom->lock);
  }

  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  shmem_internal_assert(len <= sizeof(double complex));
  shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

  do {
    ret = fi_fetch_atomic(ctx->endpoint->ep,
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
  } while(try_again(ret,&polled));

  ctx->endpoint->pending_count++;

  if(dom->use_lock) {
    SHMEM_MUTEX_UNLOCK(dom->lock);
  }
}


static inline
void
shmem_transport_cswap(void *target, const void *source, void *dest,
                      const void *operand, size_t len, int pe,
                      int datatype, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = shmem_transport_ofi_contexts[c];
  shem_transport_dom_t* dom = ctx->domain;

  if(dom->use_lock) {
    SHMEM_MUTEX_LOCK(dom->lock);
  }


  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  shmem_internal_assert(len <= sizeof(double complex));
  shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

  do {
    ret = fi_compare_atomic(ctx->endpoint->ep,
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
  } while(try_again(ret,&polled));

  ctx->endpoint->pending_count++;

  if(dom->use_lock) {
    SHMEM_MUTEX_UNLOCK(dom->lock);
  }
}


static inline
void
shmem_transport_mswap(void *target, const void *source, void *dest,
                      const void *mask, size_t len, int pe,
                      int datatype, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = shmem_transport_ofi_contexts[c];
  shem_transport_dom_t* dom = ctx->domain;

  if(dom->use_lock) {
    SHMEM_MUTEX_LOCK(dom->lock);
  }


  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  shmem_internal_assert(len <= sizeof(double complex));
  shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

  do {
    ret = fi_compare_atomic(ctx->endpoint->ep,
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
  } while(try_again(ret,&polled));


  ctx->endpoint->pending_count++;

  if(dom->use_lock) {
    SHMEM_MUTEX_UNLOCK(dom->lock);
  }
}


static inline
void
shmem_transport_atomic_small(void *target, const void *source,
    size_t len, int pe, int op, int datatype, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = shmem_transport_ofi_contexts[c];
  shem_transport_dom_t* dom = ctx->domain;

  if(dom->use_lock) {
    SHMEM_MUTEX_LOCK(dom->lock);
  }


  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  shmem_internal_assert(SHMEM_Dtsize[datatype] <= shmem_transport_ofi_max_atomic_size);
  shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

  do {
    ret = fi_inject_atomic(ctx->endpoint->ep,
        source,
        1,
        GET_DEST(dst),
        (uint64_t) addr,
        key,
        datatype,
        op);
  } while(try_again(ret,&polled));

  ctx->endpoint->pending_count++;

  if(dom->use_lock) {
    SHMEM_MUTEX_UNLOCK(dom->lock);
  }
}


static inline
void
shmem_transport_atomic_set(void *target, const void *source,
    size_t len, int pe, int datatype, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = shmem_transport_ofi_contexts[c];
  shem_transport_dom_t* dom = ctx->domain;

  if(dom->use_lock) {
    SHMEM_MUTEX_LOCK(dom->lock);
  }


  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  shmem_internal_assert(SHMEM_Dtsize[datatype] <= shmem_transport_ofi_max_atomic_size);
  shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

  do {
    ret = fi_inject_atomic(ctx->endpoint->ep,
        source,
        1,
        GET_DEST(dst),
        (uint64_t) addr,
        key,
        datatype,
        FI_ATOMIC_WRITE);
  } while (try_again(ret, &polled));

  ctx->endpoint->pending_count++;

  if(dom->use_lock) {
    SHMEM_MUTEX_UNLOCK(dom->lock);
  }
}


static inline
void
shmem_transport_atomic_fetch(void *target, const void *source,
    size_t len, int pe, int datatype, shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = shmem_transport_ofi_contexts[c];
  shem_transport_dom_t* dom = ctx->domain;

  if(dom->use_lock) {
    SHMEM_MUTEX_LOCK(dom->lock);
  }


  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(source, pe, &addr, &key);

  shmem_internal_assert(SHMEM_Dtsize[datatype] <= shmem_transport_ofi_max_atomic_size);
  shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

  do {
    ret = fi_fetch_atomic(ctx->endpoint->ep,
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
  } while (try_again(ret, &polled));


  ctx->endpoint->pending_count++;

  if(dom->use_lock) {
    SHMEM_MUTEX_UNLOCK(dom->lock);
  }
}


static inline
void
shmem_transport_atomic_nb(void *target, const void *source,
    size_t full_len, int pe, int op, int datatype, long *completion,
    shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = shmem_transport_ofi_contexts[c];
  shem_transport_dom_t* dom = ctx->domain;

  if(dom->use_lock) {
    SHMEM_MUTEX_LOCK(dom->lock);
  }

  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  size_t len = full_len/SHMEM_Dtsize[datatype];
  uint64_t polled = 0;
  uint64_t key;
  uint8_t *addr;

  shmem_internal_assert(SHMEM_Dtsize[datatype] * len == full_len);

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  if ( full_len <= shmem_transport_ofi_max_buffered_send) {

    polled = 0;

    do {
      ret = fi_inject_atomic(ctx->endpoint->ep,
          source,
          len, //count
          GET_DEST(dst),
          (uint64_t) addr,
          key,
          datatype,
          op);
    } while(try_again(ret,&polled));


    ctx->endpoint->pending_count++;

  } else {
    size_t sent = 0;

    while (sent < len) {

      size_t chunksize = MIN((len-sent),
          (shmem_transport_ofi_max_atomic_size/SHMEM_Dtsize[datatype]));

      polled = 0;
      do {
        ret = fi_atomic(ctx->endpoint->ep,
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
      } while(try_again(ret,&polled));

      ctx->endpoint->pending_count++;
      sent += chunksize;
    }
  }

  if(dom->use_lock) {
    SHMEM_MUTEX_UNLOCK(dom->lock);
  }
}


static inline
void
shmem_transport_fetch_atomic(void *target, const void *source, void *dest,
                             size_t len, int pe, int op, int datatype)
{
  shmem_transport_ctx_t* ctx = shmem_transport_ofi_contexts[c];
  shem_transport_dom_t* dom = ctx->domain;

  if(dom->use_lock) {
    SHMEM_MUTEX_LOCK(dom->lock);
  }

  int ret = 0;
  uint64_t dst = (uint64_t) pe;
  uint64_t polled = 0;
  uint64_t key;
  uint8_t *addr;

  shmem_transport_ofi_get_mr(target, pe, &addr, &key);

  shmem_internal_assert(len <= sizeof(double complex));
  shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

  do {
    ret = fi_fetch_atomic(ctx->endpoint->ep,
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
  } while(try_again(ret,&polled));


  ctx->endpoint->pending_count++;

  if(dom->use_lock) {
    SHMEM_MUTEX_UNLOCK(dom->lock);
  }
}


static inline
int shmem_transport_atomic_supported(shm_internal_op_t op,
                                     shm_internal_datatype_t datatype)
{
    return datatype != SHM_INTERNAL_LONG_DOUBLE || shmem_transport_have_long_double;
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
    int ret = fi_cntr_wait(shmem_transport_ofi_target_cntrfd, ge_val, -1);

    if (ret) {
        RAISE_ERROR(ret);
    }
#else
    RAISE_ERROR_STR("OFI transport configured for hard polling");
#endif
}

#endif /* TRANSPORT_OFI_IMPL_H */

