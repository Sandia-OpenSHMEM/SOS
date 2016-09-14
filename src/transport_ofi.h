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
extern size_t				shmem_transport_ofi_max_atomic_size;
extern uint64_t				shmem_transport_ofi_max_poll;
extern size_t          		 	shmem_transport_ofi_max_buffered_send;
extern size_t    		 	shmem_transport_ofi_max_atomic_size;
extern size_t    			shmem_transport_ofi_max_msg_size;

extern fi_addr_t *addr_table;


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

#include "transport_ofi.impl.h"

#endif /* TRANSPORT_OFI_H */
