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

struct shmem_transport_ofi_bounce_buffer_t {
    shmem_transport_ofi_frag_t frag;
    uint8_t data[];
};

typedef struct shmem_transport_ofi_bounce_buffer_t shmem_transport_ofi_bounce_buffer_t;

typedef int shmem_transport_ct_t;

typedef struct shmem_transport_cntr_ep_t {
    struct fid_cntr* counter;
    struct fid_ep* ep;
} shmem_transport_cntr_ep_t;

typedef struct shmem_transport_dom_t {
    struct fid_stx* stx;
    shmem_internal_mutex_t lock;
    int use_lock;
    /* Each endpoint is dedicated to 1 or more contexts. Sharing a
     * counter leads to performance degradation
     */
    shmem_transport_cntr_ep_t* endpoints;
    size_t num_endpoints;
    size_t max_num_endpoints;
    /* Completion queue for (non-blocking puts and) error reports.
     * Perhaps there should be one per endpoint?
     */
    struct fid_cq* cq;
    size_t num_active_contexts;
    /* Has shmem_domain_destroy been called on this? */
    int freed;
} shmem_transport_dom_t;

typedef struct shmem_transport_ctx_t {
    shmem_transport_dom_t* domain;
    shmem_transport_cntr_ep_t endpoint;
} shmem_transport_ctx_t;

extern shmem_transport_dom_t** shmem_transport_ofi_domains;
extern shmem_transport_ctx_t** shmem_transport_ofi_contexts;

extern size_t shmem_transport_num_contexts;
extern size_t shmem_transport_num_domains;

extern size_t shmem_transport_available_contexts;
extern size_t shmem_transport_available_domains;

extern struct fid_fabric*       	shmem_transport_ofi_fabfd;
extern struct fid_domain*          	shmem_transport_ofi_domainfd;
extern struct fid_ep*			shmem_transport_ofi_epfd;
extern struct fid_ep*			shmem_transport_ofi_cntr_epfd;
extern struct fid_stx*  		shmem_transport_ofi_stx;
extern struct fid_av*             	shmem_transport_ofi_avfd;
extern struct fid_cq*              	shmem_transport_ofi_put_nb_cqfd;
#ifndef ENABLE_HARD_POLLING
extern struct fid_cntr*            	shmem_transport_ofi_target_cntrfd;
#endif
extern struct fid_cntr*            	shmem_transport_ofi_put_cntrfd;
extern struct fid_cntr*            	shmem_transport_ofi_get_cntrfd;
#ifndef ENABLE_MR_SCALABLE
extern uint64_t*                        shmem_transport_ofi_target_heap_keys;
extern uint64_t*                        shmem_transport_ofi_target_data_keys;
#ifndef ENABLE_REMOTE_VIRTUAL_ADDRESSING
extern uint8_t**                       shmem_transport_ofi_target_heap_addrs;
extern uint8_t**                       shmem_transport_ofi_target_data_addrs;
#endif /* ENABLE_REMOTE_VIRTUAL_ADDRESSING */
#endif /* ENABLE_MR_SCALABLE */
extern uint64_t          		shmem_transport_ofi_pending_put_counter;
extern uint64_t 	       	 	shmem_transport_ofi_pending_get_counter;
extern uint64_t				shmem_transport_ofi_pending_cq_count;
extern uint64_t				shmem_transport_ofi_max_poll;
extern size_t          		 	shmem_transport_ofi_max_buffered_send;
extern size_t    		 	shmem_transport_ofi_max_atomic_size;
extern size_t    			shmem_transport_ofi_max_msg_size;
extern size_t    			shmem_transport_ofi_bounce_buffer_size;

extern fi_addr_t *addr_table;

extern int shmem_transport_have_long_double;

extern shmem_free_list_t *shmem_transport_ofi_bounce_buffers;

extern size_t SHMEM_Dtsize[FI_DATATYPE_LAST];

int shmem_transport_init(long eager_size);
int shmem_transport_startup(void);
int shmem_transport_fini(void);

static inline void shmem_transport_get_wait(void);


static inline
void shmem_transport_ofi_get_mr(const void *addr, int dest_pe,
                                uint8_t **mr_addr, uint64_t *key);
static inline
void shmem_transport_ofi_drain_cq(void);
static inline
shmem_transport_ofi_bounce_buffer_t*
    create_bounce_buffer(const void *source, const size_t len);

static inline
void shmem_transport_put_quiet(void);
static inline
int shmem_transport_quiet(void);
static inline
int shmem_transport_fence(void);
static inline
int try_again(const int ret, uint64_t *polled);

static inline
void shmem_transport_put_small(void *target, const void *source,
                               size_t len, int pe);
static inline
void shmem_transport_ofi_put_large(void *target, const void *source,
                                   size_t len, int pe);
static inline
void shmem_transport_put_nb(void *target, const void *source,
                            size_t len, int pe, long *completion);
static inline
void shmem_transport_put_wait(long *completion);
static inline
void shmem_transport_put_nbi(void *target, const void *source,
                             size_t len, int pe);

static inline
void shmem_transport_get(void *target, const void *source, size_t len,
                         int pe);
static inline
void shmem_transport_get_wait(void);

static inline
void shmem_transport_swap(void *target, const void *source,
                          void *dest, size_t len, int pe,
                          int datatype);
static inline
void shmem_transport_cswap(void *target, const void *source,
                           void *dest, const void *operand,
                           size_t len, int pe, int datatype);
static inline
void shmem_transport_mswap(void *target, const void *source,
                           void *dest, const void *mask, size_t len,
                           int pe, int datatype);

static inline
void shmem_transport_atomic_small(void *target, const void *source,
                                  size_t len, int pe, int op,
                                  int datatype);
static inline
void shmem_transport_atomic_set(void *target, const void *source,
                                size_t len, int pe, int datatype);
static inline
void shmem_transport_atomic_fetch(void *target, const void *source,
                                  size_t len, int pe, int datatype);
static inline
void shmem_transport_atomic_nb(void *target, const void *source,
                               size_t full_len, int pe, int op,
                               int datatype, long *completion);
static inline
void shmem_transport_fetch_atomic(void *target, const void *source,
                                  void *dest, size_t len, int pe,
                                  int op, int datatype);
static inline
int shmem_transport_atomic_supported(shm_internal_op_t op,
                                     shm_internal_datatype_t datatype);

static inline
void shmem_transport_put_ct_nb(shmem_transport_ct_t *ct, void *target,
                               const void *source, size_t len, int pe,
                               long *completion);
static inline
void shmem_transport_get_ct(shmem_transport_ct_t *ct, void *target,
                            const void *source, size_t len, int pe);
static inline
void shmem_transport_ct_create(shmem_transport_ct_t **ct_ptr);
static inline
void shmem_transport_ct_free(shmem_transport_ct_t **ct_ptr);
static inline
long shmem_transport_ct_get(shmem_transport_ct_t *ct);
static inline
void shmem_transport_ct_set(shmem_transport_ct_t *ct, long value);
static inline
void shmem_transport_ct_wait(shmem_transport_ct_t *ct, long wait_for);
static inline
uint64_t shmem_transport_received_cntr_get(void);
static inline
void shmem_transport_received_cntr_wait(uint64_t ge_val);

#include "transport_ofi.impl.h"

#endif /* TRANSPORT_OFI_H */
