/*
 *
 *  Copyright (c) 2015 Intel Corporation. All rights reserved.
 *  This software is available to you under the BSD license. For
 *  license information, see the LICENSE file in the top level directory.
 *
 */

#include <errno.h>
#include "config.h"
#include <stdlib.h>
#include <sys/time.h>
#include <sys/param.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>

#if HAVE_FNMATCH_H
#include <fnmatch.h>
#else
#define fnmatch(P, S, F) strcmp(P, S)
#endif

#define SHMEM_INTERNAL_INCLUDE

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"
#include "transport_ofi.h"
#include <unistd.h>
#include "runtime.h"

#define IF_ERR_RETURN(ret,msg) do {                 \
    if(ret) {                                           \
        OFI_ERRMSG(msg " (%s:%d)\n",__FILE__,__LINE__); \
        return ret;                                     \
    }                                                   \
  } while(0)

#define IF_OFI_ERR_RETURN(RET,MSG) do {              \
    if(RET) {                                        \
        fprintf(stderr,"SHMEM Error %d (%s:%d): %s", \
                (RET),__FILE__,__LINE__,(MSG "\n")); \
        return RET;                                  \
    }                                                \
  } while(0)

struct fabric_info {
    struct fi_info *fabrics;
    struct fi_info *p_info;
    char *prov_name;
    char *fabric_name;
    char *domain_name;
    int npes;
};

shmemx_domain_t shmem_transport_default_dom;
shmemx_ctx_t shmem_transport_default_ctx;
shmem_transport_dom_t* shmem_transport_dom;
shmem_transport_ctx_t* shmem_transport_ctx;

shmemx_domain_t SHMEMX_CTX_DEFAULT;
shmemx_ctx_t SHMEMX_DOMAIN_DEFAULT;

shmem_transport_dom_t** shmem_transport_ofi_domains;
shmem_transport_ctx_t** shmem_transport_ofi_contexts;

size_t shmem_transport_num_contexts = 0;
size_t shmem_transport_num_domains = 0;

size_t shmem_transport_available_contexts = 0;
size_t shmem_transport_available_domains = 0;

struct fid_fabric*          	shmem_transport_ofi_fabfd;
struct fid_domain*          	shmem_transport_ofi_domainfd;
struct fid_av*             	shmem_transport_ofi_avfd;
#ifndef ENABLE_HARD_POLLING
struct fid_cntr*            	shmem_transport_ofi_target_cntrfd;
#endif
#ifdef ENABLE_MR_SCALABLE
#ifdef ENABLE_REMOTE_VIRTUAL_ADDRESSING
struct fid_mr*              	shmem_transport_ofi_target_mrfd;
#else  /* !ENABLE_REMOTE_VIRTUAL_ADDRESSING */
struct fid_mr*                  shmem_transport_ofi_target_heap_mrfd;
struct fid_mr*                  shmem_transport_ofi_target_data_mrfd;
#endif
#else  /* !ENABLE_MR_SCALABLE */
struct fid_mr*                  shmem_transport_ofi_target_heap_mrfd;
struct fid_mr*                  shmem_transport_ofi_target_data_mrfd;
uint64_t*                       shmem_transport_ofi_target_heap_keys;
uint64_t*                       shmem_transport_ofi_target_data_keys;
#ifndef ENABLE_REMOTE_VIRTUAL_ADDRESSING
uint8_t**                       shmem_transport_ofi_target_heap_addrs;
uint8_t**                       shmem_transport_ofi_target_data_addrs;
#endif /* ENABLE_REMOTE_VIRTUAL_ADDRESSING */
#endif /* ENABLE_MR_SCALABLE */
uint64_t			shmem_transport_ofi_max_poll;
size_t           		shmem_transport_ofi_max_buffered_send;
size_t    	 		shmem_transport_ofi_max_atomic_size;
size_t    			shmem_transport_ofi_max_msg_size;
size_t    			shmem_transport_ofi_addrlen;
fi_addr_t			*addr_table;
#ifdef USE_ON_NODE_COMMS
#define EPHOSTNAMELEN  _POSIX_HOST_NAME_MAX + 1
static char         myephostname[EPHOSTNAMELEN];
#endif

size_t SHMEM_Dtsize[FI_DATATYPE_LAST];

static char * SHMEM_DtName[FI_DATATYPE_LAST];
static char * SHMEM_OpName[FI_ATOMIC_OP_LAST];

static inline void init_ofi_tables(void)
{
  SHMEM_Dtsize[FI_INT8]                = sizeof(int8_t);
  SHMEM_Dtsize[FI_UINT8]               = sizeof(uint8_t);
  SHMEM_Dtsize[FI_INT16]               = sizeof(int16_t);
  SHMEM_Dtsize[FI_UINT16]              = sizeof(uint16_t);
  SHMEM_Dtsize[FI_INT32]               = sizeof(int32_t);
  SHMEM_Dtsize[FI_UINT32]              = sizeof(uint32_t);
  SHMEM_Dtsize[FI_INT64]               = sizeof(int64_t);
  SHMEM_Dtsize[FI_UINT64]              = sizeof(uint64_t);
  SHMEM_Dtsize[FI_FLOAT]               = sizeof(float);
  SHMEM_Dtsize[FI_DOUBLE]              = sizeof(double);
  SHMEM_Dtsize[FI_FLOAT_COMPLEX]       = sizeof(float complex);
  SHMEM_Dtsize[FI_DOUBLE_COMPLEX]      = sizeof(double complex);
  SHMEM_Dtsize[FI_LONG_DOUBLE]         = sizeof(long double);
  SHMEM_Dtsize[FI_LONG_DOUBLE_COMPLEX] = sizeof(long double complex);

  SHMEM_DtName[FI_INT8]                = "int8";
  SHMEM_DtName[FI_UINT8]               = "uint8";
  SHMEM_DtName[FI_INT16]               = "int16";
  SHMEM_DtName[FI_UINT16]              = "uint16";
  SHMEM_DtName[FI_INT32]               = "int32";
  SHMEM_DtName[FI_UINT32]              = "uint32";
  SHMEM_DtName[FI_INT64]               = "int64";
  SHMEM_DtName[FI_UINT64]              = "uint64";
  SHMEM_DtName[FI_FLOAT]               = "float";
  SHMEM_DtName[FI_DOUBLE]              = "double";
  SHMEM_DtName[FI_FLOAT_COMPLEX]       = "float complex";
  SHMEM_DtName[FI_DOUBLE_COMPLEX]      = "double complex";
  SHMEM_DtName[FI_LONG_DOUBLE]         = "long double";
  SHMEM_DtName[FI_LONG_DOUBLE_COMPLEX] = "long double complex";

  SHMEM_OpName[FI_MIN]                 = "MIN";
  SHMEM_OpName[FI_MAX]                 = "MAX";
  SHMEM_OpName[FI_SUM]                 = "SUM";
  SHMEM_OpName[FI_PROD]                = "PROD";
  SHMEM_OpName[FI_LOR]                 = "LOR";
  SHMEM_OpName[FI_LAND]                = "LAND";
  SHMEM_OpName[FI_BOR]                 = "BOR";
  SHMEM_OpName[FI_BAND]                = "BAND";
  SHMEM_OpName[FI_LXOR]                = "LXOR";
  SHMEM_OpName[FI_BXOR]                = "BXOR";
  SHMEM_OpName[FI_ATOMIC_READ]         = "ATOMIC_WRITE";
  SHMEM_OpName[FI_ATOMIC_WRITE]        = "ATOMIC_READ";
  SHMEM_OpName[FI_CSWAP]               = "CSWAP";
  SHMEM_OpName[FI_CSWAP_NE]            = "CSWAP_NE";
  SHMEM_OpName[FI_CSWAP_LE]            = "CSWAP_LE";
  SHMEM_OpName[FI_CSWAP_LT]            = "CSWAP_LT";
  SHMEM_OpName[FI_CSWAP_GE]            = "CSWAP_GE";
  SHMEM_OpName[FI_CSWAP_GT]            = "CSWAP_GT";
  SHMEM_OpName[FI_MSWAP]               = "MSWAP";
}

/* Cover OpenSHMEM atomics API */

#define SIZEOF_AMO_DT 5
static int DT_AMO_STANDARD[]=
{
  SHM_INTERNAL_INT, SHM_INTERNAL_LONG, SHM_INTERNAL_LONG_LONG,
  SHM_INTERNAL_INT32, SHM_INTERNAL_INT64
};
#define SIZEOF_AMO_OPS 1
static int AMO_STANDARD_OPS[]=
{
  SHM_INTERNAL_SUM
};
#define SIZEOF_AMO_FOPS 1
static int FETCH_AMO_STANDARD_OPS[]=
{
  SHM_INTERNAL_SUM
};
#define SIZEOF_AMO_COPS 1
static int COMPARE_AMO_STANDARD_OPS[]=
{
  FI_CSWAP
};

/* Note: Fortran-specific types should be last so they can be disabled here */
#ifdef ENABLE_FORTRAN
#define SIZEOF_AMO_EX_DT 8
#else
#define SIZEOF_AMO_EX_DT 7
#endif
static int DT_AMO_EXTENDED[]=
{
  SHM_INTERNAL_FLOAT, SHM_INTERNAL_DOUBLE, SHM_INTERNAL_INT, SHM_INTERNAL_LONG,
  SHM_INTERNAL_LONG_LONG, SHM_INTERNAL_INT32, SHM_INTERNAL_INT64,
  SHM_INTERNAL_FORTRAN_INTEGER
};
#define SIZEOF_AMO_EX_OPS 1
static int AMO_EXTENDED_OPS[]=
{
  FI_ATOMIC_WRITE
};
#define SIZEOF_AMO_EX_FOPS 2
static int FETCH_AMO_EXTENDED_OPS[]=
{
  FI_ATOMIC_WRITE, FI_ATOMIC_READ
};


/* Cover one-sided implementation of reduction */

#define SIZEOF_RED_DT 6
static int DT_REDUCE_BITWISE[]=
{
  SHM_INTERNAL_SHORT, SHM_INTERNAL_INT, SHM_INTERNAL_LONG,
  SHM_INTERNAL_LONG_LONG, SHM_INTERNAL_INT32, SHM_INTERNAL_INT64
};
#define SIZEOF_RED_OPS 3
static int REDUCE_BITWISE_OPS[]=
{
  SHM_INTERNAL_BAND, SHM_INTERNAL_BOR, SHM_INTERNAL_BXOR
};


#define SIZEOF_REDC_DT 9
static int DT_REDUCE_COMPARE[]=
{
  SHM_INTERNAL_FLOAT, SHM_INTERNAL_DOUBLE, SHM_INTERNAL_SHORT,
  SHM_INTERNAL_INT, SHM_INTERNAL_LONG, SHM_INTERNAL_LONG_LONG,
  SHM_INTERNAL_INT32, SHM_INTERNAL_INT64, SHM_INTERNAL_LONG_DOUBLE
};
#define SIZEOF_REDC_OPS 2
static int REDUCE_COMPARE_OPS[]=
{
  SHM_INTERNAL_MAX, SHM_INTERNAL_MIN
};


#define SIZEOF_REDA_DT 11
static int DT_REDUCE_ARITH[]=
{
  SHM_INTERNAL_FLOAT, SHM_INTERNAL_DOUBLE, SHM_INTERNAL_FLOAT_COMPLEX,
  SHM_INTERNAL_DOUBLE_COMPLEX, SHM_INTERNAL_SHORT, SHM_INTERNAL_INT,
  SHM_INTERNAL_LONG, SHM_INTERNAL_LONG_LONG, SHM_INTERNAL_INT32,
  SHM_INTERNAL_INT64, SHM_INTERNAL_LONG_DOUBLE
};
#define SIZEOF_REDA_OPS 2
static int REDUCE_ARITH_OPS[]=
{
  SHM_INTERNAL_SUM, SHM_INTERNAL_PROD
};

/* Internal to SHMEM implementation atomic requirement */
/*Locking implementation requirement */
#define SIZEOF_INTERNAL_REQ_DT 1
static int DT_INTERNAL_REQ[]=
{
  SHM_INTERNAL_INT
};
#define SIZEOF_INTERNAL_REQ_OPS 1
static int INTERNAL_REQ_OPS[]=
{
  FI_MSWAP
};

typedef enum{
    ATOMIC_NO_SUPPORT,
    ATOMIC_WARNINGS,
    ATOMIC_SOFT_SUPPORT,
}atomic_support_lv;


//size of CQ
const static size_t shmem_transport_ofi_queue_slots = 32768;//default CQ Depth....
size_t   shmem_transport_ofi_max_atomic_size = 0;
uint64_t shmem_transport_ofi_max_poll = (1ULL<<30);

#define OFI_MAJOR_VERSION 1
#define OFI_MINOR_VERSION 0

struct fabric_info shmem_ofi_cq_info = {0};
struct fabric_info shmem_ofi_cntr_info = {0};

int shmemx_domain_create(int thread_level, int num_domains,
        shmemx_domain_t domains[])
{
  if(thread_level > shmem_internal_thread_level) {
    fprintf(stderr,"Cannot create domain with thread level %d when "
        "global thread level is %d\n",thread_level,
        shmem_internal_thread_level);
    return -1;
  }
  int ret = 0;
  size_t oldSize = shmem_transport_num_domains;
  size_t newSize = oldSize + num_domains;
  if(newSize > shmem_transport_available_domains) {
    size_t nBytes = newSize*sizeof(shmem_transport_dom_t*);
    shmem_transport_dom_t** newBlock = malloc(nBytes);
    memcpy(newBlock,shmem_transport_ofi_domains,
        oldSize*sizeof(shmem_transport_dom_t*));

    /* FIXME: Probably need to do an atomic swap here, but idk the
     * right way to do that in C off the top of my head
     *
     * If this function is called while another thread is creating
     * contexts, you could get a use-after-free.
     */
    shmem_transport_dom_t** oldBlock = shmem_transport_ofi_domains;
    shmem_transport_ofi_domains = newBlock;
    free(oldBlock);
    shmem_transport_available_domains = newSize;
  }

  shmem_transport_dom_t** domArray = shmem_transport_ofi_domains;

  /* TODO: maybe block allocate here instead of mallocing each one.
   * That requires more bookkeeping though.
   */
  int i;
  for(i = 0; i < num_domains; ++i) {
    size_t ix = shmem_transport_num_domains++;
    shmem_transport_dom_t* dom;
    domArray[ix] = dom = malloc(sizeof(shmem_transport_dom_t));
    domains[i] = dom;

    dom->id = ix;
    dom->freed = 0;
    dom->num_active_contexts = 0;

    int lock = (thread_level != SHMEMX_THREAD_SINGLE);
    if(!lock) {
      dom->take_lock = &dom_lock_noop;
      dom->release_lock = &dom_lock_noop;
      dom->free_lock = &dom_lock_noop;
    } else {
      dom->take_lock = &dom_take_mutex;
      dom->release_lock = &dom_release_mutex;
      dom->free_lock = &dom_free_mutex;
    }

    if(lock) {
      /* Interesting semantic choice -- this macro is
       * pass-by-reference
       */
      SHMEM_MUTEX_INIT(dom->lock);
    }

    /* TODO: fill tx_attr */
    ret = fi_stx_context(shmem_transport_ofi_domainfd, NULL,
        &dom->stx, NULL);
    IF_OFI_ERR_RETURN(ret,"stx context initialization failed");

    struct fi_cq_attr   cq_attr = {0};
    /* event type for CQ,only context stored/reported */
    cq_attr.format = FI_CQ_FORMAT_CONTEXT;
    cq_attr.size   = shmem_transport_ofi_queue_slots;

    ret = fi_cq_open(shmem_transport_ofi_domainfd, &cq_attr,
        &dom->cq, NULL);
    IF_OFI_ERR_RETURN(ret,"cq_open failed");

    struct fabric_info* info = &shmem_ofi_cq_info;
    info->p_info->ep_attr->tx_ctx_cnt = FI_SHARED_CONTEXT;
    info->p_info->tx_attr->op_flags = FI_DELIVERY_COMPLETE;
    ret = fi_endpoint(shmem_transport_ofi_domainfd,
        info->p_info, &dom->cq_ep, NULL);
    IF_OFI_ERR_RETURN(ret,"domain epfd creation failed");

    ret = fi_ep_bind(dom->cq_ep, &dom->cq->fid,
        FI_SEND | FI_SELECTIVE_COMPLETION | FI_TRANSMIT);
    IF_OFI_ERR_RETURN(ret,"domain ep_bind ep -> cq failed");
    ret = fi_ep_bind(dom->cq_ep, &shmem_transport_ofi_avfd->fid, 0);
    IF_OFI_ERR_RETURN(ret,"domain ep_bind ep -> av failed");

    ret = fi_ep_bind(dom->cq_ep, &dom->stx->fid, 0);
    IF_OFI_ERR_RETURN(ret,"domain ep_bind ep -> stx failed");


    ret = fi_enable(dom->cq_ep);
    IF_OFI_ERR_RETURN(ret,"context enable endpoint");
  }

  return ret;
}

// assumes dom->lock is locked or dom->use_lock == 0
void shmem_transport_domain_destroy(shmem_transport_dom_t* dom)
{
  /* printf("Closing down\n"); */
  /* fflush(stdout); */
  /* FIXME: add to free list, maybe? */
  shmem_transport_ofi_domains[dom->id] = NULL;

  // The attached CQ is implicitly closed (as I discovered while
  // memory-checking with Valgrind). -jpdoyle
  if(fi_close(&dom->cq_ep->fid)) {
    OFI_ERRMSG("Domain CQ Endpoint close failed (%s)",
        fi_strerror(errno));
  }

  if(fi_close(&dom->stx->fid)) {
    OFI_ERRMSG("Domain STX close failed (%s)", fi_strerror(errno));
  }

  dom->release_lock(&dom);
  dom->free_lock(&dom);

  free(dom);
}

void shmemx_domain_destroy(int num_domains, shmemx_domain_t domains[])
{
  int i;

  for(i = 0; i < num_domains; ++i) {
    shmem_transport_dom_t* dom = TRANSP_FROM_DOM_T(domains[i]);

    dom->take_lock(&dom);
    if(!dom->num_active_contexts) {
      shmem_transport_domain_destroy(dom);
    } else {
      dom->freed = 1;
      dom->release_lock(&dom);
    }
  }
}

int shmemx_ctx_create(shmemx_domain_t domain, shmemx_ctx_t *ctx)
{
  /* FIXME: This does not do resource cleanup (or unlock the mutex on
   * `domain`) on error, it just returns. This is consistent with how
   * initialization worked before, but is worse since context creation
   * is not necessarily do-or-die.
   */
  int ret = 0;
  shmem_transport_dom_t* dom = TRANSP_FROM_DOM_T(domain);

  dom->take_lock(&dom);

  struct fi_cntr_attr cntr_attr = {0};
  cntr_attr.events   = FI_CNTR_EVENTS_COMP;

  struct fabric_info* info = &shmem_ofi_cntr_info;
  info->p_info->ep_attr->tx_ctx_cnt = FI_SHARED_CONTEXT;
  info->p_info->caps = FI_RMA | FI_WRITE | FI_READ | /*SEND ONLY */
                        FI_ATOMICS; /* request atomics capability */
  info->p_info->caps |= FI_REMOTE_WRITE | FI_REMOTE_READ;
  info->p_info->tx_attr->op_flags = FI_DELIVERY_COMPLETE | FI_INJECT_COMPLETE;
  info->p_info->mode = 0;
  info->p_info->tx_attr->mode = 0;
  info->p_info->rx_attr->mode = 0;

  size_t ix = shmem_transport_num_contexts;
  size_t newSize = ++shmem_transport_num_contexts;

  if(newSize > shmem_transport_available_contexts) {
    /* TODO: I used realloc here for simplicity, but if we need atomic
     * swap up in shmemx_domain_create, we need to do that here too.
     * The relevant design decision: Must all domain/context
     * allocations happen in one thread, before any domains/contexts
     * are used in other threads? For "No", we get a simpler
     * implementation with no atomics necessary. For "Yes", we get
     * nicer user-side semantics but a possible performance hit.
     */

    size_t newAlloc = 2*newSize;
    shmem_transport_available_contexts = newAlloc;
    shmem_transport_ctx_t** ctxArr = shmem_transport_ofi_contexts;
    ctxArr = realloc(ctxArr,newAlloc*sizeof(shmem_transport_ctx_t*));
    shmem_transport_ofi_contexts = ctxArr;
  }

  size_t nbytes = sizeof(shmem_transport_ctx_t);
  shmem_transport_ofi_contexts[ix] = malloc(nbytes);
  shmem_transport_ctx_t* ctxobj = shmem_transport_ofi_contexts[ix];
  *ctx = ctxobj;
  ctxobj->id = ix;

  shmem_transport_cntr_ep_t* ep = &ctxobj->endpoint;
  ep->pending_count = 0;

  ret = fi_cntr_open(shmem_transport_ofi_domainfd, &cntr_attr,
                     &ep->counter, NULL);
  IF_OFI_ERR_RETURN(ret,"context cntr_open failed");

  ret = fi_endpoint(shmem_transport_ofi_domainfd,
      info->p_info, &ep->ep, NULL);
  IF_OFI_ERR_RETURN(ret,"epfd creation failed");

  ret = fi_ep_bind(ep->ep, &dom->stx->fid, 0);
  IF_OFI_ERR_RETURN(ret,"context ep_bind ep -> stx failed");

  ret = fi_ep_bind(ep->ep, &shmem_transport_ofi_avfd->fid, 0);
  IF_OFI_ERR_RETURN(ret,"ep_bind cntr_ep2av failed");

  ret = fi_ep_bind(ep->ep, &ep->counter->fid, FI_READ | FI_WRITE);
  IF_OFI_ERR_RETURN(ret,"ep_bind cntr_ep2cntr failed");

  ret = fi_enable(ep->ep);
  IF_OFI_ERR_RETURN(ret,"context enable endpoint");


  ctxobj->domain = dom;
  ctxobj->take_lock = dom->take_lock;
  ctxobj->release_lock = dom->release_lock;

  ++dom->num_active_contexts;

  dom->release_lock(&dom);

  return 0;
}

void shmemx_ctx_destroy(shmemx_ctx_t ctxid) {
  shmemx_ctx_quiet(ctxid);

  shmem_transport_ctx_t* ctx = TRANSP_FROM_CTX_T(ctxid);

  shmem_transport_dom_t* dom = ctx->domain;

  dom->take_lock(&dom);


  if(fi_close(&ctx->endpoint.ep->fid)) {
    OFI_ERRMSG("Domain endpoint close failed (%s)",
        fi_strerror(errno));
  }
  if(fi_close(&ctx->endpoint.counter->fid)) {
    OFI_ERRMSG("Domain counter close failed (%s)",
        fi_strerror(errno));
  }

  --dom->num_active_contexts;
  if(!dom->num_active_contexts && dom->freed) {
    shmem_transport_domain_destroy(dom);
  } else {
    dom->release_lock(&dom);
  }
  shmem_transport_ofi_contexts[ctx->id] = NULL;
  free(ctx);
}

void shmemx_ctx_quiet(shmemx_ctx_t c)
{
  shmem_transport_ctx_t* ctx = TRANSP_FROM_CTX_T(c);
  shmem_transport_dom_t* dom = ctx->domain;

  dom->take_lock(&dom);

  shmem_transport_ctx_quiet(ctx);

  dom->release_lock(&dom);
}

void shmemx_ctx_fence(shmemx_ctx_t c) {
#if WANT_TOTAL_DATA_ORDERING == 0
  /*unordered network model*/
  shmemx_ctx_quiet(c);
#endif
}


static inline int allocate_recv_cntr_mr(void)
{

    int ret = 0;
    struct fi_cntr_attr cntr_attr = {0};

    /* ------------------------------------*/
    /* POST enable resources for to EP     */
    /* ------------------------------------*/
    /* since this is AFTER enable and RMA you must create memory regions for incoming reads/writes
     * and outgoing non-blocking Puts, specifying entire VA range */

    // Create counter for incoming writes
    cntr_attr.events   = FI_CNTR_EVENTS_COMP;
    cntr_attr.flags    = 0;

#ifndef ENABLE_HARD_POLLING
    ret = fi_cntr_open(shmem_transport_ofi_domainfd, &cntr_attr,
                       &shmem_transport_ofi_target_cntrfd, NULL);
    if(ret!=0){
        OFI_ERRMSG("target cntr_open failed\n");
        return ret;
    }
#endif

#if defined(ENABLE_MR_SCALABLE) && defined(ENABLE_REMOTE_VIRTUAL_ADDRESSING)
    ret = fi_mr_reg(shmem_transport_ofi_domainfd, 0, UINT64_MAX,
		    FI_REMOTE_READ | FI_REMOTE_WRITE, 0, 0ULL, 0,
		    &shmem_transport_ofi_target_mrfd, NULL);
    if(ret!=0){
	OFI_ERRMSG("mr_reg failed\n");
	return ret;
    }

    // Bind counter with target memory region for incoming messages
#ifndef ENABLE_HARD_POLLING
    ret = fi_mr_bind(shmem_transport_ofi_target_mrfd,
                     &shmem_transport_ofi_target_cntrfd->fid,
                     FI_REMOTE_WRITE | FI_REMOTE_READ);
    if(ret!=0){
        OFI_ERRMSG("mr_bind failed\n");
        return ret;
    }

    //bind to endpoint, incoming communication associated with endpoint now has defined resources
    ret = fi_ep_bind(shmem_transport_dom->cq_ep,
                     &shmem_transport_ofi_target_mrfd->fid,
                     FI_REMOTE_READ | FI_REMOTE_WRITE);
    if(ret!=0){
	OFI_ERRMSG("ep_bind cntr_epfd2put_cntr failed\n");
	return ret;
    }

    ret = fi_ep_bind(shmem_transport_dom->cq_ep,
		    &shmem_transport_ofi_target_cntrfd->fid,
                    FI_REMOTE_WRITE | FI_REMOTE_READ);
    if(ret!=0){
	OFI_ERRMSG("ep_bind cntr_epfd2put_cntr failed\n");
	return ret;
    }
#endif /* ndef ENABLE_HARD_POLLING */

#else
    /* Register separate data and heap segments using keys 0 and 1,
     * respectively.  In MR_BASIC_MODE, the keys are ignored and selected by
     * the provider. */
    ret = fi_mr_reg(shmem_transport_ofi_domainfd, shmem_internal_heap_base,
                    shmem_internal_heap_length,
                    FI_REMOTE_READ | FI_REMOTE_WRITE, 0, 1ULL, 0,
                    &shmem_transport_ofi_target_heap_mrfd, NULL);
    if (ret != 0) {
        OFI_ERRMSG("mr_reg heap failed\n");
        return ret;
    }
    ret = fi_mr_reg(shmem_transport_ofi_domainfd, shmem_internal_data_base,
                    shmem_internal_data_length,
                    FI_REMOTE_READ | FI_REMOTE_WRITE, 0, 0ULL, 0,
                    &shmem_transport_ofi_target_data_mrfd, NULL);
    if (ret != 0) {
        OFI_ERRMSG("mr_reg data segment failed\n");
        return ret;
    }

    /* Bind counter with target memory region for incoming messages */
#ifndef ENABLE_HARD_POLLING
    ret = fi_mr_bind(shmem_transport_ofi_target_heap_mrfd,
                     &shmem_transport_ofi_target_cntrfd->fid,
                     FI_REMOTE_WRITE | FI_REMOTE_READ);
    if (ret != 0) {
        OFI_ERRMSG("mr_bind heap failed\n");
        return ret;
    }
    ret = fi_mr_bind(shmem_transport_ofi_target_data_mrfd,
                     &shmem_transport_ofi_target_cntrfd->fid,
                     FI_REMOTE_WRITE | FI_REMOTE_READ);
    if (ret != 0) {
        OFI_ERRMSG("mr_bind data segment failed\n");
        return ret;
    }

    /* Bind to endpoint, incoming communication associated with endpoint now
     * has defined resources */
    ret = fi_ep_bind(shmem_transport_dom->cq_ep,
                     &shmem_transport_ofi_target_heap_mrfd->fid,
                     FI_REMOTE_READ | FI_REMOTE_WRITE);
    if (ret != 0) {
        OFI_ERRMSG("ep_bind mr2epfd heap failed\n");
        return ret;
    }

    ret = fi_ep_bind(shmem_transport_dom->cq_ep,
                     &shmem_transport_ofi_target_data_mrfd->fid,
                     FI_REMOTE_READ | FI_REMOTE_WRITE);
    if (ret != 0) {
        OFI_ERRMSG("ep_bind mr2epfd data failed\n");
        return ret;
    }
#endif /* ndef ENABLE_HARD_POLLING */
#endif

    return ret;
}

static int publish_mr_info(void)
{
#ifndef ENABLE_MR_SCALABLE
    {
        int err;
        uint64_t heap_key, data_key;

        heap_key = fi_mr_key(shmem_transport_ofi_target_heap_mrfd);
        data_key = fi_mr_key(shmem_transport_ofi_target_data_mrfd);

        err = shmem_runtime_put("fi_heap_key", &heap_key, sizeof(uint64_t));
        if (err) {
            OFI_ERRMSG("Error putting heap key to runtime KVS\n");
            return 1;
        }

        err = shmem_runtime_put("fi_data_key", &data_key, sizeof(uint64_t));
        if (err) {
            OFI_ERRMSG("Error putting data segment key to runtime KVS\n");
            return 1;
        }
    }

#ifndef ENABLE_REMOTE_VIRTUAL_ADDRESSING
    {
        int err;
        err = shmem_runtime_put("fi_heap_addr", &shmem_internal_heap_base, sizeof(uint8_t*));
        if (err) {
            OFI_ERRMSG("Error putting heap address to runtime KVS\n");
            return 1;
        }

        err = shmem_runtime_put("fi_data_addr", &shmem_internal_data_base, sizeof(uint8_t*));
        if (err) {
            OFI_ERRMSG("Error putting data segment address to runtime KVS\n");
            return 1;
        }
    }
#endif /* ENABLE_REMOTE_VIRTUAL_ADDRESSING */
#endif /* ENABLE_MR_SCALABLE */

    return 0;
}

static int populate_mr_tables(void)
{
#ifndef ENABLE_MR_SCALABLE
    {
        int i, err;

        shmem_transport_ofi_target_heap_keys = malloc(sizeof(uint64_t) * shmem_internal_num_pes);
        if (NULL == shmem_transport_ofi_target_heap_keys) {
            OFI_ERRMSG("Out of memory allocating heap keytable\n");
            return 1;
        }

        shmem_transport_ofi_target_data_keys = malloc(sizeof(uint64_t) * shmem_internal_num_pes);
        if (NULL == shmem_transport_ofi_target_data_keys) {
            OFI_ERRMSG("Out of memory allocating heap keytable\n");
            return 1;
        }

        /* Called after the upper layer performs the runtime exchange */
        for (i = 0; i < shmem_internal_num_pes; i++) {
            err = shmem_runtime_get(i, "fi_heap_key",
                                    &shmem_transport_ofi_target_heap_keys[i],
                                    sizeof(uint64_t));
            if (err) {
                OFI_ERRMSG("Error getting heap key from runtime KVS\n");
                return 1;
            }
            err = shmem_runtime_get(i, "fi_data_key",
                                    &shmem_transport_ofi_target_data_keys[i],
                                    sizeof(uint64_t));
            if (err) {
                OFI_ERRMSG("Error getting data segment key from runtime KVS\n");
                return 1;
            }
        }
    }

#ifndef ENABLE_REMOTE_VIRTUAL_ADDRESSING
    {
        int i, err;

        shmem_transport_ofi_target_heap_addrs = malloc(sizeof(uint8_t*) * shmem_internal_num_pes);
        if (NULL == shmem_transport_ofi_target_heap_addrs) {
            OFI_ERRMSG("Out of memory allocating heap addrtable\n");
            return 1;
        }

        shmem_transport_ofi_target_data_addrs = malloc(sizeof(uint8_t*) * shmem_internal_num_pes);
        if (NULL == shmem_transport_ofi_target_data_addrs) {
            OFI_ERRMSG("Out of memory allocating data addrtable\n");
            return 1;
        }

        /* Called after the upper layer performs the runtime exchange */
        for (i = 0; i < shmem_internal_num_pes; i++) {
            err = shmem_runtime_get(i, "fi_heap_addr",
                                    &shmem_transport_ofi_target_heap_addrs[i],
                                    sizeof(uint8_t*));
            if (err) {
                OFI_ERRMSG("Error getting heap addr from runtime KVS\n");
                return 1;
            }
            err = shmem_runtime_get(i, "fi_data_addr",
                                    &shmem_transport_ofi_target_data_addrs[i],
                                    sizeof(uint8_t*));
            if (err) {
                OFI_ERRMSG("Error getting data segment addr from runtime KVS\n");
                return 1;
            }
        }
    }
#endif /* ENABLE_REMOTE_VIRTUAL_ADDRESSING */
#endif /* ENABLE_MR_SCALABLE */

    return 0;
}

/*SOFT_SUPPORT will not produce warning or error */
static inline int atomicvalid_rtncheck(int ret, int atomic_size,
                                    atomic_support_lv atomic_sup,
                                    char strOP[], char strDT[])
{
    if((ret != 0 || atomic_size == 0) && atomic_sup != ATOMIC_SOFT_SUPPORT) {
        if(atomic_sup == ATOMIC_WARNINGS) {
            fprintf(stderr, "Warning OFI detected no support for atomic '%s' "
               "on type '%s'\n", strOP, strDT);
        }
        else if(atomic_sup == ATOMIC_NO_SUPPORT) {
            OFI_ERRMSG("Error: atomicvalid ret=%d atomic_size=%d \n",
                       ret, atomic_size);
	        return ret;
        } else {
            OFI_ERRMSG("Error: invalid software atomic support request\n");
            RAISE_ERROR(-1);
        }
    }

    return 0;
}

static inline int atomicvalid_DTxOP(int DT_MAX, int OPS_MAX, int DT[],
                                    int OPS[], atomic_support_lv atomic_sup)
{
    int i, j, ret = 0;
    size_t atomic_size;

    for(i=0; i<DT_MAX; i++) {
      for(j=0; j<OPS_MAX; j++) {
        ret = fi_atomicvalid(shmem_transport_dom->cq_ep, DT[i],
                        OPS[j], &atomic_size);
         if(atomicvalid_rtncheck(ret, atomic_size, atomic_sup,
                            SHMEM_OpName[OPS[j]],
                            SHMEM_DtName[DT[i]]))
           return ret;
      }
    }

    return 0;
}

static inline int compare_atomicvalid_DTxOP(int DT_MAX, int OPS_MAX, int DT[],
                                    int OPS[], atomic_support_lv atomic_sup)
{
    int i, j, ret = 0;
    size_t atomic_size;

    for(i=0; i<DT_MAX; i++) {
      for(j=0; j<OPS_MAX; j++) {
        ret = fi_compare_atomicvalid(shmem_transport_ctx->endpoint.ep, DT[i],
                        OPS[j], &atomic_size);
         if(atomicvalid_rtncheck(ret, atomic_size, atomic_sup,
                            SHMEM_OpName[OPS[j]],
                            SHMEM_DtName[DT[i]]))
           return ret;
      }
    }

    return 0;
}

static inline int fetch_atomicvalid_DTxOP(int DT_MAX, int OPS_MAX, int DT[],
                                    int OPS[], atomic_support_lv atomic_sup)
{
    int i, j, ret = 0;
    size_t atomic_size;

    for(i=0; i<DT_MAX; i++) {
      for(j=0; j<OPS_MAX; j++) {
        ret = fi_fetch_atomicvalid(shmem_transport_ctx->endpoint.ep, DT[i],
                        OPS[j], &atomic_size);
         if(atomicvalid_rtncheck(ret, atomic_size, atomic_sup,
                            SHMEM_OpName[OPS[j]],
                            SHMEM_DtName[DT[i]]))
           return ret;
      }
    }

    return 0;
}
static inline int atomic_limitations_check(void)
{

    /* ----------------------------------------*/
    /* Retrieve messaging limitations from OFI */
    /*          NOTE:                          */
    /*   currently only have reduction software*/
    /*   atomic support, user can optionally   */
    /*   request for warnings if other atomic  */
    /*   limitations are detected              */
    /* ----------------------------------------*/

    int ret = 0;
    atomic_support_lv general_atomic_sup = ATOMIC_NO_SUPPORT;
    atomic_support_lv reduction_sup = ATOMIC_SOFT_SUPPORT;
    size_t atomic_size;

    if(NULL != shmem_util_getenv_str("OFI_ATOMIC_CHECKS_WARN"))
        general_atomic_sup = ATOMIC_WARNINGS;

    init_ofi_tables();

    /*Retrieve atomic max size */
    ret = fi_atomicvalid(shmem_transport_ctx->endpoint.ep, FI_INT64, FI_SUM,
			&atomic_size);
    if(ret!=0 || (atomic_size == 0)){ //not supported
	    OFI_ERRMSG("atomicvalid failed: cannot determine max atomic size for transport\n");
	    return ret;
    }
    shmem_transport_ofi_max_atomic_size = atomic_size * (sizeof(int64_t));

    if(shmem_transport_ofi_max_atomic_size > shmem_transport_ofi_max_msg_size) {
        OFI_ERRMSG("Error: OFI provider max atomic size is larger than max message size\n");
        RAISE_ERROR(-1);
    }

    /* Standard OPS check */
    ret = atomicvalid_DTxOP(SIZEOF_AMO_DT, SIZEOF_AMO_OPS, DT_AMO_STANDARD,
                      AMO_STANDARD_OPS, general_atomic_sup);
    if(ret)
        return ret;

    ret = fetch_atomicvalid_DTxOP(SIZEOF_AMO_DT, SIZEOF_AMO_FOPS,
                    DT_AMO_STANDARD, FETCH_AMO_STANDARD_OPS,
                    general_atomic_sup);
    if(ret)
        return ret;

    ret = compare_atomicvalid_DTxOP(SIZEOF_AMO_DT, SIZEOF_AMO_COPS,
                    DT_AMO_STANDARD, COMPARE_AMO_STANDARD_OPS,
                    general_atomic_sup);
    if(ret)
        return ret;

    /* Extended OPS check */
    ret = atomicvalid_DTxOP(SIZEOF_AMO_EX_DT, SIZEOF_AMO_EX_OPS, DT_AMO_EXTENDED,
                      AMO_EXTENDED_OPS, general_atomic_sup);
    if(ret)
        return ret;

    ret = fetch_atomicvalid_DTxOP(SIZEOF_AMO_EX_DT, SIZEOF_AMO_EX_FOPS,
                    DT_AMO_EXTENDED, FETCH_AMO_EXTENDED_OPS,
                    general_atomic_sup);
    if(ret)
        return ret;

    /* Reduction OPS check */
    ret = atomicvalid_DTxOP(SIZEOF_RED_DT, SIZEOF_RED_OPS, DT_REDUCE_BITWISE,
                      REDUCE_BITWISE_OPS, reduction_sup);
    if(ret)
        return ret;

    ret = atomicvalid_DTxOP(SIZEOF_REDC_DT, SIZEOF_REDC_OPS, DT_REDUCE_COMPARE,
                      REDUCE_COMPARE_OPS, reduction_sup);
    if(ret)
        return ret;

    ret = atomicvalid_DTxOP(SIZEOF_REDA_DT, SIZEOF_REDA_OPS, DT_REDUCE_ARITH,
                      REDUCE_ARITH_OPS, reduction_sup);
    if(ret)
        return ret;

    /* Internal atomic requirement */
    ret = compare_atomicvalid_DTxOP(SIZEOF_INTERNAL_REQ_DT, SIZEOF_INTERNAL_REQ_OPS,
                    DT_INTERNAL_REQ, INTERNAL_REQ_OPS, general_atomic_sup);
    if(ret)
        return ret;

    return 0;
}

static inline int publish_av_info(struct fabric_info *info)
{
    int    ret = 0;
    char   epname[128];
    size_t epnamelen = sizeof(epname);

#ifdef USE_ON_NODE_COMMS
    if(gethostname(myephostname, (EPHOSTNAMELEN - 1)) != 0)
        OFI_ERRMSG("gethostname error: %s \n", strerror(errno));

    myephostname[EPHOSTNAMELEN] = '\0';

    ret = shmem_runtime_put("fi_ephostname", myephostname, EPHOSTNAMELEN);
    if (ret != 0) {
        OFI_ERRMSG("shmem_runtime_put ephostname failed\n");
        return ret;
    }
#endif

    ret = fi_getname((fid_t)shmem_transport_dom->cq_ep, epname,
        &epnamelen);
    if(ret!=0 || (epnamelen > sizeof(epname))){
        OFI_ERRMSG("fi_getname failed\n");
        return ret;
    }

    ret = shmem_runtime_put("fi_epname", epname, epnamelen);
    if (ret != 0) {
        OFI_ERRMSG("shmem_runtime_put epname failed\n");
        return ret;
    }

    /* Note: we assume that the length of an address is the same for all
     * endpoints.  This is safe for most HPC systems, but could be incorrect in
     * a heterogeneous context. */
    shmem_transport_ofi_addrlen = epnamelen;

    return ret;
}

static inline int populate_av(void)
{
    int    i, ret = 0;
    char   *alladdrs = NULL;
#ifdef USE_ON_NODE_COMMS
    int    num_on_node = 0;
    char   ephostname[EPHOSTNAMELEN];
#endif

    alladdrs = malloc(shmem_internal_num_pes * shmem_transport_ofi_addrlen);
    if (alladdrs == NULL) {
        OFI_ERRMSG("Out of memory allocating 'alladdrs'\n");
        return ret;
    }

    for (i = 0; i < shmem_internal_num_pes; i++) {
        char *addr_ptr = alladdrs + i * shmem_transport_ofi_addrlen;
        shmem_runtime_get(i, "fi_epname", addr_ptr, shmem_transport_ofi_addrlen);

#ifdef USE_ON_NODE_COMMS
        shmem_runtime_get(i, "fi_ephostname", ephostname, EPHOSTNAMELEN);
        if(strncmp(myephostname, ephostname, EPHOSTNAMELEN) == 0) {
            SHMEM_SET_RANK_SAME_NODE(i, num_on_node++);
            if (num_on_node > 255) {
	            OFI_ERRMSG("ERROR: Too many local ranks\n");
                return 1;
            }
        }
#endif
    }

    ret = fi_av_insert(shmem_transport_ofi_avfd,
                       alladdrs,
                       shmem_internal_num_pes,
                       addr_table,
                       0,
                       NULL);
    if (ret != shmem_internal_num_pes) {
        OFI_ERRMSG("av insert failed\n");
        return ret;
    }

    free(alladdrs);

    return 0;
}

static inline int allocate_fabric_resources(struct fabric_info *info)
{
    int ret = 0;
    struct fi_av_attr   av_attr = {0};


    /* fabric domain: define domain of resources physical and logical*/
    ret = fi_fabric(info->p_info->fabric_attr, &shmem_transport_ofi_fabfd, NULL);
    if(ret!=0){
	OFI_ERRMSG("fabric initialization failed\n");
	return ret;
    }

    /*access domain: define communication resource limits/boundary within fabric domain */
    ret = fi_domain(shmem_transport_ofi_fabfd, info->p_info,
		    &shmem_transport_ofi_domainfd,NULL);
    if(ret!=0){
	OFI_ERRMSG("domain initialization failed\n");
	return ret;
    }

    /*AV table set-up for PE mapping*/

#ifdef USE_AV_MAP
    av_attr.type = FI_AV_MAP;
    addr_table   = (fi_addr_t*) malloc(info->npes * sizeof(fi_addr_t));
#else
    /* open Address Vector and bind the AV to the domain */
    av_attr.type = FI_AV_TABLE;
    addr_table   = NULL;
#endif

    ret = fi_av_open(shmem_transport_ofi_domainfd,
		    &av_attr,
		    &shmem_transport_ofi_avfd,
		    NULL);
    if(ret!=0){
	OFI_ERRMSG("av open failed\n");
	return ret;
    }

    return ret;
}

static inline int query_for_fabric(struct fabric_info *info)
{
    int                 ret = 0;
    struct fi_info      hints = {0};
    struct fi_tx_attr   tx_attr = {0};
    struct fi_domain_attr domain_attr = {0};
    struct fi_fabric_attr fabric_attr = {0};
    struct fi_ep_attr   ep_attr = {0};

    shmem_transport_ofi_max_buffered_send = sizeof(long double);

    fabric_attr.prov_name = info->prov_name;

    hints.caps   = FI_RMA |     /* request rma capability
                                    implies FI_READ/WRITE FI_REMOTE_READ/WRITE */
                   FI_ATOMICS;  /* request atomics capability */
#ifndef ENABLE_HARD_POLLING
    hints.caps |= FI_RMA_EVENT; /* want to use remote counters */
#endif /* ndef ENABLE_HARD_POLLING */
    hints.addr_format         = FI_FORMAT_UNSPEC;
    hints.mode		      = FI_CONTEXT;
    domain_attr.data_progress = FI_PROGRESS_AUTO;
    domain_attr.resource_mgmt = FI_RM_ENABLED;
#ifdef ENABLE_MR_SCALABLE
    domain_attr.mr_mode       = FI_MR_SCALABLE; /* VA space-doesn't have to be pre-allocated */
#else
    domain_attr.mr_mode       = FI_MR_BASIC; /* VA space is pre-allocated */
#endif
/* <<<<<<< HEAD */
/*     domain_attr.threading     = FI_THREAD_SAFE; /1* we promise to serialize access */
/* 						       to endpoints. we have only one */
/* 						       thread active at a time *1/ */
/* ======= */
    domain_attr.threading     = FI_THREAD_ENDPOINT; /* we promise to serialize access
                                                       to endpoints. we have only one
                                                       thread active at a time */

    hints.domain_attr         = &domain_attr;
    ep_attr.type              = FI_EP_RDM; /* reliable connectionless */
    hints.fabric_attr	      = &fabric_attr;
    tx_attr.op_flags          = FI_DELIVERY_COMPLETE;
    tx_attr.inject_size       = shmem_transport_ofi_max_buffered_send; /*require provider to support this as a min*/
    hints.tx_attr	      = &tx_attr; /* TODO: fill tx_attr */
    hints.rx_attr	      = NULL;
    hints.ep_attr             = &ep_attr;

    /* find fabric provider to use that is able to support RMA and ATOMICS */
    ret = fi_getinfo( FI_VERSION(OFI_MAJOR_VERSION, OFI_MINOR_VERSION),
                      NULL, NULL, 0, &hints, &(info->fabrics));

    if(ret!=0){
        OFI_ERRMSG("OFI transport did not find any valid fabric services (provider=%s)\n",
                   info->prov_name != NULL ? info->prov_name : "<auto>");
	return ret;
    }

    /* If the user supplied a fabric or domain name, use it to select the
     * fabric.  Otherwise, select the first fabric in the list. */
    if (info->fabric_name != NULL || info->domain_name != NULL) {
        struct fi_info *cur_fabric;

        info->p_info = NULL;

        for (cur_fabric = info->fabrics; cur_fabric; cur_fabric = cur_fabric->next) {
            if (info->fabric_name == NULL ||
                fnmatch(info->fabric_name, cur_fabric->fabric_attr->name, 0) == 0) {
                if (info->domain_name == NULL ||
                    fnmatch(info->domain_name, cur_fabric->domain_attr->name, 0) == 0) {
                    info->p_info = cur_fabric;
                    break;
                }
            }
        }
    }
    else {
        info->p_info = info->fabrics;
    }

    if(NULL == info->p_info) {
        OFI_ERRMSG("OFI transport, no valid fabric (prov=%s, fabric=%s, domain=%s)\n",
                   info->prov_name != NULL ? info->prov_name : "<auto>",
                   info->fabric_name != NULL ? info->fabric_name : "<auto>",
                   info->domain_name != NULL ? info->domain_name : "<auto>");
	return ret;
    }

    if(info->p_info->ep_attr->max_msg_size > 0) {
        shmem_transport_ofi_max_msg_size = info->p_info->ep_attr->max_msg_size;
    } else {
        OFI_ERRMSG("OFI provider did not set max_msg_size\n");
	return 1;
    }

    shmem_internal_assertp(info->p_info->tx_attr->inject_size >= shmem_transport_ofi_max_buffered_send);
    shmem_transport_ofi_max_buffered_send = info->p_info->tx_attr->inject_size;

    return ret;

}

int shmem_transport_init(int thread_level, long eager_size)
{
  int ret = 0;

  {
    struct fabric_info info = {0};

    info.npes      = shmem_runtime_get_size();

    info.prov_name = shmem_util_getenv_str("OFI_PROVIDER");
    if (NULL == info.prov_name)
      info.prov_name = shmem_util_getenv_str("OFI_USE_PROVIDER");

    info.fabric_name = shmem_util_getenv_str("OFI_FABRIC");
    info.domain_name = shmem_util_getenv_str("OFI_DOMAIN");

    shmem_ofi_cq_info = info;
    shmem_ofi_cntr_info = info;
  }

  ret = query_for_fabric(&shmem_ofi_cq_info);
  if(ret!=0)
    return ret;

  ret = query_for_fabric(&shmem_ofi_cntr_info);
  if(ret!=0)
    return ret;

  ret = allocate_fabric_resources(&shmem_ofi_cq_info);

  if(ret!=0)
    return ret;

  shmem_transport_available_contexts = 8;
  shmem_transport_available_domains = 8;
  shmem_transport_ofi_domains = malloc(
    shmem_transport_available_domains*sizeof(shmem_transport_dom_t*));
  shmem_transport_ofi_contexts = malloc(
    shmem_transport_available_contexts
    *sizeof(shmem_transport_ctx_t*));

  ret = shmemx_domain_create(thread_level,1,&shmem_transport_default_dom);
  if(ret!=0)
    return ret;

  ret = shmemx_ctx_create(shmem_transport_default_dom,
      &shmem_transport_default_ctx);
  if(ret!=0)
    return ret;

  SHMEMX_DOMAIN_DEFAULT = shmem_transport_default_dom;
  SHMEMX_CTX_DEFAULT = shmem_transport_default_ctx;

  shmem_transport_dom =
    TRANSP_FROM_DOM_T(shmem_transport_default_dom);
  shmem_transport_ctx =
    TRANSP_FROM_CTX_T(shmem_transport_default_ctx);

  ret = allocate_recv_cntr_mr();
  if(ret!=0)
    return ret;

  ret = publish_mr_info();
  if (ret != 0)
    return ret;

  ret = atomic_limitations_check();
  if(ret!=0)
    return ret;

  ret = publish_av_info(&shmem_ofi_cq_info);
  if(ret!=0)
    return ret;

  return 0;
}

int shmem_transport_startup(void)
{
    int ret;

    ret = populate_mr_tables();
    if (ret != 0)
        return ret;

    ret = populate_av();
    if(ret!=0)
        return ret;

    return 0;
}

void shmem_transport_print_info(void)
{
    char *ofi_provider;

    if (NULL == (ofi_provider = shmem_util_getenv_str("OFI_PROVIDER")))
        if (NULL == (ofi_provider = shmem_util_getenv_str("OFI_USE_PROVIDER")))
            ofi_provider = "AUTO";

    printf("\n");
    printf("Network transport:      OFI\n");
    printf("SMA_OFI_PROVIDER        %s\n", ofi_provider);
    printf("\tProvider that should be used by the OFI transport\n");
    printf("SMA_OFI_FABRIC          %s\n",
           (NULL != shmem_util_getenv_str("OFI_FABRIC")) ?
           shmem_util_getenv_str("OFI_FABRIC") : "AUTO");
    printf("\tFabric that should be used by the OFI transport\n");
    printf("SMA_OFI_DOMAIN          %s\n",
           (NULL != shmem_util_getenv_str("OFI_DOMAIN")) ?
           shmem_util_getenv_str("OFI_DOMAIN") : "AUTO");
    printf("\tFabric domain that should be used by the OFI transport\n");
    printf("SMA_OFI_ATOMIC_CHECKS_WARN %s\n",
           (NULL != shmem_util_getenv_str("OFI_ATOMIC_CHECKS_WARN")) ?
           "Set" : "Not set");
    printf("\tDisplay warnings about unsupported atomic operations\n");
}

int shmem_transport_fini(void)
{
    /* Wait for acks before shutdown */
    size_t i;
    for(i = 0; i < shmem_transport_num_contexts; ++i) {
      shmem_transport_ctx_t* ctx = shmem_transport_ofi_contexts[i];
      if(ctx) {
        shmemx_ctx_destroy(ctx);
      }
    }

    for(i = 0; i < shmem_transport_num_domains; ++i) {
      shmem_transport_dom_t* dom = shmem_transport_ofi_domains[i];
      if(dom) {
        dom->take_lock(&dom);
        shmem_transport_domain_destroy(dom);
      }
    }

    free(shmem_transport_ofi_domains);
    free(shmem_transport_ofi_contexts);

#if defined(ENABLE_MR_SCALABLE) && defined(ENABLE_REMOTE_VIRTUAL_ADDRESSING)
    if (shmem_transport_ofi_target_mrfd &&
		    fi_close(&shmem_transport_ofi_target_mrfd->fid)) {
	OFI_ERRMSG("Target MR close failed (%s)", fi_strerror(errno));
    }
#else
    if (shmem_transport_ofi_target_heap_mrfd &&
        fi_close(&shmem_transport_ofi_target_heap_mrfd->fid)) {
        OFI_ERRMSG("Target heap MR close failed (%s)", fi_strerror(errno));
    }

    if (shmem_transport_ofi_target_data_mrfd &&
        fi_close(&shmem_transport_ofi_target_data_mrfd->fid)) {
        OFI_ERRMSG("Target data MR close failed (%s)", fi_strerror(errno));
    }
#endif

#ifndef ENABLE_HARD_POLLING
    if(shmem_transport_ofi_target_cntrfd &&
		    fi_close(&shmem_transport_ofi_target_cntrfd->fid)){
        OFI_ERRMSG("Target CT close failed (%s)", fi_strerror(errno));
    }
#endif

    if (shmem_transport_ofi_avfd &&
		    fi_close(&shmem_transport_ofi_avfd->fid)) {
        OFI_ERRMSG("AV close failed (%s)", fi_strerror(errno));
    }

    /* This seems to be closed by closing some of the other resources
     * (uncommenting gives a "file not found error" OFI error)
     * -jpdoyle
     */
    /* if (shmem_transport_ofi_domainfd && */
		    /* fi_close(&shmem_transport_ofi_domainfd->fid)) { */
    /*     OFI_ERRMSG("Domain close failed (%s)", fi_strerror(errno)); */
    /* } */

    if (shmem_transport_ofi_fabfd &&
		    fi_close(&shmem_transport_ofi_fabfd->fid)) {
        OFI_ERRMSG("Fabric close failed (%s)", fi_strerror(errno));
    }

#ifdef USE_AV_MAP
  free(addr_table);
#endif

    fi_freeinfo(shmem_ofi_cq_info.fabrics);
    fi_freeinfo(shmem_ofi_cntr_info.fabrics);

    return 0;
}
