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

struct fabric_info {
    struct fi_info *fabrics;
    struct fi_info *p_info;
    char *prov_name;
    char *fabric_name;
    char *domain_name;
    int npes;
};

struct fid_fabric*          	shmem_transport_ofi_fabfd;
struct fid_domain*          	shmem_transport_ofi_domainfd;
struct fid_ep*			shmem_transport_ofi_epfd;
struct fid_ep*			shmem_transport_ofi_cntr_epfd;
struct fid_stx*  		shmem_transport_ofi_stx;
struct fid_av*             	shmem_transport_ofi_avfd;
struct fid_cq*              	shmem_transport_ofi_put_nb_cqfd;
struct fid_cntr*            	shmem_transport_ofi_target_cntrfd;
struct fid_cntr*            	shmem_transport_ofi_put_cntrfd;
struct fid_cntr*            	shmem_transport_ofi_get_cntrfd;
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
uint64_t	          	shmem_transport_ofi_pending_put_counter;
uint64_t        	 	shmem_transport_ofi_pending_get_counter;
uint64_t			shmem_transport_ofi_pending_cq_count;
uint64_t			shmem_transport_ofi_max_poll;
size_t           		shmem_transport_ofi_max_buffered_send;
size_t    	 		shmem_transport_ofi_max_atomic_size;
size_t    			shmem_transport_ofi_max_msg_size;
size_t    			shmem_transport_ofi_bounce_buffer_size;
fi_addr_t			*addr_table;

size_t SHMEM_Dtsize[FI_DATATYPE_LAST];

static inline void init_dt_size(void)
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
}

static int SHM_DT_INT[]=
{
  DTYPE_SHORT, DTYPE_INT, DTYPE_LONG,
};

static int SHM_DT_CMP[]=
{
  DTYPE_SHORT, DTYPE_INT, DTYPE_LONG,
  SHM_INTERNAL_FLOAT, SHM_INTERNAL_DOUBLE
};

static int SHM_BOPS[]=
{
  SHM_INTERNAL_BAND, SHM_INTERNAL_BOR, SHM_INTERNAL_BXOR,
};

static int SHM_OPS[]=
{
  SHM_INTERNAL_MIN,  SHM_INTERNAL_MAX, SHM_INTERNAL_SUM,
  SHM_INTERNAL_PROD,
};

int shmem_transport_have_long_double = 1;

shmem_free_list_t *shmem_transport_ofi_bounce_buffers = NULL;

//size of CQ
const static size_t shmem_transport_ofi_queue_slots = 32768;//default CQ Depth....
size_t   shmem_transport_ofi_max_atomic_size = 0;
uint64_t shmem_transport_ofi_max_poll = (1ULL<<30);

#define OFI_MAJOR_VERSION 1
#define OFI_MINOR_VERSION 0

static
void
init_bounce_buffer(shmem_free_list_item_t *item)
{
    shmem_transport_ofi_frag_t *frag =
        (shmem_transport_ofi_frag_t*) item;
    frag->mytype = SHMEM_TRANSPORT_OFI_TYPE_BOUNCE;
}

static inline int allocate_endpoints(struct fabric_info *info)
{

  int ret = 0;

    /* ------------------------------------*/
    /* 		Allocate Endpoints	   */
    /* ------------------------------------*/

    /* this endpoint is used to get completion events and
     * used to expose memory to incoming reads/writes */
    info->p_info->ep_attr->tx_ctx_cnt = FI_SHARED_CONTEXT;
    info->p_info->tx_attr->op_flags = FI_DELIVERY_COMPLETE;
    ret = fi_endpoint(shmem_transport_ofi_domainfd,
                      info->p_info, &shmem_transport_ofi_epfd, NULL);
    if(ret!=0){
	OFI_ERRMSG("epfd creation failed\n");
	return ret;
    }

    /* this endpoint is used only to get read and write
     * counter updates */
    info->p_info->caps = FI_RMA | FI_WRITE | FI_READ | /*SEND ONLY */
                         FI_ATOMICS; /* request atomics capability */
    info->p_info->tx_attr->op_flags = FI_DELIVERY_COMPLETE | FI_INJECT_COMPLETE;
    info->p_info->mode = 0;
    info->p_info->tx_attr->mode = 0;
    info->p_info->rx_attr->mode = 0;
    ret = fi_endpoint(shmem_transport_ofi_domainfd,
                      info->p_info, &shmem_transport_ofi_cntr_epfd, NULL);
    if(ret!=0){
	OFI_ERRMSG("cntr_epfd creation failed\n");
	return ret;
    }

    return ret;

}

static inline int bind_resources_to_and_enable_ep(void)
{

    /*   must bind resources created to EP then enable EP for communication (resources can now be used) */

    int ret = 0;

    /* attach the endpoints to the shared context */
    ret = fi_ep_bind(shmem_transport_ofi_epfd,
		    &shmem_transport_ofi_stx->fid, 0);
    if(ret!=0){
	OFI_ERRMSG("ep_bind epfd2stx failed\n");
	return ret;
    }

    ret = fi_ep_bind(shmem_transport_ofi_cntr_epfd,
		    &shmem_transport_ofi_stx->fid, 0);
    if(ret!=0){
	OFI_ERRMSG("ep_bind cntr_epfd2stx failed\n");
	return ret;
    }

    /* attaching to endpoint enables counting "writes" for calls used with this endpoint*/
    ret = fi_ep_bind(shmem_transport_ofi_cntr_epfd,
		    &shmem_transport_ofi_put_cntrfd->fid, FI_WRITE);
    if(ret!=0){
	OFI_ERRMSG("ep_bind cntr_epfd2put_cntr failed\n");
	return ret;
    }

    /* attach to endpoint */
    ret = fi_ep_bind(shmem_transport_ofi_cntr_epfd,
		    &shmem_transport_ofi_get_cntrfd->fid, FI_READ);
    if(ret!=0){
	OFI_ERRMSG("ep_bind cntr_epfd2get_cntr failed\n");
	return ret;
    }

    /* attach CQ for obtaining completions for large puts (NB puts) */
    ret = fi_ep_bind(shmem_transport_ofi_epfd,
		    &shmem_transport_ofi_put_nb_cqfd->fid, FI_SEND);
    if(ret!=0){
	OFI_ERRMSG("ep_bind ep2cq_nb failed\n");
	return ret;
    }

    ret = fi_ep_bind(shmem_transport_ofi_epfd, &shmem_transport_ofi_avfd->fid, 0);
    if(ret!=0){
	OFI_ERRMSG("ep_bind ep2av failed\n");
	return ret;
    }

    ret = fi_ep_bind(shmem_transport_ofi_cntr_epfd, &shmem_transport_ofi_avfd->fid, 0);
    if(ret!=0){
	OFI_ERRMSG("ep_bind cntr_ep2av failed\n");
	return ret;
    }

    /*enable active endpoint state: can now perform data transfers*/
    ret = fi_enable(shmem_transport_ofi_epfd);
    if(ret!=0){
	OFI_ERRMSG("enable_epfd failed\n");
	return ret;
    }
    ret = fi_enable(shmem_transport_ofi_cntr_epfd);
    if(ret!=0){
	OFI_ERRMSG("enable_cntr_epfd failed\n");
	return ret;
    }

    return ret;
}

static inline int allocate_cntr_and_cq(void)
{

    int ret = 0;
    struct fi_cntr_attr cntr_attr = {0};
    struct fi_cq_attr   cq_attr = {0};
    cntr_attr.events   = FI_CNTR_EVENTS_COMP;

    /* -------------------------------------------------------*/
    /* Define Completion tracking Resources to Attach to EP   */
    /* -------------------------------------------------------*/

    // Create counter for counting completions of outgoing writes

    ret = fi_cntr_open(shmem_transport_ofi_domainfd, &cntr_attr,
		  &shmem_transport_ofi_put_cntrfd, NULL);
    if(ret!=0){
	OFI_ERRMSG("put cntr_open failed\n");
	return ret;
    }

    // Create counter for counting completions of outbound reads

    ret = fi_cntr_open(shmem_transport_ofi_domainfd, &cntr_attr,
		  &shmem_transport_ofi_get_cntrfd, NULL);
    if(ret!=0){
	OFI_ERRMSG("get cntr_open failed\n");
	return ret;
    }

    /* Create CQ to be used for NB puts */
    cq_attr.format    = FI_CQ_FORMAT_CONTEXT;//event type for CQ,only context stored/reported
    cq_attr.size      =	shmem_transport_ofi_queue_slots;

    ret = fi_cq_open(shmem_transport_ofi_domainfd, &cq_attr,
		    &shmem_transport_ofi_put_nb_cqfd, NULL);
    if(ret!=0){
	OFI_ERRMSG("cq_open failed\n");
	return ret;
    }

    return ret;

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

    ret = fi_cntr_open(shmem_transport_ofi_domainfd, &cntr_attr,
                       &shmem_transport_ofi_target_cntrfd, NULL);
    if(ret!=0){
        OFI_ERRMSG("target cntr_open failed\n");
        return ret;
    }

#if defined(ENABLE_MR_SCALABLE) && defined(ENABLE_REMOTE_VIRTUAL_ADDRESSING)
    ret = fi_mr_reg(shmem_transport_ofi_domainfd, 0, UINT64_MAX,
		    FI_REMOTE_READ | FI_REMOTE_WRITE, 0, 0ULL, 0,
		    &shmem_transport_ofi_target_mrfd, NULL);
    if(ret!=0){
	OFI_ERRMSG("mr_reg failed\n");
	return ret;
    }

    // Bind counter with target memory region for incoming messages
    ret = fi_mr_bind(shmem_transport_ofi_target_mrfd,
		    &shmem_transport_ofi_target_cntrfd->fid,
		    FI_REMOTE_WRITE | FI_REMOTE_READ);
    if(ret!=0){
	OFI_ERRMSG("mr_bind failed\n");
	return ret;
    }

    //bind to endpoint, incoming communication associated with endpoint now has defined resources
    ret = fi_ep_bind(shmem_transport_ofi_epfd,
		    &shmem_transport_ofi_target_mrfd->fid,
		    FI_REMOTE_READ | FI_REMOTE_WRITE);
    if(ret!=0){
	OFI_ERRMSG("ep_bind mr2epfd failed\n");
	return ret;
    }

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
    ret = fi_ep_bind(shmem_transport_ofi_epfd,
                     &shmem_transport_ofi_target_heap_mrfd->fid,
                     FI_REMOTE_READ | FI_REMOTE_WRITE);
    if (ret != 0) {
        OFI_ERRMSG("ep_bind mr2epfd heap failed\n");
        return ret;
    }
    ret = fi_ep_bind(shmem_transport_ofi_epfd,
                     &shmem_transport_ofi_target_data_mrfd->fid,
                     FI_REMOTE_READ | FI_REMOTE_WRITE);
    if (ret != 0) {
        OFI_ERRMSG("ep_bind mr2epfd data failed\n");
        return ret;
    }
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

static inline int atomic_limitations_check(void)
{

    /* ----------------------------------------*/
    /* Retrieve messaging limitations from OFI */
    /* ----------------------------------------*/

    int i, ret = 0;

    init_dt_size();

    /*RETRIEVE atomic max size for ATOMIC_NB case */
    size_t atomic_size;
    ret = fi_atomicvalid(shmem_transport_ofi_epfd, FI_INT64, FI_MAX,
			&atomic_size);
    if(ret!=0 || (atomic_size == 0)){ //not supported
	OFI_ERRMSG("atomicvalid failed\n");
	return ret;
    }
    shmem_transport_ofi_max_atomic_size = atomic_size * (sizeof(long));

    if(shmem_transport_ofi_max_atomic_size > shmem_transport_ofi_max_msg_size) {
        OFI_ERRMSG("Error: OFI provider max atomic size is larger than max message size\n");
        RAISE_ERROR(-1);
    }

    int j;
    /* Binary OPS check */
    for(i=0; i<3; i++) {//DT
      for(j=0; j<3; j++) { //OPS
        ret = fi_atomicvalid(shmem_transport_ofi_epfd, SHM_DT_INT[i], SHM_BOPS[j],
                        &atomic_size);
        if(ret!=0 || atomic_size == 0) {
           OFI_ERRMSG("ret=%d atomic_size=%d %d %d\n", ret, (int)atomic_size, i, j);
	   return ret;
        }
      }
    }

    /* OTHER OPS check */
    for(i=0; i<5; i++) {//DT
      for(j=0; j<4; j++) { //OPS
        ret = fi_atomicvalid(shmem_transport_ofi_epfd, SHM_DT_CMP[i], SHM_OPS[j],
                        &atomic_size);
        if(ret!=0 || atomic_size == 0) {
           OFI_ERRMSG("ret=%d atomic_size=%d %d %d\n", ret, (int)atomic_size, i, j);
	   return ret;
        }
      }
    }

    /* LONG DOUBLE limitation is common */
    for(j=0; j<4; j++) { //OPS
        ret = fi_atomicvalid(shmem_transport_ofi_epfd, SHM_INTERNAL_LONG_DOUBLE, SHM_OPS[j], &atomic_size);
        if(ret!=0 || atomic_size == 0) {
		shmem_transport_have_long_double = 0;
		break;
	}
    }

    return 0;
}

static inline int exchange_and_av_insert(struct fabric_info *info)
{
    int                 i, rank = 0, ret = 0;
    char   epname[128], *alladdrs = NULL;
    size_t epnamelen = sizeof(epname);

    rank = shmem_runtime_get_rank();

    ret = fi_getname((fid_t)shmem_transport_ofi_epfd, epname, &epnamelen);
    if(ret!=0 || (epnamelen > sizeof(epname))){
	OFI_ERRMSG("PMI get rank failed\n");
	return ret;
    }

    alladdrs = malloc(info->npes * epnamelen);
    if(alladdrs==NULL){
	OFI_ERRMSG("alladdrs is NULL\n");
	return ret;
    }

    ret = shmem_runtime_put("OFI", epname, epnamelen);
    shmem_runtime_exchange();
    shmem_runtime_barrier();

    for(i=0; i<info->npes; i++)
    {
	    void *tgt = alladdrs + i*epnamelen;
	    if (i == rank) {
		    memcpy(tgt, epname, epnamelen);
	    } else {
		    shmem_runtime_get(i,"OFI", tgt, epnamelen);
	    }
    }

    ret = fi_av_insert(shmem_transport_ofi_avfd,
            alladdrs,
            info->npes,
            addr_table,
            0,
	    NULL);
    if(ret!=info->npes){
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

    /*transmit context: allocate one transmit context for this SHMEM PE
     * and share it across different multiple endpoints. Since we have only
     * one thread per PE, a single context is sufficient and allows more
     * more PEs/node (i.e. doesn't exhaust contexts)  */
    ret = fi_stx_context(shmem_transport_ofi_domainfd, NULL, /* TODO: fill tx_attr */
		    &shmem_transport_ofi_stx, NULL);
    if(ret!=0) {
	OFI_ERRMSG("stx context initialization failed\n");
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

    hints.caps	  = FI_RMA |     /* request rma capability
                                    implies FI_READ/WRITE FI_REMOTE_READ/WRITE */
                   FI_ATOMICS |  /* request atomics capability */
                   FI_RMA_EVENT; /* want to use remote counters */
    hints.addr_format         = FI_FORMAT_UNSPEC;
    hints.mode		      = FI_CONTEXT;
    domain_attr.data_progress = FI_PROGRESS_AUTO;
    domain_attr.resource_mgmt = FI_RM_ENABLED;
#ifdef ENABLE_MR_SCALABLE
    domain_attr.mr_mode       = FI_MR_SCALABLE; /* VA space-doesn't have to be pre-allocated */
#else
    domain_attr.mr_mode       = FI_MR_BASIC; /* VA space is pre-allocated */
#endif
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

int shmem_transport_init(long eager_size)
{
    int ret = 0;
    struct fabric_info info = {0};

    info.npes      = shmem_runtime_get_size();

    info.prov_name = shmem_util_getenv_str("OFI_PROVIDER");
    if (NULL == info.prov_name)
        info.prov_name = shmem_util_getenv_str("OFI_USE_PROVIDER");

    info.fabric_name = shmem_util_getenv_str("OFI_FABRIC");
    info.domain_name = shmem_util_getenv_str("OFI_DOMAIN");

    ret = query_for_fabric(&info);
    if(ret!=0)
	return ret;

    shmem_transport_ofi_bounce_buffer_size = eager_size;

    //init LL for NB buffers
    shmem_transport_ofi_bounce_buffers =
       shmem_free_list_init(sizeof(shmem_transport_ofi_bounce_buffer_t)
				+ eager_size, init_bounce_buffer);

    ret = allocate_fabric_resources(&info);

    if(ret!=0)
	return ret;

    ret = allocate_endpoints(&info);
    if(ret!=0)
	return ret;

    ret = allocate_cntr_and_cq();
    if(ret!=0)
	return ret;

    ret = bind_resources_to_and_enable_ep();
    if(ret!=0)
	return ret;

    ret = allocate_recv_cntr_mr();
    if(ret!=0)
	return ret;

    ret = publish_mr_info();
    if (ret != 0)
        return ret;

    ret = atomic_limitations_check();
    if(ret!=0)
	return ret;

    ret = exchange_and_av_insert(&info);
    if(ret!=0)
	return ret;

    fi_freeinfo(info.fabrics);

    return 0;
}

int shmem_transport_startup(void)
{
    int ret;

    ret = populate_mr_tables();
    if (ret != 0)
        return ret;

    return 0;
}

int shmem_transport_fini(void)
{
    /* Wait for acks before shutdown */
    shmem_transport_quiet();

    if (shmem_transport_ofi_epfd &&
		    fi_close(&shmem_transport_ofi_epfd->fid)) {
        OFI_ERRMSG("Endpoint close failed (%s)", fi_strerror(errno));
    }

    if (shmem_transport_ofi_cntr_epfd &&
		    fi_close(&shmem_transport_ofi_cntr_epfd->fid)) {
        OFI_ERRMSG("Endpoint close failed (%s)", fi_strerror(errno));
    }

    if (shmem_transport_ofi_stx &&
		    fi_close(&shmem_transport_ofi_stx->fid)) {
        OFI_ERRMSG("Shared context close failed (%s)", fi_strerror(errno));
    }

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

    if (shmem_transport_ofi_put_nb_cqfd &&
		    fi_close(&shmem_transport_ofi_put_nb_cqfd->fid)) {
        OFI_ERRMSG("Write CQ close failed (%s)", fi_strerror(errno));
    }

    if(shmem_transport_ofi_put_cntrfd &&
		    fi_close(&shmem_transport_ofi_put_cntrfd->fid)){
        OFI_ERRMSG("INJECT PUT CT close failed (%s)", fi_strerror(errno));
    }

    if(shmem_transport_ofi_get_cntrfd &&
		    fi_close(&shmem_transport_ofi_get_cntrfd->fid)){
        OFI_ERRMSG("GET CT close failed (%s)", fi_strerror(errno));
    }

    if(shmem_transport_ofi_target_cntrfd &&
		    fi_close(&shmem_transport_ofi_target_cntrfd->fid)){
        OFI_ERRMSG("Target CT close failed (%s)", fi_strerror(errno));
    }

    if (shmem_transport_ofi_avfd &&
		    fi_close(&shmem_transport_ofi_avfd->fid)) {
        OFI_ERRMSG("AV close failed (%s)", fi_strerror(errno));
    }

    if (shmem_transport_ofi_domainfd &&
		    fi_close(&shmem_transport_ofi_domainfd->fid)) {
        OFI_ERRMSG("Domain close failed (%s)", fi_strerror(errno));
    }

    if (shmem_transport_ofi_fabfd &&
		    fi_close(&shmem_transport_ofi_fabfd->fid)) {
        OFI_ERRMSG("Fabric close failed (%s)", fi_strerror(errno));
    }

    if (NULL != shmem_transport_ofi_bounce_buffers) {
        shmem_free_list_destroy(shmem_transport_ofi_bounce_buffers);
    }

#ifdef USE_AV_MAP
  free(addr_table);
#endif

    return 0;
}
