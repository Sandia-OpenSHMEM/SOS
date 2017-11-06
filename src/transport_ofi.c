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
#include <inttypes.h>

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

struct fid_fabric*              shmem_transport_ofi_fabfd;
struct fid_domain*              shmem_transport_ofi_domainfd;
struct fid_stx*                 shmem_transport_ofi_stx;
struct fid_av*                  shmem_transport_ofi_avfd;
struct fid_cq*                  shmem_transport_ofi_put_nb_cqfd;
#ifndef ENABLE_HARD_POLLING
struct fid_cntr*                shmem_transport_ofi_target_cntrfd;
#endif
#ifdef ENABLE_MR_SCALABLE
#ifdef ENABLE_REMOTE_VIRTUAL_ADDRESSING
struct fid_mr*                  shmem_transport_ofi_target_mrfd;
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
shmem_internal_atomic_uint64_t  shmem_transport_ofi_pending_cq_count;
uint64_t                        shmem_transport_ofi_max_poll;
long                            shmem_transport_ofi_put_poll_limit;
long                            shmem_transport_ofi_get_poll_limit;
size_t                          shmem_transport_ofi_max_buffered_send;
size_t                          shmem_transport_ofi_max_msg_size;
size_t                          shmem_transport_ofi_bounce_buffer_size;
size_t                          shmem_transport_ofi_addrlen;
fi_addr_t                       *addr_table;
#ifdef USE_ON_NODE_COMMS
#define EPHOSTNAMELEN  _POSIX_HOST_NAME_MAX + 1
static char                     myephostname[EPHOSTNAMELEN];
#endif
#ifdef ENABLE_THREADS
shmem_internal_mutex_t          shmem_transport_ofi_lock;
#endif

struct fabric_info shmem_transport_ofi_info = {0};

static shmem_transport_ctx_t** shmem_transport_ofi_contexts;
static size_t shmem_transport_ofi_num_ctx = 0;

shmem_transport_ctx_t shmem_transport_ctx_default;
void *SHMEM_CTX_DEFAULT = &shmem_transport_ctx_default;

static size_t shmem_transport_ofi_grow_size = 128;
static size_t shmem_transport_ofi_avail_ctx = 0;

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
    SHMEM_Dtsize[FI_FLOAT_COMPLEX]       = sizeof(float _Complex);
    SHMEM_Dtsize[FI_DOUBLE_COMPLEX]      = sizeof(double _Complex);
    SHMEM_Dtsize[FI_LONG_DOUBLE]         = sizeof(long double);
    SHMEM_Dtsize[FI_LONG_DOUBLE_COMPLEX] = sizeof(long double _Complex);

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
    SHMEM_DtName[FI_FLOAT_COMPLEX]       = "float _Complex";
    SHMEM_DtName[FI_DOUBLE_COMPLEX]      = "double _Complex";
    SHMEM_DtName[FI_LONG_DOUBLE]         = "long double";
    SHMEM_DtName[FI_LONG_DOUBLE_COMPLEX] = "long double _Complex";

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
/* Locking implementation requirement */
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


shmem_free_list_t *shmem_transport_ofi_bounce_buffers = NULL;

/* default CQ depth */
const static size_t shmem_transport_ofi_queue_slots = 32768;
uint64_t shmem_transport_ofi_max_poll = (1ULL<<30);

#define OFI_MAJOR_VERSION 1
#define OFI_MINOR_VERSION 0

static
void init_bounce_buffer(shmem_free_list_item_t *item)
{
    shmem_transport_ofi_frag_t *frag =
        (shmem_transport_ofi_frag_t*) item;
    frag->mytype = SHMEM_TRANSPORT_OFI_TYPE_BOUNCE;
}

static inline
int allocate_endpoints(shmem_transport_ctx_t *ctx, struct fabric_info *info)
{

    int ret = 0;

    /* ------------------------------------ */
    /*         Allocate Endpoints           */
    /* ------------------------------------ */

    /* this endpoint is used to get completion events and
     * used to expose memory to incoming reads/writes */
    info->p_info->ep_attr->tx_ctx_cnt = FI_SHARED_CONTEXT;
    info->p_info->tx_attr->op_flags = FI_DELIVERY_COMPLETE;
    ret = fi_endpoint(shmem_transport_ofi_domainfd,
                      info->p_info, &ctx->endpoint.ep, NULL);
    if (ret!=0) {
        RAISE_WARN_STR("epfd creation failed");
        return ret;
    }

    ret = fi_endpoint(shmem_transport_ofi_domainfd,
                      info->p_info, &ctx->endpoint.cntr_ep, NULL);
    if (ret!=0) {
        RAISE_WARN_STR("cntr_epfd creation failed");
        return ret;
    }

    return ret;

}

static inline
int bind_resources_to_and_enable_ep(shmem_transport_ctx_t *ctx)
{
    /* must bind resources created to EP then enable EP for communication
     * (resources can now be used) */

    int ret = 0;

    /* attach the endpoints to the shared context */
    ret = fi_ep_bind(ctx->endpoint.ep,
                     &shmem_transport_ofi_stx->fid, 0);
    if (ret!=0) {
        RAISE_WARN_STR("ep_bind epfd2stx failed");
        return ret;
    }

    ret = fi_ep_bind(ctx->endpoint.cntr_ep,
                     &shmem_transport_ofi_stx->fid, 0);
    if (ret!=0) {
        RAISE_WARN_STR("ep_bind cntr_epfd2stx failed");
        return ret;
    }

    /* attaching to endpoint enables counting "writes" for calls used with this
     * endpoint */
    ret = fi_ep_bind(ctx->endpoint.cntr_ep,
                     &ctx->endpoint.put_cntrfd->fid, FI_WRITE);
    if (ret!=0) {
        RAISE_WARN_STR("ep_bind cntr_epfd2put_cntr failed");
        return ret;
    }

    /* attach to endpoint */
    ret = fi_ep_bind(ctx->endpoint.cntr_ep,
                     &ctx->endpoint.get_cntrfd->fid, FI_READ);
    if (ret!=0) {
        RAISE_WARN_STR("ep_bind cntr_epfd2get_cntr failed");
        return ret;
    }

    /* attach CQ for obtaining completions for large puts (NB puts) */
    ret = fi_ep_bind(ctx->endpoint.ep,
                     &shmem_transport_ofi_put_nb_cqfd->fid, FI_SEND);
    if (ret!=0) {
        RAISE_WARN_STR("ep_bind ep2cq_nb failed");
        return ret;
    }

    /* attach CQ for error handling on cntr EP */
    ret = fi_ep_bind(ctx->endpoint.cntr_ep,
                     &shmem_transport_ofi_put_nb_cqfd->fid,
                     FI_SELECTIVE_COMPLETION | FI_TRANSMIT);
    if (ret!=0) {
        RAISE_WARN_STR("ep_bind cntrep2cq_nb failed");
        return ret;
    }

    ret = fi_ep_bind(ctx->endpoint.ep,
                     &shmem_transport_ofi_avfd->fid, 0);
    if (ret!=0) {
        RAISE_WARN_STR("ep_bind ep2av failed");
        return ret;
    }

    ret = fi_ep_bind(ctx->endpoint.cntr_ep,
                     &shmem_transport_ofi_avfd->fid, 0);
    if (ret!=0) {
        RAISE_WARN_STR("ep_bind cntr_ep2av failed");
        return ret;
    }

    /* enable active endpoint state: can now perform data transfers */
    ret = fi_enable(ctx->endpoint.ep);
    if (ret!=0) {
        RAISE_WARN_STR("enable_epfd failed");
        return ret;
    }
    ret = fi_enable(ctx->endpoint.cntr_ep);
    if (ret!=0) {
        RAISE_WARN_STR("enable_cntr_epfd failed");
        return ret;
    }

    return ret;
}

static inline
int allocate_cntr_and_cq(shmem_transport_ctx_t *ctx)
{

    int ret = 0;
    struct fi_cntr_attr cntr_put_attr = {0};
    struct fi_cntr_attr cntr_get_attr = {0};
    struct fi_cq_attr   cq_attr = {0};

    cntr_put_attr.events   = FI_CNTR_EVENTS_COMP;
    cntr_get_attr.events   = FI_CNTR_EVENTS_COMP;

    /* Set FI_WAIT based on the put and get polling limits defined above */
    if (shmem_transport_ofi_put_poll_limit < 0) {
        cntr_put_attr.wait_obj = FI_WAIT_NONE;
    } else {
        cntr_put_attr.wait_obj = FI_WAIT_UNSPEC;
    }
    if (shmem_transport_ofi_get_poll_limit < 0) {
        cntr_get_attr.wait_obj = FI_WAIT_NONE;
    } else {
        cntr_get_attr.wait_obj = FI_WAIT_UNSPEC;
    }

    /* ------------------------------------------------------- */
    /* Define Completion tracking Resources to Attach to EP    */
    /* ------------------------------------------------------- */

    /* Create counter for counting completions of outgoing writes */

    ret = fi_cntr_open(shmem_transport_ofi_domainfd, &cntr_put_attr,
                       &ctx->endpoint.put_cntrfd, NULL);
    if (ret!=0) {
        RAISE_WARN_STR("put cntr_open failed");
        return ret;
    }

    /* Create counter for counting completions of outbound reads */

    ret = fi_cntr_open(shmem_transport_ofi_domainfd, &cntr_get_attr,
                       &ctx->endpoint.get_cntrfd, NULL);
    if (ret!=0) {
        RAISE_WARN_STR("get cntr_open failed");
        return ret;
    }

    /* Create CQ to be used for NB puts, only context reported */
    cq_attr.format    = FI_CQ_FORMAT_CONTEXT;
    cq_attr.size      = shmem_transport_ofi_queue_slots;

    ret = fi_cq_open(shmem_transport_ofi_domainfd, &cq_attr,
                     &shmem_transport_ofi_put_nb_cqfd, NULL);
    if (ret!=0) {
        RAISE_WARN_STR("cq_open failed");
        return ret;
    }

    return ret;

}

static inline
int allocate_recv_cntr_mr(void)
{

    int ret = 0;

    /* ------------------------------------ */
    /* POST enable resources for to EP      */
    /* ------------------------------------ */

    /* since this is AFTER enable and RMA you must create memory regions for
     * incoming reads/writes and outgoing non-blocking Puts, specifying entire
     * VA range */

#ifndef ENABLE_HARD_POLLING
    {
        struct fi_cntr_attr cntr_attr = {0};

        /* Create counter for incoming writes */
        cntr_attr.events   = FI_CNTR_EVENTS_COMP;
        cntr_attr.wait_obj = FI_WAIT_UNSPEC;

        ret = fi_cntr_open(shmem_transport_ofi_domainfd, &cntr_attr,
                           &shmem_transport_ofi_target_cntrfd, NULL);
        if (ret!=0) {
            RAISE_WARN_STR("target cntr_open failed");
            return ret;
        }
    }
#endif

#if defined(ENABLE_MR_SCALABLE) && defined(ENABLE_REMOTE_VIRTUAL_ADDRESSING)
    ret = fi_mr_reg(shmem_transport_ofi_domainfd, 0, UINT64_MAX,
                    FI_REMOTE_READ | FI_REMOTE_WRITE, 0, 0ULL, 0,
                    &shmem_transport_ofi_target_mrfd, NULL);
    if (ret!=0) {
        RAISE_WARN_STR("mr_reg failed");
        return ret;
    }

    /* Bind counter with target memory region for incoming messages */
#ifndef ENABLE_HARD_POLLING
    ret = fi_mr_bind(shmem_transport_ofi_target_mrfd,
                     &shmem_transport_ofi_target_cntrfd->fid,
                     FI_REMOTE_WRITE | FI_REMOTE_READ);
    if (ret!=0) {
        RAISE_WARN_STR("mr_bind failed");
        return ret;
    }

    ret = fi_ep_bind(shmem_transport_ctx_default.endpoint.cntr_ep,
                     &shmem_transport_ofi_target_cntrfd->fid, FI_REMOTE_WRITE | FI_REMOTE_READ);
    if (ret!=0) {
        RAISE_WARN_STR("ep_bind cntr_epfd2put_cntr failed");
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
        RAISE_WARN_STR("mr_reg heap failed");
        return ret;
    }
    ret = fi_mr_reg(shmem_transport_ofi_domainfd, shmem_internal_data_base,
                    shmem_internal_data_length,
                    FI_REMOTE_READ | FI_REMOTE_WRITE, 0, 0ULL, 0,
                    &shmem_transport_ofi_target_data_mrfd, NULL);
    if (ret != 0) {
        RAISE_WARN_STR("mr_reg data segment failed");
        return ret;
    }

    /* Bind counter with target memory region for incoming messages */
#ifndef ENABLE_HARD_POLLING
    ret = fi_mr_bind(shmem_transport_ofi_target_heap_mrfd,
                     &shmem_transport_ofi_target_cntrfd->fid,
                     FI_REMOTE_WRITE | FI_REMOTE_READ);
    if (ret != 0) {
        RAISE_WARN_STR("mr_bind heap failed");
        return ret;
    }
    ret = fi_mr_bind(shmem_transport_ofi_target_data_mrfd,
                     &shmem_transport_ofi_target_cntrfd->fid,
                     FI_REMOTE_WRITE | FI_REMOTE_READ);
    if (ret != 0) {
        RAISE_WARN_STR("mr_bind data segment failed");
        return ret;
    }

    ret = fi_ep_bind(shmem_transport_ctx_default.endpoint.cntr_ep,
                     &shmem_transport_ofi_target_cntrfd->fid, FI_REMOTE_WRITE | FI_REMOTE_READ);
    if (ret!=0) {
        RAISE_WARN_STR("ep_bind cntr_epfd2put_cntr failed");
        return ret;
    }
#endif /* ndef ENABLE_HARD_POLLING */
#endif

    return ret;
}

static
int publish_mr_info(void)
{
#ifndef ENABLE_MR_SCALABLE
    {
        int err;
        uint64_t heap_key, data_key;

        heap_key = fi_mr_key(shmem_transport_ofi_target_heap_mrfd);
        data_key = fi_mr_key(shmem_transport_ofi_target_data_mrfd);

        err = shmem_runtime_put("fi_heap_key", &heap_key, sizeof(uint64_t));
        if (err) {
            RAISE_WARN_STR("Put of heap key to runtime KVS failed");
            return 1;
        }

        err = shmem_runtime_put("fi_data_key", &data_key, sizeof(uint64_t));
        if (err) {
            RAISE_WARN_STR("Put of data segment key to runtime KVS failed");
            return 1;
        }
    }

#ifndef ENABLE_REMOTE_VIRTUAL_ADDRESSING
    {
        int err;
        err = shmem_runtime_put("fi_heap_addr", &shmem_internal_heap_base, sizeof(uint8_t*));
        if (err) {
            RAISE_WARN_STR("Put of heap address to runtime KVS failed");
            return 1;
        }

        err = shmem_runtime_put("fi_data_addr", &shmem_internal_data_base, sizeof(uint8_t*));
        if (err) {
            RAISE_WARN_STR("Put of data segment address to runtime KVS failed");
            return 1;
        }
    }
#endif /* ENABLE_REMOTE_VIRTUAL_ADDRESSING */
#endif /* ENABLE_MR_SCALABLE */

    return 0;
}

static
int populate_mr_tables(void)
{
#ifndef ENABLE_MR_SCALABLE
    {
        int i, err;

        shmem_transport_ofi_target_heap_keys = malloc(sizeof(uint64_t) * shmem_internal_num_pes);
        if (NULL == shmem_transport_ofi_target_heap_keys) {
            RAISE_WARN_STR("Out of memory allocating heap keytable");
            return 1;
        }

        shmem_transport_ofi_target_data_keys = malloc(sizeof(uint64_t) * shmem_internal_num_pes);
        if (NULL == shmem_transport_ofi_target_data_keys) {
            RAISE_WARN_STR("Out of memory allocating heap keytable");
            return 1;
        }

        /* Called after the upper layer performs the runtime exchange */
        for (i = 0; i < shmem_internal_num_pes; i++) {
            err = shmem_runtime_get(i, "fi_heap_key",
                                    &shmem_transport_ofi_target_heap_keys[i],
                                    sizeof(uint64_t));
            if (err) {
                RAISE_WARN_STR("Get of heap key from runtime KVS failed");
                return 1;
            }
            err = shmem_runtime_get(i, "fi_data_key",
                                    &shmem_transport_ofi_target_data_keys[i],
                                    sizeof(uint64_t));
            if (err) {
                RAISE_WARN_STR("Get of data segment key from runtime KVS failed");
                return 1;
            }
        }
    }

#ifndef ENABLE_REMOTE_VIRTUAL_ADDRESSING
    {
        int i, err;

        shmem_transport_ofi_target_heap_addrs = malloc(sizeof(uint8_t*) * shmem_internal_num_pes);
        if (NULL == shmem_transport_ofi_target_heap_addrs) {
            RAISE_WARN_STR("Out of memory allocating heap addrtable");
            return 1;
        }

        shmem_transport_ofi_target_data_addrs = malloc(sizeof(uint8_t*) * shmem_internal_num_pes);
        if (NULL == shmem_transport_ofi_target_data_addrs) {
            RAISE_WARN_STR("Out of memory allocating data addrtable");
            return 1;
        }

        /* Called after the upper layer performs the runtime exchange */
        for (i = 0; i < shmem_internal_num_pes; i++) {
            err = shmem_runtime_get(i, "fi_heap_addr",
                                    &shmem_transport_ofi_target_heap_addrs[i],
                                    sizeof(uint8_t*));
            if (err) {
                RAISE_WARN_STR("Get of heap address from runtime KVS failed");
                return 1;
            }
            err = shmem_runtime_get(i, "fi_data_addr",
                                    &shmem_transport_ofi_target_data_addrs[i],
                                    sizeof(uint8_t*));
            if (err) {
                RAISE_WARN_STR("Get of data segment address from runtime KVS failed");
                return 1;
            }
        }
    }
#endif /* ENABLE_REMOTE_VIRTUAL_ADDRESSING */
#endif /* ENABLE_MR_SCALABLE */

    return 0;
}

/* SOFT_SUPPORT will not produce warning or error */
static inline
int atomicvalid_rtncheck(int ret, int atomic_size,
                         atomic_support_lv atomic_sup,
                         char strOP[], char strDT[])
{
    if ((ret != 0 || atomic_size == 0) && atomic_sup != ATOMIC_SOFT_SUPPORT) {
        RAISE_WARN_MSG("Provider does not support atomic '%s' "
                       "on type '%s' (%d, %d)\n", strOP, strDT, ret, atomic_size);

        if (atomic_sup != ATOMIC_WARNINGS) {
            return ret ? ret : -1;
        }
    }

    return 0;
}

static inline
int atomicvalid_DTxOP(int DT_MAX, int OPS_MAX, int DT[], int OPS[],
                      atomic_support_lv atomic_sup)
{
    int i, j, ret = 0;
    size_t atomic_size;

    for(i=0; i<DT_MAX; i++) {
        for(j=0; j<OPS_MAX; j++) {
            ret = fi_atomicvalid(shmem_transport_ctx_default.endpoint.ep, DT[i],
                                 OPS[j], &atomic_size);
            if (atomicvalid_rtncheck(ret, atomic_size, atomic_sup,
                                     SHMEM_OpName[OPS[j]],
                                     SHMEM_DtName[DT[i]]))
                return ret;
        }
    }

    return 0;
}

static inline
int compare_atomicvalid_DTxOP(int DT_MAX, int OPS_MAX, int DT[],
                              int OPS[], atomic_support_lv atomic_sup)
{
    int i, j, ret = 0;
    size_t atomic_size;

    for(i=0; i<DT_MAX; i++) {
        for(j=0; j<OPS_MAX; j++) {
            ret = fi_compare_atomicvalid(shmem_transport_ctx_default.endpoint.ep, DT[i],
                                         OPS[j], &atomic_size);
            if (atomicvalid_rtncheck(ret, atomic_size, atomic_sup,
                                     SHMEM_OpName[OPS[j]],
                                     SHMEM_DtName[DT[i]]))
                return ret;
        }
    }

    return 0;
}

static inline
int fetch_atomicvalid_DTxOP(int DT_MAX, int OPS_MAX, int DT[], int OPS[],
                            atomic_support_lv atomic_sup)
{
    int i, j, ret = 0;
    size_t atomic_size;

    for(i=0; i<DT_MAX; i++) {
        for(j=0; j<OPS_MAX; j++) {
            ret = fi_fetch_atomicvalid(shmem_transport_ctx_default.endpoint.ep, DT[i],
                                       OPS[j], &atomic_size);
            if (atomicvalid_rtncheck(ret, atomic_size, atomic_sup,
                                     SHMEM_OpName[OPS[j]],
                                     SHMEM_DtName[DT[i]]))
                return ret;
        }
    }

    return 0;
}

static inline
int atomic_limitations_check(void)
{
    /* Retrieve messaging limitations from OFI
     *
     * NOTE: Currently only have reduction software atomic support. User can
     * optionally request for warnings if other atomic limitations are detected
     */

    int ret = 0;
    atomic_support_lv general_atomic_sup = ATOMIC_NO_SUPPORT;
    atomic_support_lv reduction_sup = ATOMIC_SOFT_SUPPORT;

    if (shmem_internal_params.OFI_ATOMIC_CHECKS_WARN)
        general_atomic_sup = ATOMIC_WARNINGS;

    init_ofi_tables();

    /* Standard OPS check */
    ret = atomicvalid_DTxOP(SIZEOF_AMO_DT, SIZEOF_AMO_OPS, DT_AMO_STANDARD,
                            AMO_STANDARD_OPS, general_atomic_sup);
    if (ret)
        return ret;

    ret = fetch_atomicvalid_DTxOP(SIZEOF_AMO_DT, SIZEOF_AMO_FOPS,
                                  DT_AMO_STANDARD, FETCH_AMO_STANDARD_OPS,
                                  general_atomic_sup);
    if (ret)
        return ret;

    ret = compare_atomicvalid_DTxOP(SIZEOF_AMO_DT, SIZEOF_AMO_COPS,
                                    DT_AMO_STANDARD, COMPARE_AMO_STANDARD_OPS,
                                    general_atomic_sup);
    if (ret)
        return ret;

    /* Extended OPS check */
    ret = atomicvalid_DTxOP(SIZEOF_AMO_EX_DT, SIZEOF_AMO_EX_OPS, DT_AMO_EXTENDED,
                            AMO_EXTENDED_OPS, general_atomic_sup);
    if (ret)
        return ret;

    ret = fetch_atomicvalid_DTxOP(SIZEOF_AMO_EX_DT, SIZEOF_AMO_EX_FOPS,
                                  DT_AMO_EXTENDED, FETCH_AMO_EXTENDED_OPS,
                                  general_atomic_sup);
    if (ret)
        return ret;

    /* Reduction OPS check */
    ret = atomicvalid_DTxOP(SIZEOF_RED_DT, SIZEOF_RED_OPS, DT_REDUCE_BITWISE,
                            REDUCE_BITWISE_OPS, reduction_sup);
    if (ret)
        return ret;

    ret = atomicvalid_DTxOP(SIZEOF_REDC_DT, SIZEOF_REDC_OPS, DT_REDUCE_COMPARE,
                            REDUCE_COMPARE_OPS, reduction_sup);
    if (ret)
        return ret;

    ret = atomicvalid_DTxOP(SIZEOF_REDA_DT, SIZEOF_REDA_OPS, DT_REDUCE_ARITH,
                            REDUCE_ARITH_OPS, reduction_sup);
    if (ret)
        return ret;

    /* Internal atomic requirement */
    ret = compare_atomicvalid_DTxOP(SIZEOF_INTERNAL_REQ_DT, SIZEOF_INTERNAL_REQ_OPS,
                                    DT_INTERNAL_REQ, INTERNAL_REQ_OPS,
                                    general_atomic_sup);
    if (ret)
        return ret;

    return 0;
}

static inline
int publish_av_info(struct fabric_info *info)
{
    int    ret = 0;
    char   epname[128];
    size_t epnamelen = sizeof(epname);

#ifdef USE_ON_NODE_COMMS
    if (gethostname(myephostname, (EPHOSTNAMELEN - 1)) != 0)
        RAISE_ERROR_MSG("gethostname error: %s \n", strerror(errno));

    myephostname[EPHOSTNAMELEN-1] = '\0';

    ret = shmem_runtime_put("fi_ephostname", myephostname, EPHOSTNAMELEN);
    if (ret != 0) {
        RAISE_WARN_STR("shmem_runtime_put ephostname failed");
        return ret;
    }
#endif

    ret = fi_getname((fid_t)shmem_transport_ctx_default.endpoint.ep, epname, &epnamelen);
    if (ret!=0 || (epnamelen > sizeof(epname))) {
        RAISE_WARN_STR("fi_getname failed");
        return ret;
    }

    ret = shmem_runtime_put("fi_epname", epname, epnamelen);
    if (ret != 0) {
        RAISE_WARN_STR("shmem_runtime_put epname failed");
        return ret;
    }

    /* Note: we assume that the length of an address is the same for all
     * endpoints.  This is safe for most HPC systems, but could be incorrect in
     * a heterogeneous context. */
    shmem_transport_ofi_addrlen = epnamelen;

    return ret;
}

static inline
int populate_av(void)
{
    int    i, ret = 0;
    char   *alladdrs = NULL;
#ifdef USE_ON_NODE_COMMS
    int    num_on_node = 0;
    char   ephostname[EPHOSTNAMELEN];
#endif

    alladdrs = malloc(shmem_internal_num_pes * shmem_transport_ofi_addrlen);
    if (alladdrs == NULL) {
        RAISE_WARN_STR("Out of memory allocating 'alladdrs'");
        return ret;
    }

    for (i = 0; i < shmem_internal_num_pes; i++) {
        char *addr_ptr = alladdrs + i * shmem_transport_ofi_addrlen;
        shmem_runtime_get(i, "fi_epname", addr_ptr, shmem_transport_ofi_addrlen);

#ifdef USE_ON_NODE_COMMS
        shmem_runtime_get(i, "fi_ephostname", ephostname, EPHOSTNAMELEN);
        if (strncmp(myephostname, ephostname, EPHOSTNAMELEN) == 0) {
            SHMEM_SET_RANK_SAME_NODE(i, num_on_node++);
            if (num_on_node > 255) {
                RAISE_WARN_STR("Number of local ranks exceeds limit of 255");
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
        RAISE_WARN_STR("av insert failed");
        return ret;
    }

    free(alladdrs);

    return 0;
}

static inline
int allocate_fabric_resources(struct fabric_info *info)
{
    int ret = 0;
    struct fi_av_attr   av_attr = {0};


    /* fabric domain: define domain of resources physical and logical */
    ret = fi_fabric(info->p_info->fabric_attr, &shmem_transport_ofi_fabfd, NULL);
    if (ret!=0) {
        RAISE_WARN_STR("fabric initialization failed");
        return ret;
    }

    DEBUG_MSG("OFI version: built %"PRIu32".%"PRIu32", cur. %"PRIu32".%"PRIu32"; "
              "provider version: %"PRIu32".%"PRIu32"\n",
              FI_MAJOR_VERSION, FI_MINOR_VERSION,
              FI_MAJOR(fi_version()), FI_MINOR(fi_version()),
              FI_MAJOR(info->p_info->fabric_attr->prov_version),
              FI_MINOR(info->p_info->fabric_attr->prov_version));

    /* access domain: define communication resource limits/boundary within
     * fabric domain */
    ret = fi_domain(shmem_transport_ofi_fabfd, info->p_info,
                    &shmem_transport_ofi_domainfd,NULL);
    if (ret!=0) {
        RAISE_WARN_STR("domain initialization failed");
        return ret;
    }

    /* transmit context: allocate one transmit context for this SHMEM PE
     * and share it across different multiple endpoints. Since we have only
     * one thread per PE, a single context is sufficient and allows more
     * more PEs/node (i.e. doesn't exhaust contexts)  */
    ret = fi_stx_context(shmem_transport_ofi_domainfd, NULL, /* TODO: fill tx_attr */
                         &shmem_transport_ofi_stx, NULL);
    if (ret!=0) {
        RAISE_WARN_STR("stx context initialization failed");
        return ret;
    }

    /* AV table set-up for PE mapping */

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
    if (ret!=0) {
        RAISE_WARN_STR("av open failed");
        return ret;
    }

    return ret;
}

static inline
int query_for_fabric(struct fabric_info *info)
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
    domain_attr.data_progress = FI_PROGRESS_AUTO;
    domain_attr.resource_mgmt = FI_RM_ENABLED;
#ifdef ENABLE_MR_SCALABLE
    domain_attr.mr_mode       = FI_MR_SCALABLE; /* VA space-doesn't have to be pre-allocated */
#else
    domain_attr.mr_mode       = FI_MR_BASIC; /* VA space is pre-allocated */
#endif
#if !defined(ENABLE_MR_SCALABLE) || !defined(ENABLE_REMOTE_VIRTUAL_ADDRESSING)
    domain_attr.mr_key_size   = 1; /* Heap and data use different MR keys, need
                                      at least 1 byte */
#endif
#ifdef ENABLE_THREADS
    if (shmem_internal_thread_level == SHMEM_THREAD_MULTIPLE)
        domain_attr.threading = FI_THREAD_SAFE;
    else
        domain_attr.threading = FI_THREAD_DOMAIN;
#else
    domain_attr.threading     = FI_THREAD_DOMAIN;
#endif

    hints.domain_attr         = &domain_attr;
    ep_attr.type              = FI_EP_RDM; /* reliable connectionless */
    hints.fabric_attr         = &fabric_attr;
    tx_attr.op_flags          = FI_DELIVERY_COMPLETE;
    tx_attr.inject_size       = shmem_transport_ofi_max_buffered_send; /* require provider to support this as a min */
    hints.tx_attr             = &tx_attr; /* TODO: fill tx_attr */
    hints.rx_attr             = NULL;
    hints.ep_attr             = &ep_attr;

    /* find fabric provider to use that is able to support RMA and ATOMICS */
    ret = fi_getinfo( FI_VERSION(OFI_MAJOR_VERSION, OFI_MINOR_VERSION),
                      NULL, NULL, 0, &hints, &(info->fabrics));

    if (ret!=0) {
        RAISE_WARN_MSG("OFI transport did not find any valid fabric services (provider=%s)\n",
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

    if (NULL == info->p_info) {
        RAISE_WARN_MSG("OFI transport, no valid fabric (prov=%s, fabric=%s, domain=%s)\n",
                       info->prov_name != NULL ? info->prov_name : "<auto>",
                       info->fabric_name != NULL ? info->fabric_name : "<auto>",
                       info->domain_name != NULL ? info->domain_name : "<auto>");
        return ret;
    }

    if (info->p_info->ep_attr->max_msg_size > 0) {
        shmem_transport_ofi_max_msg_size = info->p_info->ep_attr->max_msg_size;
    } else {
        RAISE_WARN_STR("OFI provider did not set max_msg_size");
        return 1;
    }

#if defined(ENABLE_MR_SCALABLE) && defined(ENABLE_REMOTE_VIRTUAL_ADDRESSING)
    /* Only use a single MR, no keys required */
    info->p_info->domain_attr->mr_key_size = 0;
#else
    /* Heap and data use different MR keys, need at least 1 byte */
    info->p_info->domain_attr->mr_key_size = 1;
#endif

    shmem_internal_assertp(info->p_info->tx_attr->inject_size >= shmem_transport_ofi_max_buffered_send);
    shmem_transport_ofi_max_buffered_send = info->p_info->tx_attr->inject_size;

    DEBUG_MSG("OFI provider: %s, fabric: %s, domain: %s\n",
              info->p_info->fabric_attr->prov_name,
              info->p_info->fabric_attr->name, info->p_info->domain_attr->name);

    return ret;
}

static int shmem_transport_ofi_ctx_init(shmem_transport_ctx_t *ctx, int id)
{
    int ret = 0;

    struct fi_cntr_attr cntr_attr = {0};
    cntr_attr.events   = FI_CNTR_EVENTS_COMP;
    /* Set FI_WAIT based on the put and get polling limits defined above */
    if (shmem_transport_ofi_put_poll_limit < 0) {
        cntr_attr.wait_obj = FI_WAIT_NONE;
    } else {
        cntr_attr.wait_obj = FI_WAIT_UNSPEC;
    }

    struct fabric_info* info = &shmem_transport_ofi_info;
    info->p_info->ep_attr->tx_ctx_cnt = FI_SHARED_CONTEXT;
    info->p_info->caps = FI_RMA | FI_WRITE | FI_READ | /*SEND ONLY */
        FI_ATOMICS; /* request atomics capability */
    info->p_info->caps |= FI_REMOTE_WRITE | FI_REMOTE_READ;
    info->p_info->tx_attr->op_flags = FI_DELIVERY_COMPLETE | FI_INJECT_COMPLETE;
    info->p_info->mode = 0;
    info->p_info->tx_attr->mode = 0;
    info->p_info->rx_attr->mode = 0;

    ctx->id = id;
    shmem_transport_cntr_ep_t* ep = &ctx->endpoint;
    shmem_internal_atomic_write(&ep->pending_put_counter, 0);
    shmem_internal_atomic_write(&ep->pending_get_counter, 0);

    ret = fi_cntr_open(shmem_transport_ofi_domainfd, &cntr_attr,
                       &ep->put_cntrfd, NULL);
    if (ret!=0) {
        RAISE_ERROR_MSG("context cntr_open failed (%s)\n", fi_strerror(errno));
        return ret;
    }

    ret = fi_cntr_open(shmem_transport_ofi_domainfd, &cntr_attr,
                       &ep->get_cntrfd, NULL);
    if (ret!=0) {
        RAISE_ERROR_MSG("context cntr_open failed (%s)\n", fi_strerror(errno));
        return ret;
    }

    ret = allocate_endpoints(ctx, info);
    if (ret!=0) {
        RAISE_ERROR_MSG("ctx allocate endpoints failed (%s)\n", fi_strerror(errno));
        return ret;
    }

    ret = bind_resources_to_and_enable_ep(ctx);
    if (ret!=0) {
        RAISE_ERROR_MSG("context bind/enable resources failed (%s)\n", fi_strerror(errno));
        return ret;
    }

    return 0;
}


int shmem_transport_init(void)
{
    int ret = 0;

    shmem_transport_ofi_info.npes      = shmem_runtime_get_size();

    if (shmem_internal_params.OFI_PROVIDER_provided)
        shmem_transport_ofi_info.prov_name = shmem_internal_params.OFI_PROVIDER;
    if (shmem_internal_params.OFI_USE_PROVIDER_provided)
        shmem_transport_ofi_info.prov_name = shmem_internal_params.OFI_USE_PROVIDER;
    else
        shmem_transport_ofi_info.prov_name = NULL;

    if (shmem_internal_params.OFI_FABRIC_provided)
        shmem_transport_ofi_info.fabric_name = shmem_internal_params.OFI_FABRIC;
    else
        shmem_transport_ofi_info.fabric_name = NULL;

    if (shmem_internal_params.OFI_DOMAIN_provided)
        shmem_transport_ofi_info.domain_name = shmem_internal_params.OFI_DOMAIN;
    else
        shmem_transport_ofi_info.domain_name = NULL;


    ret = query_for_fabric(&shmem_transport_ofi_info);
    if (ret!=0)
        return ret;

    ret = allocate_fabric_resources(&shmem_transport_ofi_info);
    if(ret!=0)
        return ret;

    shmem_transport_ofi_avail_ctx = shmem_transport_ofi_grow_size;

    shmem_transport_ofi_contexts = malloc(shmem_transport_ofi_avail_ctx
                                          * sizeof(shmem_transport_ctx_t*));

    /* The current bounce buffering implementation is only compatible with
     * providers that don't require FI_CONTEXT */
    if (shmem_transport_ofi_info.p_info->mode & FI_CONTEXT) {
        if (shmem_internal_my_pe == 0 && shmem_internal_params.BOUNCE_SIZE > 0) {
            DEBUG_STR("OFI provider requires FI_CONTEXT; disabling bounce buffering");
        }
        shmem_transport_ofi_bounce_buffer_size = 0;
        shmem_transport_ofi_bounce_buffers = NULL;
    } else {
        shmem_transport_ofi_bounce_buffer_size = shmem_internal_params.BOUNCE_SIZE;
        shmem_transport_ofi_bounce_buffers =
            shmem_free_list_init(sizeof(shmem_transport_ofi_bounce_buffer_t) +
                                 shmem_transport_ofi_bounce_buffer_size,
                                 init_bounce_buffer);
    }

    shmem_transport_ofi_put_poll_limit = shmem_internal_params.OFI_TX_POLL_LIMIT;
    shmem_transport_ofi_get_poll_limit = shmem_internal_params.OFI_RX_POLL_LIMIT;

    //ret = shmem_transport_ofi_ctx_init(-1, &shmem_transport_ctx_default);
    //if (ret!=0)
    //    return ret;

    ret = allocate_endpoints(&shmem_transport_ctx_default, &shmem_transport_ofi_info);
    if (ret!=0)
        return ret;

    ret = allocate_cntr_and_cq(&shmem_transport_ctx_default);
    if (ret!=0)
        return ret;

    ret = bind_resources_to_and_enable_ep(&shmem_transport_ctx_default);
    if (ret!=0)
        return ret;

    ret = allocate_recv_cntr_mr();
    if (ret!=0)
        return ret;

    ret = publish_mr_info();
    if (ret != 0)
        return ret;

    ret = atomic_limitations_check();
    if (ret!=0)
        return ret;

    ret = publish_av_info(&shmem_transport_ofi_info);
    if (ret!=0)
        return ret;

    shmem_internal_atomic_write(&shmem_transport_ctx_default.endpoint.pending_put_counter, 0);
    shmem_internal_atomic_write(&shmem_transport_ctx_default.endpoint.pending_get_counter, 0);
    shmem_internal_atomic_write(&shmem_transport_ofi_pending_cq_count, 0);

    return 0;
}

int shmem_transport_startup(void)
{
    int ret;

    ret = populate_mr_tables();
    if (ret != 0)
        return ret;

    ret = populate_av();
    if (ret!=0)
        return ret;

    return 0;
}

int shmem_transport_ctx_create(shmem_transport_ctx_t **ctx)
{
    SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);

    int ret;
    int id = shmem_transport_ofi_num_ctx;

    /* FIXME: This does not do resource cleanup 
     * on error, it just returns. This is consistent with how
     * initialization worked before, but is worse since context creation
     * is not necessarily do-or-die.
     */
    /* JD: This needs to have a mutex around it to synchronize a possible
     * update to the contexts array.
     *
     * This also does not reuse entries from contexts that were freed.
     */
    if (shmem_transport_ofi_num_ctx == shmem_transport_ofi_avail_ctx) {
        shmem_transport_ofi_avail_ctx += shmem_transport_ofi_grow_size;
        shmem_transport_ofi_contexts = realloc(shmem_transport_ofi_contexts,
               shmem_transport_ofi_avail_ctx * sizeof(shmem_transport_ctx_t*));

        if (shmem_transport_ofi_contexts == NULL) {
            RAISE_ERROR_STR("Error: out of memory when allocating OFI ctx array");
        }
    }

    shmem_transport_ctx_t *ctxp = malloc(sizeof(shmem_transport_ctx_t));

    if (ctxp == NULL) {
        RAISE_ERROR_STR("Error: out of memory when allocating OFI ctx object");
    }

    ret = shmem_transport_ofi_ctx_init(ctxp, id);

    if (ret) {
        shmem_transport_ofi_contexts[id] = NULL;
        free(ctxp);
    } else {
        shmem_transport_ofi_contexts[id] = ctxp;
        shmem_transport_ofi_num_ctx++;
        *ctx = ctxp;
    }

    SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
    return ret;

}

void shmem_transport_ctx_destroy(shmem_transport_ctx_t *ctx)
{
    shmem_transport_quiet(ctx);

    if (fi_close(&ctx->endpoint.ep->fid)) {
        RAISE_ERROR_MSG("Context endpoint close failed (%s)\n", fi_strerror(errno));
    }
    if (fi_close(&ctx->endpoint.cntr_ep->fid)) {
        RAISE_ERROR_MSG("Context cntr endpoint close failed (%s)\n", fi_strerror(errno));
    }
    if (fi_close(&ctx->endpoint.put_cntrfd->fid)) {
        RAISE_ERROR_MSG("Context counter close failed (%s)\n", fi_strerror(errno));
    }
    if (fi_close(&ctx->endpoint.get_cntrfd->fid)) {
        RAISE_ERROR_MSG("Context counter close failed (%s)\n", fi_strerror(errno));
    }

    if (ctx->id >= 0) {
        SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);
        shmem_transport_ofi_contexts[ctx->id] = NULL;
        SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
        free(ctx);
    }
}

int shmem_transport_fini(void)
{
    /* Wait for acks before shutdown */
    size_t i;
    for(i = 0; i < shmem_transport_ofi_num_ctx; ++i) {
        shmem_transport_ctx_t* ctx = shmem_transport_ofi_contexts[i];
        if(ctx) {
            shmem_transport_ctx_destroy(ctx);
            if (shmem_transport_ofi_contexts[i] != NULL) {
                RAISE_ERROR_MSG("Unable to free ctx %zu", i);
            }
        }
    }

    //shmem_transport_ctx_destroy(&shmem_transport_ctx_default);
    shmem_transport_quiet(&shmem_transport_ctx_default);

    if (shmem_transport_ctx_default.endpoint.ep &&
        fi_close(&shmem_transport_ctx_default.endpoint.ep->fid)) {
        RAISE_ERROR_MSG("Endpoint close failed (%s)\n", fi_strerror(errno));
    }

    if (shmem_transport_ctx_default.endpoint.cntr_ep &&
        fi_close(&shmem_transport_ctx_default.endpoint.cntr_ep->fid)) {
        RAISE_ERROR_MSG("Endpoint close failed (%s)\n", fi_strerror(errno));
    }

    if (shmem_transport_ofi_stx &&
        fi_close(&shmem_transport_ofi_stx->fid)) {
        RAISE_ERROR_MSG("Shared context close failed (%s)\n", fi_strerror(errno));
    }

#if defined(ENABLE_MR_SCALABLE) && defined(ENABLE_REMOTE_VIRTUAL_ADDRESSING)
    if (shmem_transport_ofi_target_mrfd &&
        fi_close(&shmem_transport_ofi_target_mrfd->fid)) {
        RAISE_ERROR_MSG("Target MR close failed (%s)\n", fi_strerror(errno));
    }
#else
    if (shmem_transport_ofi_target_heap_mrfd &&
        fi_close(&shmem_transport_ofi_target_heap_mrfd->fid)) {
        RAISE_ERROR_MSG("Target heap MR close failed (%s)\n", fi_strerror(errno));
    }

    if (shmem_transport_ofi_target_data_mrfd &&
        fi_close(&shmem_transport_ofi_target_data_mrfd->fid)) {
        RAISE_ERROR_MSG("Target data MR close failed (%s)\n", fi_strerror(errno));
    }
#endif

    if (shmem_transport_ofi_put_nb_cqfd &&
        fi_close(&shmem_transport_ofi_put_nb_cqfd->fid)) {
        RAISE_ERROR_MSG("Write CQ close failed (%s)\n", fi_strerror(errno));
    }

    if (shmem_transport_ctx_default.endpoint.put_cntrfd &&
        fi_close(&shmem_transport_ctx_default.endpoint.put_cntrfd->fid)) {
        RAISE_ERROR_MSG("INJECT PUT CT close failed (%s)\n", fi_strerror(errno));
    }

    if (shmem_transport_ctx_default.endpoint.get_cntrfd &&
        fi_close(&shmem_transport_ctx_default.endpoint.get_cntrfd->fid)) {
        RAISE_ERROR_MSG("GET CT close failed (%s)\n", fi_strerror(errno));
    }

#ifndef ENABLE_HARD_POLLING
    if (shmem_transport_ofi_target_cntrfd &&
        fi_close(&shmem_transport_ofi_target_cntrfd->fid)) {
        RAISE_ERROR_MSG("Target CT close failed (%s)\n", fi_strerror(errno));
    }
#endif

    if (shmem_transport_ofi_avfd &&
        fi_close(&shmem_transport_ofi_avfd->fid)) {
        RAISE_ERROR_MSG("AV close failed (%s)\n", fi_strerror(errno));
    }

    if (shmem_transport_ofi_domainfd &&
        fi_close(&shmem_transport_ofi_domainfd->fid)) {
        RAISE_ERROR_MSG("Domain close failed (%s)\n", fi_strerror(errno));
    }

    if (shmem_transport_ofi_fabfd &&
        fi_close(&shmem_transport_ofi_fabfd->fid)) {
        RAISE_ERROR_MSG("Fabric close failed (%s)\n", fi_strerror(errno));
    }

    if (NULL != shmem_transport_ofi_bounce_buffers) {
        shmem_free_list_destroy(shmem_transport_ofi_bounce_buffers);
    }

#ifdef USE_AV_MAP
    free(addr_table);
#endif

    fi_freeinfo(shmem_transport_ofi_info.fabrics);

    return 0;
}
