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
#include <sys/types.h>

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
struct fid_av*                  shmem_transport_ofi_avfd;
struct fid_ep*                  shmem_transport_ofi_target_ep;
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
uint64_t                        shmem_transport_ofi_max_poll;
long                            shmem_transport_ofi_put_poll_limit;
long                            shmem_transport_ofi_get_poll_limit;
size_t                          shmem_transport_ofi_max_buffered_send;
size_t                          shmem_transport_ofi_max_msg_size;
size_t                          shmem_transport_ofi_bounce_buffer_size;
long                            shmem_transport_ofi_max_bounce_buffers;
size_t                          shmem_transport_ofi_addrlen;
#ifdef ENABLE_MR_RMA_EVENT
int                             shmem_transport_ofi_mr_rma_event;
#endif
fi_addr_t                       *addr_table;
#ifdef USE_ON_NODE_COMMS
#define EPHOSTNAMELEN  _POSIX_HOST_NAME_MAX + 1
static char                     myephostname[EPHOSTNAMELEN];
#endif
#ifdef ENABLE_THREADS
shmem_internal_mutex_t          shmem_transport_ofi_lock;
/* Need a syscall to gettid() because glibc doesn't provide a wrapper
 * (see gettid manpage in the NOTES section): */
    #ifndef __APPLE__
        #include <sys/syscall.h>
        #ifdef SYS_gettid
        #define gettid() syscall(SYS_gettid)
        #else
        #warning "SYS_gettid not available"
        #endif
    #endif
#endif

struct fabric_info shmem_transport_ofi_info = {0};

static shmem_transport_ctx_t** shmem_transport_ofi_contexts = NULL;
static size_t shmem_transport_ofi_contexts_len = 0;
static size_t shmem_transport_ofi_grow_size = 128;

shmem_transport_ctx_t shmem_transport_ctx_default;
shmem_ctx_t SHMEM_CTX_DEFAULT = (shmem_ctx_t) &shmem_transport_ctx_default;

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


/* default CQ depth */
uint64_t shmem_transport_ofi_max_poll = (1ULL<<30);


/* STX control variables */
enum stx_share_alg_t {
    ROUNDROBIN = 0,
    RANDOM
};
typedef enum stx_share_alg_t stx_share_alg_t;
static stx_share_alg_t shmem_transport_ofi_stx_share_alg;

static long shmem_transport_ofi_stx_max;

struct shmem_transport_ofi_stx_t {
    struct fid_stx*   stx;
    long              ref_cnt;
    bool              private;   /* true if private, false if shared */
#ifdef __APPLE__
    uint64_t          owner_tid;
#else
    pid_t             owner_tid;
#endif
};
typedef struct shmem_transport_ofi_stx_t shmem_transport_ofi_stx_t;
static shmem_transport_ofi_stx_t* shmem_transport_ofi_stx_pool;


#define OFI_MAJOR_VERSION 1
#ifdef ENABLE_MR_RMA_EVENT
#define OFI_MINOR_VERSION 5
#else
#define OFI_MINOR_VERSION 0
#endif

static
void init_bounce_buffer(shmem_free_list_item_t *item)
{
    shmem_transport_ofi_frag_t *frag =
        (shmem_transport_ofi_frag_t*) item;
    frag->mytype = SHMEM_TRANSPORT_OFI_TYPE_BOUNCE;
}


static inline
int bind_enable_cq_ep_resources(shmem_transport_ctx_t *ctx)
{
    int ret = 0;

    /* Attach the shared context */
    ret = fi_ep_bind(ctx->cq_ep, &shmem_transport_ofi_stx_pool[ctx->stx_idx].stx->fid, 0);
    OFI_CHECK_RETURN_STR(ret, "fi_ep_bind STX to CQ endpoint failed");

    /* Attach CQ for obtaining completions for buffered puts */
    ret = fi_ep_bind(ctx->cq_ep, &ctx->cq->fid, FI_SEND);
    OFI_CHECK_RETURN_STR(ret, "fi_ep_bind CQ to CQ endpoint failed");

    /* Attach the address vector */
    ret = fi_ep_bind(ctx->cq_ep, &shmem_transport_ofi_avfd->fid, 0);
    OFI_CHECK_RETURN_STR(ret, "fi_ep_bind AV to CQ endpoint failed");

    ret = fi_enable(ctx->cq_ep);
    OFI_CHECK_RETURN_STR(ret, "fi_enable on CQ endpoint failed");

    return ret;
}


static inline
int bind_enable_cntr_ep_resources(shmem_transport_ctx_t *ctx)
{
    int ret = 0;

    /* Attach the shared context */
    ret = fi_ep_bind(ctx->cntr_ep, &shmem_transport_ofi_stx_pool[ctx->stx_idx].stx->fid, 0);
    OFI_CHECK_RETURN_STR(ret, "fi_ep_bind STX to CNTR endpoint failed");

    shmem_transport_ofi_stx_pool[ctx->stx_idx].ref_cnt += 1;

    /* Attach counter for obtaining put completions */
    ret = fi_ep_bind(ctx->cntr_ep, &ctx->put_cntr->fid, FI_WRITE);
    OFI_CHECK_RETURN_STR(ret, "fi_ep_bind put CNTR to CNTR endpoint failed");

    /* Attach counter for obtaining get completions */
    ret = fi_ep_bind(ctx->cntr_ep, &ctx->get_cntr->fid, FI_READ);
    OFI_CHECK_RETURN_STR(ret, "fi_ep_bind get CNTR to CNTR endpoint failed");

    /* Attach CQ for error handling */
    ret = fi_ep_bind(ctx->cntr_ep, &ctx->cq->fid,
                     FI_SELECTIVE_COMPLETION | FI_TRANSMIT);
    OFI_CHECK_RETURN_STR(ret, "fi_ep_bind CQ to CNTR endpoint failed");

    /* Attach the address vector */
    ret = fi_ep_bind(ctx->cntr_ep, &shmem_transport_ofi_avfd->fid, 0);
    OFI_CHECK_RETURN_STR(ret, "fi_ep_bind AV to CNTR endpoint failed");

    ret = fi_enable(ctx->cntr_ep);
    OFI_CHECK_RETURN_STR(ret, "fi_enable on CNTR endpoint failed");

    return ret;
}


static inline
int allocate_recv_cntr_mr(void)
{
    int ret = 0;
    uint64_t flags = 0;

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
        OFI_CHECK_RETURN_STR(ret, "target CNTR open failed");

#ifdef ENABLE_MR_RMA_EVENT
        if (shmem_transport_ofi_mr_rma_event)
            flags |= FI_RMA_EVENT;
#endif /* ENABLE_MR_RMA_EVENT */
    }
#endif

#if defined(ENABLE_MR_SCALABLE) && defined(ENABLE_REMOTE_VIRTUAL_ADDRESSING)
    ret = fi_mr_reg(shmem_transport_ofi_domainfd, 0, UINT64_MAX,
                    FI_REMOTE_READ | FI_REMOTE_WRITE, 0, 0ULL, flags,
                    &shmem_transport_ofi_target_mrfd, NULL);
    OFI_CHECK_RETURN_STR(ret, "target memory (all) registration failed");

    /* Bind counter with target memory region for incoming messages */
#ifndef ENABLE_HARD_POLLING
    ret = fi_mr_bind(shmem_transport_ofi_target_mrfd,
                     &shmem_transport_ofi_target_cntrfd->fid,
                     FI_REMOTE_WRITE | FI_REMOTE_READ);
    OFI_CHECK_RETURN_STR(ret, "target CNTR binding to MR failed");

    ret = fi_ep_bind(shmem_transport_ofi_target_ep,
                     &shmem_transport_ofi_target_cntrfd->fid, FI_REMOTE_WRITE | FI_REMOTE_READ);
    OFI_CHECK_RETURN_STR(ret, "target CNTR binding to EP failed");

#ifdef ENABLE_MR_RMA_EVENT
    if (shmem_transport_ofi_mr_rma_event) {
        ret = fi_mr_enable(shmem_transport_ofi_target_mrfd);
        OFI_CHECK_RETURN_STR(ret, "target MR enable failed");
    }
#endif /* ENABLE_MR_RMA_EVENT */
#endif /* ndef ENABLE_HARD_POLLING */

#else
    /* Register separate data and heap segments using keys 0 and 1,
     * respectively.  In MR_BASIC_MODE, the keys are ignored and selected by
     * the provider. */
    ret = fi_mr_reg(shmem_transport_ofi_domainfd, shmem_internal_heap_base,
                    shmem_internal_heap_length,
                    FI_REMOTE_READ | FI_REMOTE_WRITE, 0, 1ULL, flags,
                    &shmem_transport_ofi_target_heap_mrfd, NULL);
    OFI_CHECK_RETURN_STR(ret, "target memory (heap) registration failed");

    ret = fi_mr_reg(shmem_transport_ofi_domainfd, shmem_internal_data_base,
                    shmem_internal_data_length,
                    FI_REMOTE_READ | FI_REMOTE_WRITE, 0, 0ULL, flags,
                    &shmem_transport_ofi_target_data_mrfd, NULL);
    OFI_CHECK_RETURN_STR(ret, "target memory (data) registration failed");

    /* Bind counter with target memory region for incoming messages */
#ifndef ENABLE_HARD_POLLING
    ret = fi_mr_bind(shmem_transport_ofi_target_heap_mrfd,
                     &shmem_transport_ofi_target_cntrfd->fid,
                     FI_REMOTE_WRITE | FI_REMOTE_READ);
    OFI_CHECK_RETURN_STR(ret, "target CNTR binding to heap MR failed");

    ret = fi_mr_bind(shmem_transport_ofi_target_data_mrfd,
                     &shmem_transport_ofi_target_cntrfd->fid,
                     FI_REMOTE_WRITE | FI_REMOTE_READ);
    OFI_CHECK_RETURN_STR(ret, "target CNTR binding to data MR failed");

    ret = fi_ep_bind(shmem_transport_ofi_target_ep,
                     &shmem_transport_ofi_target_cntrfd->fid, FI_REMOTE_WRITE | FI_REMOTE_READ);
    OFI_CHECK_RETURN_STR(ret, "target CNTR binding to EP failed");

#ifdef ENABLE_MR_RMA_EVENT
    if (shmem_transport_ofi_mr_rma_event) {
        ret = fi_mr_enable(shmem_transport_ofi_target_data_mrfd);
        OFI_CHECK_RETURN_STR(ret, "target data MR enable failed");

        ret = fi_mr_enable(shmem_transport_ofi_target_heap_mrfd);
        OFI_CHECK_RETURN_STR(ret, "target heap MR enable failed");
    }
#endif /* ENABLE_MR_RMA_EVENT */
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
            ret = fi_atomicvalid(shmem_transport_ctx_default.cntr_ep, DT[i],
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
            ret = fi_compare_atomicvalid(shmem_transport_ctx_default.cntr_ep, DT[i],
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
            ret = fi_fetch_atomicvalid(shmem_transport_ctx_default.cntr_ep, DT[i],
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
    OFI_CHECK_RETURN_STR(ret, "shmem_runtime_put fi_ephostname failed");
#endif

    ret = fi_getname((fid_t)shmem_transport_ofi_target_ep, epname, &epnamelen);
    if (ret != 0 || (epnamelen > sizeof(epname))) {
        RAISE_WARN_STR("fi_getname failed");
        return ret;
    }

    ret = shmem_runtime_put("fi_epname", epname, epnamelen);
    OFI_CHECK_RETURN_STR(ret, "shmem_runtime_put fi_epname failed");

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
    OFI_CHECK_RETURN_STR(ret, "fabric initialization failed");

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
    OFI_CHECK_RETURN_STR(ret, "domain initialization failed");

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
    OFI_CHECK_RETURN_STR(ret, "AV creation failed");

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
#  if !defined(ENABLE_HARD_POLLING) && defined(ENABLE_MR_RMA_EVENT)
    domain_attr.mr_mode      |= FI_MR_RMA_EVENT; /* can support RMA_EVENT on MR */
#  endif
#else
    domain_attr.mr_mode       = FI_MR_BASIC; /* VA space is pre-allocated */
#endif
#if !defined(ENABLE_MR_SCALABLE) || !defined(ENABLE_REMOTE_VIRTUAL_ADDRESSING)
    domain_attr.mr_key_size   = 1; /* Heap and data use different MR keys, need
                                      at least 1 byte */
#endif
#ifdef ENABLE_THREADS
    if (shmem_internal_thread_level == SHMEM_THREAD_MULTIPLE) {
#ifdef USE_THREAD_COMPLETION
        domain_attr.threading = FI_THREAD_COMPLETION;
#else
        domain_attr.threading = FI_THREAD_SAFE;
#endif /* USE_THREAD_COMPLETION */
    } else
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

    OFI_CHECK_RETURN_MSG(ret, "OFI transport did not find any valid fabric services "
                              "(provider=%s)\n",
                              info->prov_name != NULL ? info->prov_name : "<auto>");

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
#ifdef ENABLE_MR_RMA_EVENT
    shmem_transport_ofi_mr_rma_event = (info->p_info->domain_attr->mr_mode & FI_MR_RMA_EVENT) != 0;
#endif

    DEBUG_MSG("OFI provider: %s, fabric: %s, domain: %s\n",
              info->p_info->fabric_attr->prov_name,
              info->p_info->fabric_attr->name, info->p_info->domain_attr->name);

    return ret;
}

static int shmem_transport_ofi_target_ep_init(void)
{
    int ret = 0;

    struct fabric_info* info = &shmem_transport_ofi_info;
    info->p_info->ep_attr->tx_ctx_cnt = 0;
    info->p_info->caps = FI_RMA | FI_ATOMICS | FI_REMOTE_READ | FI_REMOTE_WRITE;
    info->p_info->tx_attr->op_flags = FI_DELIVERY_COMPLETE;
    info->p_info->mode = 0;
    info->p_info->tx_attr->mode = 0;
    info->p_info->rx_attr->mode = 0;

    ret = fi_endpoint(shmem_transport_ofi_domainfd,
                      info->p_info, &shmem_transport_ofi_target_ep, NULL);
    OFI_CHECK_RETURN_MSG(ret, "target endpoint creation failed (%s)\n", fi_strerror(errno));

    /* Attach the address vector */
    ret = fi_ep_bind(shmem_transport_ofi_target_ep, &shmem_transport_ofi_avfd->fid, 0);
    OFI_CHECK_RETURN_STR(ret, "fi_ep_bind AV to target endpoint failed");

    ret = allocate_recv_cntr_mr();
    if (ret != 0) return ret;

    ret = fi_enable(shmem_transport_ofi_target_ep);
    OFI_CHECK_RETURN_STR(ret, "fi_enable on target endpoint failed");

    return 0;
}

static int shmem_transport_ofi_ctx_init(shmem_transport_ctx_t *ctx, int id)
{
    int ret = 0;

    struct fi_cntr_attr cntr_put_attr = {0};
    struct fi_cntr_attr cntr_get_attr = {0};
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

    /* Allow provider to choose CQ size, since we are using FI_RM_ENABLED.
     * Context format is used to return bounce buffer pointers in the event
     * so that they can be inserted back into to the free list. */
    struct fi_cq_attr cq_attr = {0};
    cq_attr.format = FI_CQ_FORMAT_CONTEXT;

    struct fabric_info* info = &shmem_transport_ofi_info;
    info->p_info->ep_attr->tx_ctx_cnt = FI_SHARED_CONTEXT;
    info->p_info->caps = FI_RMA | FI_WRITE | FI_READ | FI_ATOMICS;
    info->p_info->tx_attr->op_flags = FI_DELIVERY_COMPLETE;
    info->p_info->mode = 0;
    info->p_info->tx_attr->mode = 0;
    info->p_info->rx_attr->mode = 0;

    ctx->id = id;
#ifdef USE_CTX_LOCK
    SHMEM_MUTEX_INIT(ctx->lock);
#endif
    shmem_internal_atomic_write(&ctx->pending_put_cntr, 0);
    shmem_internal_atomic_write(&ctx->pending_get_cntr, 0);

    ret = fi_cntr_open(shmem_transport_ofi_domainfd, &cntr_put_attr,
                       &ctx->put_cntr, NULL);
    OFI_CHECK_RETURN_MSG(ret, "put_cntr creation failed (%s)\n", fi_strerror(errno));

    ret = fi_cntr_open(shmem_transport_ofi_domainfd, &cntr_get_attr,
                       &ctx->get_cntr, NULL);
    OFI_CHECK_RETURN_MSG(ret, "get_cntr creation failed (%s)\n", fi_strerror(errno));

    ret = fi_cq_open(shmem_transport_ofi_domainfd, &cq_attr, &ctx->cq, NULL);
    OFI_CHECK_RETURN_MSG(ret, "cq_open failed (%s)\n", fi_strerror(errno));

    ret = fi_endpoint(shmem_transport_ofi_domainfd,
                      info->p_info, &ctx->cntr_ep, NULL);
    OFI_CHECK_RETURN_MSG(ret, "cntr_ep creation failed (%s)\n", fi_strerror(errno));

    /* TODO: STX contexts should be shared by SHMEM contexts that are private
     * to the same thread (i.e. have SHMEM_CTX_PRIVATE option set and same
     * thread ID/gettid()).  */
    /* TODO: Fill in TX attr */

    /* After reaching the STX limit, share STXs by selecting an array index
     * according to the "stx_share_algorithm". */
    uint32_t stx_idx = 0;
#ifdef ENABLE_THREADS
    if (shmem_internal_thread_level > SHMEM_THREAD_FUNNELED) {
        switch (shmem_transport_ofi_stx_share_alg) {
            case (ROUNDROBIN):
                stx_idx = (stx_idx + 1) % shmem_transport_ofi_stx_max;
                break;
            case (RANDOM):
                stx_idx = rand() % shmem_transport_ofi_stx_max;
                break;
        }
        #ifdef __APPLE__
        pthread_threadid_np(NULL, &shmem_transport_ofi_stx_pool[stx_idx].owner_tid);
        #else
        shmem_transport_ofi_stx_pool[stx_idx].owner_tid = gettid();
        #endif
    }
#else
    shmem_transport_ofi_stx_pool[stx_idx].owner_tid = 0;
#endif
    ctx->stx_idx = stx_idx;

    ret = bind_enable_cntr_ep_resources(ctx);
    OFI_CHECK_RETURN_MSG(ret, "context bind/enable CNTR endpoint failed (%s)\n", fi_strerror(errno));

    if (ctx->options & SHMEMX_CTX_BOUNCE_BUFFER &&
        shmem_transport_ofi_bounce_buffer_size > 0 &&
        shmem_transport_ofi_max_bounce_buffers > 0)
    {
        info->p_info->tx_attr->op_flags = FI_DELIVERY_COMPLETE;
        ret = fi_endpoint(shmem_transport_ofi_domainfd,
                          info->p_info, &ctx->cq_ep, NULL);
        OFI_CHECK_RETURN_MSG(ret, "cq_ep creation failed (%s)\n", fi_strerror(errno));

        ret = bind_enable_cq_ep_resources(ctx);
        OFI_CHECK_RETURN_MSG(ret, "context bind/enable CQ endpoint failed (%s)\n", fi_strerror(errno));

        ctx->bounce_buffers =
            shmem_free_list_init(sizeof(shmem_transport_ofi_bounce_buffer_t) +
                                 shmem_transport_ofi_bounce_buffer_size,
                                 init_bounce_buffer);
    }
    else {
        ctx->options &= ~SHMEMX_CTX_BOUNCE_BUFFER;
        ctx->cq_ep = NULL;
        ctx->bounce_buffers = NULL;
    }

    return 0;
}


int shmem_transport_init(void)
{
    int ret = 0;
    int i;

    SHMEM_MUTEX_INIT(shmem_transport_ofi_lock);

    shmem_transport_ofi_info.npes = shmem_runtime_get_size();

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
    if (ret != 0) return ret;

    ret = allocate_fabric_resources(&shmem_transport_ofi_info);
    if (ret != 0) return ret;

    /* STX max settings */
    shmem_transport_ofi_stx_max = shmem_internal_params.OFI_STX_MAX;

    /* STX sharing settings */
    char *type = shmem_internal_params.OFI_STX_SHARE_ALGORITHM;
    if (0 == strcmp(type, "round-robin")) {
        shmem_transport_ofi_stx_share_alg = ROUNDROBIN;
    } else if (0 == strcmp(type, "random")) {
        shmem_transport_ofi_stx_share_alg = RANDOM;
    } else {
        RAISE_WARN_MSG("Ignoring bad STX share algorithm '%s', using 'round-robin'\n", type);
        shmem_transport_ofi_stx_share_alg = ROUNDROBIN;
    }

    if ((shmem_internal_thread_level == SHMEM_THREAD_SINGLE ||
         shmem_internal_thread_level == SHMEM_THREAD_FUNNELED ) &&
         shmem_internal_params.OFI_STX_MAX_provided &&
         shmem_internal_params.OFI_STX_MAX > 1) {
        /* We need only 1 STX per PE with SHMEM_THREAD_SINGLE or SHMEM_THREAD_FUNNELED */
        RAISE_WARN_MSG("Ignoring invalid STX max setting '%ld' w/THREAD_SINGLE, using 1\n", 
                       shmem_internal_params.OFI_STX_MAX);
        shmem_transport_ofi_stx_max = 1;
    }

    /* Allocate STX array with max length */
    shmem_transport_ofi_stx_pool = malloc(shmem_transport_ofi_stx_max *
                                          sizeof(shmem_transport_ofi_stx_t));

    for (i = 0; i < shmem_transport_ofi_stx_max; i++) {
        ret = fi_stx_context(shmem_transport_ofi_domainfd, NULL,
                             &shmem_transport_ofi_stx_pool[i].stx, NULL);
        OFI_CHECK_RETURN_MSG(ret, "STX context creation failed (%s)\n", fi_strerror(ret));
    }

    /* The current bounce buffering implementation is only compatible with
     * providers that don't require FI_CONTEXT */
    if (shmem_transport_ofi_info.p_info->mode & FI_CONTEXT) {
        if (shmem_internal_my_pe == 0 && shmem_internal_params.BOUNCE_SIZE > 0) {
            DEBUG_STR("OFI provider requires FI_CONTEXT; disabling bounce buffering");
        }
        shmem_transport_ofi_bounce_buffer_size = 0;
        shmem_transport_ofi_max_bounce_buffers = 0;
    } else {
        shmem_transport_ofi_bounce_buffer_size = shmem_internal_params.BOUNCE_SIZE;
        shmem_transport_ofi_max_bounce_buffers = shmem_internal_params.MAX_BOUNCE_BUFFERS;
    }

    shmem_transport_ofi_put_poll_limit = shmem_internal_params.OFI_TX_POLL_LIMIT;
    shmem_transport_ofi_get_poll_limit = shmem_internal_params.OFI_RX_POLL_LIMIT;

    shmem_transport_ctx_default.options = SHMEMX_CTX_BOUNCE_BUFFER;

    ret = shmem_transport_ofi_ctx_init(&shmem_transport_ctx_default, SHMEM_CTX_DEFAULT_ID);
    if (ret != 0) return ret;

    ret = shmem_transport_ofi_target_ep_init();
    if (ret != 0) return ret;

    ret = publish_mr_info();
    if (ret != 0) return ret;

    ret = atomic_limitations_check();
    if (ret != 0) return ret;

    ret = publish_av_info(&shmem_transport_ofi_info);
    if (ret != 0) return ret;

    return 0;
}

int shmem_transport_startup(void)
{
    int ret;

    ret = populate_mr_tables();
    if (ret != 0) return ret;

    ret = populate_av();
    if (ret != 0) return ret;

    return 0;
}

int shmem_transport_ctx_create(long options, shmem_transport_ctx_t **ctx)
{
    SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);

    int ret;
    int id;

    /* FIXME: Context creation does not do resource cleanup on error, it just
     * returns. This is consistent with how initialization worked before, but
     * is worse since context creation is not necessarily do-or-die.
     */

    /* Look for an open slot in the contexts array */
    for (id = 0; id < shmem_transport_ofi_contexts_len; id++)
        if (shmem_transport_ofi_contexts[id] == NULL) break;

    /* If none found, grow the array */
    if (id >= shmem_transport_ofi_contexts_len) {
        id = shmem_transport_ofi_contexts_len;

        ssize_t i = shmem_transport_ofi_contexts_len;
        shmem_transport_ofi_contexts_len += shmem_transport_ofi_grow_size;
        shmem_transport_ofi_contexts = realloc(shmem_transport_ofi_contexts,
               shmem_transport_ofi_contexts_len * sizeof(shmem_transport_ctx_t*));

        for ( ; i < shmem_transport_ofi_contexts_len; i++)
            shmem_transport_ofi_contexts[i] = NULL;

        if (shmem_transport_ofi_contexts == NULL) {
            RAISE_ERROR_STR("Error: out of memory when allocating OFI ctx array");
        }
    }

    shmem_transport_ctx_t *ctxp = malloc(sizeof(shmem_transport_ctx_t));

    if (ctxp == NULL) {
        RAISE_ERROR_STR("Error: out of memory when allocating OFI ctx object");
    }

    ctxp->options = options;

    ret = shmem_transport_ofi_ctx_init(ctxp, id);

    if (ret) {
        shmem_transport_ofi_contexts[id] = NULL;
        free(ctxp);
    } else {
        shmem_transport_ofi_contexts[id] = ctxp;
        *ctx = ctxp;
    }

    SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
    return ret;

}

void shmem_transport_ctx_destroy(shmem_transport_ctx_t *ctx)
{
    int ret;

    shmem_transport_quiet(ctx);

    ret = fi_close(&ctx->cntr_ep->fid);
    OFI_CHECK_ERROR_MSG(ret, "Context CNTR endpoint close failed (%s)\n", fi_strerror(errno));

    if (ctx->cq_ep) {
        ret = fi_close(&ctx->cq_ep->fid);
        OFI_CHECK_ERROR_MSG(ret, "Context CQ EP close failed (%s)\n", fi_strerror(errno));

        shmem_free_list_destroy(ctx->bounce_buffers);
    }

    shmem_transport_ofi_stx_pool[ctx->stx_idx].ref_cnt--;

    ret = fi_close(&ctx->put_cntr->fid);
    OFI_CHECK_ERROR_MSG(ret, "Context put CNTR close failed (%s)\n", fi_strerror(errno));

    ret = fi_close(&ctx->get_cntr->fid);
    OFI_CHECK_ERROR_MSG(ret, "Context get CNTR close failed (%s)\n", fi_strerror(errno));

    ret = fi_close(&ctx->cq->fid);
    OFI_CHECK_ERROR_MSG(ret, "Context CQ close failed (%s)\n", fi_strerror(errno));

#ifdef USE_CTX_LOCK
    SHMEM_MUTEX_DESTROY(ctx->lock);
#endif

    if (ctx->id >= 0) {
        SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);
        shmem_transport_ofi_contexts[ctx->id] = NULL;
        SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
        free(ctx);
    }
    else if (ctx->id != SHMEM_CTX_DEFAULT_ID) {
        RAISE_ERROR_MSG("Attempted to destroy an invalid context (%s)\n", fi_strerror(errno));
    }
}

int shmem_transport_fini(void)
{
    int ret;
    size_t i;

    /* Free all shareable contexts.  This performs a quiet on each context,
     * ensuring all operations have completed before proceeding with shutdown. */

    for (i = 0; i < shmem_transport_ofi_contexts_len; ++i) {
        if (shmem_transport_ofi_contexts[i]) {
            shmem_transport_ctx_destroy(shmem_transport_ofi_contexts[i]);
        }
    }

    if (shmem_transport_ofi_contexts) free(shmem_transport_ofi_contexts);
    shmem_transport_ctx_destroy(&shmem_transport_ctx_default);

    for(i = 0; i < shmem_transport_ofi_stx_max; ++i) {
        ret = fi_close(&shmem_transport_ofi_stx_pool[i].stx->fid);
        OFI_CHECK_ERROR_MSG(ret, "STX context close failed (%s)\n", fi_strerror(errno));
    }
    free(shmem_transport_ofi_stx_pool);

    ret = fi_close(&shmem_transport_ofi_target_ep->fid);
    OFI_CHECK_ERROR_MSG(ret, "Target endpoint close failed (%s)\n", fi_strerror(errno));

#if defined(ENABLE_MR_SCALABLE) && defined(ENABLE_REMOTE_VIRTUAL_ADDRESSING)
    ret = fi_close(&shmem_transport_ofi_target_mrfd->fid);
    OFI_CHECK_ERROR_MSG(ret, "Target MR close failed (%s)\n", fi_strerror(errno));
#else
    ret = fi_close(&shmem_transport_ofi_target_heap_mrfd->fid);
    OFI_CHECK_ERROR_MSG(ret, "Target heap MR close failed (%s)\n", fi_strerror(errno));

    ret = fi_close(&shmem_transport_ofi_target_data_mrfd->fid);
    OFI_CHECK_ERROR_MSG(ret, "Target data MR close failed (%s)\n", fi_strerror(errno));
#endif

#ifndef ENABLE_HARD_POLLING
    ret = fi_close(&shmem_transport_ofi_target_cntrfd->fid);
    OFI_CHECK_ERROR_MSG(ret, "Target CT close failed (%s)\n", fi_strerror(errno));
#endif

    ret = fi_close(&shmem_transport_ofi_avfd->fid);
    OFI_CHECK_ERROR_MSG(ret, "AV close failed (%s)\n", fi_strerror(errno));

    ret = fi_close(&shmem_transport_ofi_domainfd->fid);
    OFI_CHECK_ERROR_MSG(ret, "Domain close failed (%s)\n", fi_strerror(errno));

    ret = fi_close(&shmem_transport_ofi_fabfd->fid);
    OFI_CHECK_ERROR_MSG(ret, "Fabric close failed (%s)\n", fi_strerror(errno));

#ifdef USE_AV_MAP
    free(addr_table);
#endif

    fi_freeinfo(shmem_transport_ofi_info.fabrics);

    SHMEM_MUTEX_DESTROY(shmem_transport_ofi_lock);

    return 0;
}
