/* -*- C -*-
 *
 * Copyright (c) 2020 NVidia Corporation.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include <string.h>
#include "transport_ucx.h"
#include "shmem.h"
#include "shmem_team.h"
#include <ucs/type/status.h>
#include <ucs/type/thread_mode.h>
#include <ucp/api/ucp_def.h>
#include <ucp/api/ucp.h>

shmem_transport_ctx_t shmem_transport_ctx_default;
shmem_ctx_t SHMEM_CTX_DEFAULT = (shmem_ctx_t) &shmem_transport_ctx_default;

ucp_context_h shmem_transport_ucp_ctx;
ucp_worker_h  shmem_transport_ucp_worker;
ucp_config_t *shmem_transport_ucp_config;
ucp_mem_h     shmem_transport_ucp_mem_data;
ucp_mem_h     shmem_transport_ucp_mem_heap;

shmem_transport_peer_t *shmem_transport_peers;

/* Tables to translate between SHM_INTERNAL and UCP ops */
ucp_atomic_post_op_t shmem_transport_ucx_post_op[] = {
    UCP_ATOMIC_POST_OP_AND,
    UCP_ATOMIC_POST_OP_OR,
    UCP_ATOMIC_POST_OP_XOR,
    UCP_ATOMIC_POST_OP_ADD
};

ucp_atomic_fetch_op_t shmem_transport_ucx_fetch_op[] = {
    UCP_ATOMIC_FETCH_OP_FAND,
    UCP_ATOMIC_FETCH_OP_FOR,
    UCP_ATOMIC_FETCH_OP_FXOR,
    UCP_ATOMIC_FETCH_OP_FADD
};

void shmem_transport_recv_cb_nop(void *request, ucs_status_t status) {
    return;
}

int shmem_transport_init(void)
{
    ucs_status_t status;
    ucp_params_t params;
    ucp_worker_params_t worker_params;

    params.field_mask = UCP_PARAM_FIELD_FEATURES;
    params.features   = UCP_FEATURE_RMA | UCP_FEATURE_AMO32 | UCP_FEATURE_AMO64;

    status = ucp_config_read(NULL, NULL, &shmem_transport_ucp_config);
    UCX_CHECK_STATUS(status);
    status = ucp_init(&params, shmem_transport_ucp_config, &shmem_transport_ucp_ctx);
    UCX_CHECK_STATUS(status);

    worker_params.field_mask  = UCP_WORKER_PARAM_FIELD_THREAD_MODE;

    switch (shmem_internal_thread_level) {
        case SHMEM_THREAD_SINGLE:
            worker_params.thread_mode = UCS_THREAD_MODE_SINGLE;
            break;
        case SHMEM_THREAD_FUNNELED:
            worker_params.thread_mode = UCS_THREAD_MODE_SERIALIZED;
            break;
        case SHMEM_THREAD_SERIALIZED:
            worker_params.thread_mode = UCS_THREAD_MODE_SERIALIZED;
            break;
        case SHMEM_THREAD_MULTIPLE:
            worker_params.thread_mode = UCS_THREAD_MODE_MULTI;
            break;
        default:
            RAISE_ERROR_MSG("Invalid thread level (%d)\n", shmem_internal_thread_level);
    }

    status = ucp_worker_create(shmem_transport_ucp_ctx, &worker_params,
                               &shmem_transport_ucp_worker);
    UCX_CHECK_STATUS(status);

    /* Publish addressing info to be exchanged by runtime layer */
    {
        ucp_address_t *addr;
        size_t len;
        int ret;

        status = ucp_worker_get_address(shmem_transport_ucp_worker, &addr, &len);
        UCX_CHECK_STATUS(status);

        ret = shmem_runtime_put("addr_len", &len, sizeof(size_t));
        if (ret) RAISE_ERROR_MSG("Runtime put of UCX address length failed (length %zu)\n", len);

        ret = shmem_runtime_put("addr", addr, len);
        if (ret) RAISE_ERROR_MSG("Runtime put of UCX address failed (length %zu)\n", len);

        ucp_worker_release_address(shmem_transport_ucp_worker, addr);
    }

    /* Register memory and publish rkeys */
    {
        void *rkey;
        size_t len;
        int ret;
        ucp_mem_map_params_t params;

        params.field_mask = UCP_MEM_MAP_PARAM_FIELD_ADDRESS |
                            UCP_MEM_MAP_PARAM_FIELD_LENGTH  |
                            UCP_MEM_MAP_PARAM_FIELD_FLAGS;
        params.flags      = 0;

        /* Data segment */
        params.address = shmem_internal_data_base;
        params.length  = shmem_internal_data_length;
        status = ucp_mem_map(shmem_transport_ucp_ctx, &params, &shmem_transport_ucp_mem_data);
        UCX_CHECK_STATUS(status);

        status = ucp_rkey_pack(shmem_transport_ucp_ctx, shmem_transport_ucp_mem_data,
                              &rkey, &len);
        UCX_CHECK_STATUS(status);
        ret = shmem_runtime_put("data_rkey_len", &len, sizeof(size_t));
        if (ret) RAISE_ERROR_STR("Runtime put of UCX data segment rkey length");
        ret = shmem_runtime_put("data_rkey", rkey, len);
        if (ret) RAISE_ERROR_STR("Runtime put of UCX data segment rkey");
        ucp_rkey_buffer_release(rkey);

        ret = shmem_runtime_put("data_base", &shmem_internal_data_base, sizeof(uint8_t*));
        if (ret) {
            RAISE_WARN_STR("Put of data segment address to runtime KVS failed");
            return 1;
        }

        /* Heap segment */
        params.address = shmem_internal_heap_base;
        params.length  = shmem_internal_heap_length;
        status = ucp_mem_map(shmem_transport_ucp_ctx, &params, &shmem_transport_ucp_mem_heap);
        UCX_CHECK_STATUS(status);

        status = ucp_rkey_pack(shmem_transport_ucp_ctx, shmem_transport_ucp_mem_heap,
                            &rkey, &len);
        ret = shmem_runtime_put("heap_rkey_len", &len, sizeof(size_t));
        if (ret) RAISE_ERROR_STR("Runtime put of UCX heap segment rkey length");
        ret = shmem_runtime_put("heap_rkey", rkey, len);
        if (ret) RAISE_ERROR_STR("Runtime put of UCX heap segment rkey");
        ucp_rkey_buffer_release(rkey);

        ret = shmem_runtime_put("heap_base", &shmem_internal_heap_base, sizeof(uint8_t*));
        if (ret) {
            RAISE_WARN_STR("Put of heap address to runtime KVS failed");
            return 1;
        }
    }

    /* Configure the default context */
    shmem_transport_ctx_default.options = 0;
    shmem_transport_ctx_default.team    = &shmem_internal_team_world;

    return 0;
}

int shmem_transport_startup(void)
{
    int i, ret;

    shmem_transport_peers = malloc(shmem_internal_num_pes *
                                   sizeof(shmem_transport_peer_t));

    /* Build connection table to each peer */
    /* FIXME: Currently, we create connections to each peer during startup.
     * Would it be better to create endpoints on-demand? And is there a limit
     * to how many we can have? */
    for (i = 0; i < shmem_internal_num_pes; i++) {
        ucs_status_t status;
        ucp_ep_params_t params;
        size_t rkey_len;
        void *rkey;

        ret = shmem_runtime_get(i, "addr_len", &shmem_transport_peers[i].addr_len, sizeof(size_t));
        if (ret) RAISE_ERROR_MSG("Runtime get of UCX address length failed (PE %d, ret %d)\n", i, ret);

        shmem_transport_peers[i].addr = malloc(shmem_transport_peers[i].addr_len);
        ret = shmem_runtime_get(i, "addr", shmem_transport_peers[i].addr,
                                shmem_transport_peers[i].addr_len);
        if (ret) RAISE_ERROR_MSG("Runtime get of UCX address failed (PE %d, ret %d)\n", i, ret);

        params.field_mask = UCP_EP_PARAM_FIELD_REMOTE_ADDRESS;
        params.address    = shmem_transport_peers[i].addr;

        status = ucp_ep_create(shmem_transport_ucp_worker, &params, &shmem_transport_peers[i].ep);
        UCX_CHECK_STATUS(status);

        ret = shmem_runtime_get(i, "data_rkey_len", &rkey_len, sizeof(size_t));
        if (ret) RAISE_ERROR_MSG("Runtime get of UCX data rkey length failed (PE %d, ret %d)\n", i, ret);
        rkey = malloc(rkey_len);
        if (rkey == NULL) RAISE_ERROR_MSG("Out of memory, allocating rkey buffer (len = %zu)\n", rkey_len);
        ret = shmem_runtime_get(i, "data_rkey", rkey, rkey_len);
        if (ret) RAISE_ERROR_MSG("Runtime get of UCX data rkey failed (PE %d, ret %d)\n", i, ret);
        /* XXX: Why is an EP needed to unpack the rkey, but not to pack it or register memory? */
        status = ucp_ep_rkey_unpack(shmem_transport_peers[i].ep, rkey, &shmem_transport_peers[i].data_rkey);
        UCX_CHECK_STATUS(status);
        free(rkey);

        ret = shmem_runtime_get(i, "heap_rkey_len", &rkey_len, sizeof(size_t));
        if (ret) RAISE_ERROR_MSG("Runtime get of UCX heap rkey length failed (PE %d, ret %d)\n", i, ret);
        rkey = malloc(rkey_len);
        if (rkey == NULL) RAISE_ERROR_MSG("Out of memory, allocating rkey buffer (len = %zu)\n", rkey_len);
        ret = shmem_runtime_get(i, "heap_rkey", rkey, rkey_len);
        if (ret) RAISE_ERROR_MSG("Runtime get of UCX heap rkey failed (PE %d, ret %d)\n", i, ret);
        status = ucp_ep_rkey_unpack(shmem_transport_peers[i].ep, rkey, &shmem_transport_peers[i].heap_rkey);
        UCX_CHECK_STATUS(status);
        free(rkey);

        ret = shmem_runtime_get(i, "data_base", &shmem_transport_peers[i].data_base, sizeof(void*));
        if (ret) RAISE_ERROR_MSG("Runtime get of UCX data base address failed (PE %d, ret %d)\n", i, ret);

        ret = shmem_runtime_get(i, "heap_base", &shmem_transport_peers[i].heap_base, sizeof(void*));
        if (ret) RAISE_ERROR_MSG("Runtime get of UCX heap base address failed (PE %d, ret %d)\n", i, ret);
    }

    return 0;
}

int shmem_transport_fini(void)
{
    ucs_status_t status;
    int i;

    /* Clean up contexts */
    shmem_transport_quiet(&shmem_transport_ctx_default);

    /* Clean up peers table */
    for (i = 0; i < shmem_internal_num_pes; i++) {
        ucp_rkey_destroy(shmem_transport_peers[i].data_rkey);
        ucp_rkey_destroy(shmem_transport_peers[i].heap_rkey);
        ucs_status_ptr_t pstatus = ucp_ep_close_nb(shmem_transport_peers[i].ep,
                                                   UCP_EP_CLOSE_MODE_FLUSH);
        shmem_transport_ucx_complete_op(pstatus);
        free(shmem_transport_peers[i].addr);
    }

    /* Unmap memory and shut down UCX */
    status = ucp_mem_unmap(shmem_transport_ucp_ctx, shmem_transport_ucp_mem_data);
    UCX_CHECK_STATUS(status);
    status = ucp_mem_unmap(shmem_transport_ucp_ctx, shmem_transport_ucp_mem_heap);
    UCX_CHECK_STATUS(status);

    ucp_worker_destroy(shmem_transport_ucp_worker);
    ucp_cleanup(shmem_transport_ucp_ctx);
    ucp_config_release(shmem_transport_ucp_config);

    return 0;
}
