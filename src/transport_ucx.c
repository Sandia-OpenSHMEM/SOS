/* -*- C -*-
 *
 * Copyright (c) 2020 NVidia Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include "transport_ucx.h"
#include "shmem.h"
#include "shmem_team.h"
#include <ucs/type/status.h>
#include <ucs/type/thread_mode.h>
#include <ucp/api/ucp_def.h>
#include <ucp/api/ucp.h>

/* On multi-rail systems, the address size can exceed what PMI is able to
 * handle in a single KVS entry. Large addresses are spread out across multiple
 * KVS entries according to the RUNTIME_ADDR_CHUNK size. The size below was
 * chosen for compatibility with the Hydra process manager. */
#define RUNTIME_ADDR_CHUNK 400

#define MIN(a,b) (((a)<(b))?(a):(b))

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

void shmem_transport_ucx_cb_nop(void *request, ucs_status_t status) {
    return;
}

void shmem_transport_ucx_cb_complete(void *request, ucs_status_t status, void *user_data) {
    if (status != UCS_OK)
        RAISE_ERROR_STR("Error while completing operation");

    __atomic_store_n((long*)user_data, 1, __ATOMIC_RELEASE);
    return;
}

static pthread_t shmem_transport_ucx_progress_thread;
static int shmem_transport_ucx_progress_thread_enabled = 1;

static void * shmem_transport_ucx_progress_thread_func(void *arg)
{
    while (__atomic_load_n(&shmem_transport_ucx_progress_thread_enabled, __ATOMIC_ACQUIRE)) {
        shmem_transport_probe();
        usleep(shmem_internal_params.PROGRESS_INTERVAL);
    }

    return NULL;
}

int shmem_transport_init(void)
{
    ucs_status_t status;
    ucp_params_t params;
    ucp_worker_params_t worker_params;
    ucp_worker_attr_t worker_attr;
    ucs_thread_mode_t requested;

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

    requested = worker_params.thread_mode;

    if (shmem_internal_params.PROGRESS_INTERVAL > 0)
        worker_params.thread_mode = UCS_THREAD_MODE_MULTI;

    status = ucp_worker_create(shmem_transport_ucp_ctx, &worker_params,
                               &shmem_transport_ucp_worker);
    UCX_CHECK_STATUS(status);

    worker_attr.field_mask = UCP_WORKER_ATTR_FIELD_THREAD_MODE;
    status = ucp_worker_query(shmem_transport_ucp_worker, &worker_attr);
    UCX_CHECK_STATUS(status);

    DEBUG_MSG("UCX thread mode %d, requested %d\n",
              worker_attr.thread_mode, worker_params.thread_mode);

    /* Note: UCX must be configured with --enable-mt for multithreaded usage */
    if (worker_attr.thread_mode < worker_params.thread_mode) {
        if (worker_attr.thread_mode < requested) {
            RAISE_ERROR_MSG("SHMEM threading requested (%d), "
                            "but UCX threading support not available (%d)\n",
                            requested, worker_attr.thread_mode);
        } else if (shmem_internal_params.PROGRESS_INTERVAL_provided) {
            RAISE_ERROR_MSG("Requested progress thread, "
                            "but UCX threading support not available (%d)\n",
                            worker_attr.thread_mode);
        } else {
            /* Thread level meets the application's needs and they did not
             * request the progress thread */
            DEBUG_MSG("UCX threading support not available (%d), "
                      "auto-disabling progress thread\n",
                      worker_attr.thread_mode);
            shmem_internal_params.PROGRESS_INTERVAL = 0;
        }
    }

    /* Publish addressing info to be exchanged by runtime layer */
    {
        ucp_address_t *addr;
        uint8_t *addr_bytes;
        size_t len;
        int ret;

        status = ucp_worker_get_address(shmem_transport_ucp_worker, &addr, &len);
        UCX_CHECK_STATUS(status);

        /* Limit the X in "addrX" to a range of printable ASCII characters */
        if (len > 'z' - '0' * RUNTIME_ADDR_CHUNK)
            RAISE_ERROR_MSG("UCX address too large (length %zu, chunk %d)\n", len, RUNTIME_ADDR_CHUNK);

        ret = shmem_runtime_put("addr_len", &len, sizeof(size_t));
        if (ret) RAISE_ERROR_MSG("Runtime put of UCX address length failed (length %zu)\n", len);

        addr_bytes = (uint8_t*) addr;

        for (size_t chunk = 0; chunk < len; chunk += RUNTIME_ADDR_CHUNK) {
            char key[6] = "addrX";
            size_t chunk_idx = 4;

            key[chunk_idx] = '0' + chunk/RUNTIME_ADDR_CHUNK;

            ret = shmem_runtime_put(key, addr_bytes+chunk, MIN(len-chunk, RUNTIME_ADDR_CHUNK));

            if (ret) {
                RAISE_ERROR_MSG("Runtime put of UCX address chunk %zu failed (chunk %d)\n",
                                chunk/RUNTIME_ADDR_CHUNK, RUNTIME_ADDR_CHUNK);
            }
        }

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

#ifndef ENABLE_REMOTE_VIRTUAL_ADDRESSING
        ret = shmem_runtime_put("data_base", &shmem_internal_data_base, sizeof(uint8_t*));
        if (ret) {
            RAISE_WARN_STR("Put of data segment address to runtime KVS failed");
            return 1;
        }
#endif

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

#ifndef ENABLE_REMOTE_VIRTUAL_ADDRESSING
        ret = shmem_runtime_put("heap_base", &shmem_internal_heap_base, sizeof(uint8_t*));
        if (ret) {
            RAISE_WARN_STR("Put of heap address to runtime KVS failed");
            return 1;
        }
#endif
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
    for (i = 0; i < shmem_internal_num_pes; i++) {
        ucs_status_t status;
        ucp_ep_params_t params;
        size_t rkey_len;
        void *rkey;
        uint8_t *addr_bytes;
        size_t len;

        ret = shmem_runtime_get(i, "addr_len", &shmem_transport_peers[i].addr_len, sizeof(size_t));
        if (ret) RAISE_ERROR_MSG("Runtime get of UCX address length failed (PE %d, ret %d)\n", i, ret);

        len = shmem_transport_peers[i].addr_len;
        shmem_transport_peers[i].addr = malloc(len);
        addr_bytes = (uint8_t*) shmem_transport_peers[i].addr;

        for (size_t chunk = 0; chunk < len; chunk += RUNTIME_ADDR_CHUNK) {
            char key[6] = "addrX";
            size_t chunk_idx = 4;

            key[chunk_idx] = '0' + chunk/RUNTIME_ADDR_CHUNK;

            ret = shmem_runtime_get(i, key, addr_bytes+chunk, MIN(len-chunk, RUNTIME_ADDR_CHUNK));

            if (ret) {
                RAISE_ERROR_MSG("Runtime get of UCX address chunk %zu failed (chunk %d)\n",
                                chunk/RUNTIME_ADDR_CHUNK, RUNTIME_ADDR_CHUNK);
            }
        }

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

#ifndef ENABLE_REMOTE_VIRTUAL_ADDRESSING
        ret = shmem_runtime_get(i, "data_base", &shmem_transport_peers[i].data_base, sizeof(void*));
        if (ret) RAISE_ERROR_MSG("Runtime get of UCX data base address failed (PE %d, ret %d)\n", i, ret);

        ret = shmem_runtime_get(i, "heap_base", &shmem_transport_peers[i].heap_base, sizeof(void*));
        if (ret) RAISE_ERROR_MSG("Runtime get of UCX heap base address failed (PE %d, ret %d)\n", i, ret);
#endif
    }

    if (shmem_internal_params.PROGRESS_INTERVAL > 0)
        pthread_create(&shmem_transport_ucx_progress_thread, NULL,
                       &shmem_transport_ucx_progress_thread_func, NULL);

    return 0;
}

int shmem_transport_fini(void)
{
    ucs_status_t status;
    int i;
    void *progress_out;

    if (shmem_internal_params.PROGRESS_INTERVAL > 0) {
        __atomic_store_n(&shmem_transport_ucx_progress_thread_enabled, 0, __ATOMIC_RELEASE);
        pthread_join(shmem_transport_ucx_progress_thread, &progress_out);
    }

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

    free(shmem_transport_peers);

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
