#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include "shmem_internal.h"
#include "shmem_node_util.h"

#define SHMEM_INTERNAL_MAX_NPES_PER_NODE 255

int *shmem_internal_location_array = NULL;

static int node_util_is_initialized = 0;
static int node_util_is_started = 0;
static int n_local_pes = 0;
static shmem_transport_addr_t addr;

int shmem_node_util_init(void)
{
    int ret;

    if (!node_util_is_initialized) {
        addr = shmem_transport_get_local_addr();

        shmem_internal_location_array = malloc(sizeof(int) * shmem_internal_num_pes);
        if (NULL == shmem_internal_location_array) {
            RETURN_ERROR_STR("Out of memory when allocating node_util location array");
            return 1;
        }

        memset(shmem_internal_location_array, -1, shmem_internal_num_pes);

        ret = shmem_runtime_put("nodename", &addr, sizeof(addr));
        if (ret !=0) {
            RETURN_ERROR_MSG("shmem_node_util_init failed during nodename store to KVS: (%d)", ret);
            return ret;
        }

        node_util_is_initialized = 1;
    } else {
        if (shmem_internal_params.DEBUG) {
            DEBUG_STR("Initialized shmem_node_util more than once");
        }
    }

    return 0;
}


void shmem_node_util_fini(void)
{
    free(shmem_internal_location_array);
    return;
}

/* This function should only be called after the shmem_runtime KVS has synchronized */
int shmem_node_util_startup(void)
{
    int ret, i;
    shmem_transport_addr_t addr_cmp;

    if (!node_util_is_started) {
        for (i = 0; i < shmem_internal_num_pes; i++) {
            ret = shmem_runtime_get(i, "nodename", &addr_cmp, sizeof(shmem_transport_addr_t));
            if (ret != 0) {
                RETURN_ERROR_MSG("shmem_node_util_startup failed during nodename read from KVS: (%d)", ret);
                return ret;
            }
            if (memcmp(&addr, &addr_cmp, sizeof(shmem_transport_addr_t)) == 0) {
                shmem_node_util_set_local_rank(i, n_local_pes++);
            }
        }
#ifdef USE_ON_NODE_COMMS
        if (n_local_pes > SHMEM_INTERNAL_MAX_NPES_PER_NODE) {
            RETURN_ERROR_MSG("Number of local ranks exceeds limit of %d", SHMEM_INTERNAL_MAX_NPES_PER_NODE);
            return 1;
        } else if (n_local_pes <= 0) {
            RETURN_ERROR_STR("Failed to find any node-local PEs");
            return 1;
        }
#endif
        node_util_is_started = 1;
    } else {
        if (shmem_internal_params.DEBUG) {
            DEBUG_STR("Called shmem_node_util_startup more than once");
        }
    }

    return 0;
}


int shmem_node_util_n_local_pes()
{
    return n_local_pes;
}

shmem_transport_addr_t shmem_node_util_get_addr(int pe)
{
    int ret;
    shmem_transport_addr_t addr;

    if (pe == shmem_internal_my_pe) {
        return shmem_transport_get_local_addr();
    } else {
        if (node_util_is_started) {
            ret = shmem_runtime_get(pe, "nodename", &addr, sizeof(shmem_transport_addr_t));
            if (ret != 0) {
                RAISE_ERROR_MSG("shmem_node_util_get_addr failed: (%d)", ret);
            }
        } else {
            RAISE_ERROR_STR("shmem_node_util_get_addr called before shmem_node_util_startup");
        }
        return addr;
    }
}
