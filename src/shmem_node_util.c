#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include "shmem_internal.h"
#include "shmem_node_util.h"


int *shmem_internal_location_array = NULL;

static int node_util_is_initialized = 0;
static char my_hostname[SHMEM_INTERNAL_MAX_HOSTNAME_LEN];
static int n_local_pes = 0;

int shmem_node_util_init(void)
{
    int ret;
    char errmsg[256];

    if (!node_util_is_initialized) {
        if (gethostname(my_hostname, SHMEM_INTERNAL_MAX_HOSTNAME_LEN)) {
            RETURN_ERROR_MSG("gethostname failed '%s'", shmem_util_strerror(errno, errmsg, 256));
            return errno;
        }

        shmem_internal_location_array = malloc(sizeof(int) * shmem_internal_num_pes);
        if (NULL == shmem_internal_location_array) {
            RETURN_ERROR_STR("Error: out of memory when allocating node_util location array");
            return 1;
        }

        memset(shmem_internal_location_array, -1, shmem_internal_num_pes);

        ret = shmem_runtime_put("nodename", my_hostname, strlen(my_hostname)+1);
        if (ret !=0) {
            RETURN_ERROR_MSG("shmem_node_util_init failed during nodename store to KVS: (%d)", ret);
            return ret;
        }

        node_util_is_initialized = 1;
    } else {
        RAISE_WARN_STR("Attemping to initialize shmem_node_util more than once");
    }

    return 0;
}


void shmem_node_util_fini(void)
{
    free(shmem_internal_location_array);
    return;
}


char* shmem_node_util_nodename(void)
{
    return my_hostname;
}


/* This function should only be called after the shmem_runtime KVS has synchronized */
int shmem_node_util_startup(void)
{
    int ret, i;
    char nodename[SHMEM_INTERNAL_MAX_HOSTNAME_LEN];

    for (i = 0; i < shmem_internal_num_pes; i++) {
        ret = shmem_runtime_get(i, "nodename", nodename, SHMEM_INTERNAL_MAX_HOSTNAME_LEN);
        if (ret != 0) {
            RETURN_ERROR_MSG("shmem_node_util_startup failed during nodename read from KVS: (%d)", ret);
            return ret;
        }
        if (strncmp(shmem_node_util_nodename(), nodename, strlen(shmem_node_util_nodename())) == 0) {
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

    return 0;
}

int shmem_node_util_n_local_pes() {
    return n_local_pes;
}
