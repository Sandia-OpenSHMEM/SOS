#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "shmem_internal.h"
#include "shmem_node_util.h"


char *shmem_internal_location_array = NULL;

static int node_util_is_initialized = 0;


int shmem_node_util_init(void) {
    int ret;

    if (!node_util_is_initialized) {
        shmem_internal_location_array = malloc(sizeof(char) * shmem_internal_num_pes);
        if (NULL == shmem_internal_location_array) {
            RAISE_ERROR_STR("Error: out of memory when allocating node_util location array");
            return 1;
        }

        memset(shmem_internal_location_array, -1, shmem_internal_num_pes);

        ret = shmem_runtime_put("nodename", shmem_internal_nodename(), strlen(shmem_internal_nodename())+1);
        if (ret !=0) {
            RAISE_ERROR_MSG("shmem_node_util_init failed during nodename store to KVS: (%d)", ret);
            return ret;
        }

        node_util_is_initialized = 1;
    } else {
        RAISE_WARN_STR("Attemping to initialize shmem_node_util more than once");
    }

    return 0;
}


inline
void shmem_node_util_set_node_rank(int pe, int node_rank) {

    shmem_internal_location_array[pe] = node_rank;
    return;
}

inline
int shmem_node_util_get_rank_same_node(int pe) {

#ifdef USE_ON_NODE_COMMS
    return shmem_internal_location_array[pe];
#elif defined(USE_MEMCPY)
    return pe == shmem_internal_my_pe ? 0 : -1;
#else
    return -1;
#endif

}

int shmem_node_util_pe_on_same_node(int pe) {
    char nodename[SHMEM_INTERNAL_MAX_HOSTNAME_LEN];

    shmem_runtime_get(pe, "nodename", nodename, SHMEM_INTERNAL_MAX_HOSTNAME_LEN);
    if (strncmp(shmem_internal_nodename(), nodename, strlen(shmem_internal_nodename())) == 0) {
        return 1;
    } else {
        return 0;
    }
}

int shmem_node_util_count_local_pes(void) {
    int i;
    char nodename[SHMEM_INTERNAL_MAX_HOSTNAME_LEN];
    int num_on_node = 0;

    for (i = 0; i < shmem_internal_num_pes; i++) {
        shmem_runtime_get(i, "nodename", nodename, SHMEM_INTERNAL_MAX_HOSTNAME_LEN);
        if (strncmp(shmem_internal_nodename(), nodename, strlen(shmem_internal_nodename())) == 0) {
            shmem_node_util_set_node_rank(i, num_on_node++);
#ifdef USE_ON_NODE_COMMS
            if (num_on_node > 255) {
                RAISE_WARN_STR("Number of local ranks exceeds limit of 255");
                return -1;
            }
#endif
        }
    }

    return num_on_node;

}
