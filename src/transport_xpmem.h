/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 * 
 * This file is part of the Portals SHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef TRANSPORT_XPMEM_H
#define TRANSPORT_XPMEM_H

#include <string.h>
#include <xpmem.h>

struct shmem_transport_xpmem_peer_info_t {
    xpmem_apid_t data_apid;
    xpmem_apid_t heap_apid;
    void *data_attach_ptr;
    void *heap_attach_ptr;
    void *data_ptr;
    void *heap_ptr;
};

extern struct shmem_transport_xpmem_peer_info_t *shmem_transport_xpmem_peers;


#define XPMEM_GET_REMOTE_ACCESS(target, rank, ptr)                      \
    do {                                                                \
        if (((void*) target > shmem_internal_data_base) &&              \
            ((char*) target < (char*) shmem_internal_data_base + shmem_internal_data_length)) { \
            ptr = (char*) target - (char*) shmem_internal_data_base +   \
                (char*) shmem_transport_xpmem_peers[rank].data_ptr;     \
        } else if (((void*) target > shmem_internal_heap_base) &&       \
                   ((char*) target < (char*) shmem_internal_heap_base + shmem_internal_heap_length)) { \
            ptr = (char*) target - (char*) shmem_internal_heap_base +   \
                (char*) shmem_transport_xpmem_peers[rank].heap_ptr;     \
        } else {                                                        \
            ptr = NULL;                                                 \
        }                                                               \
    } while (0)

int shmem_transport_xpmem_init(long eager_size);

int shmem_transport_xpmem_startup(void);

int shmem_transport_xpmem_fini(void);

static inline
void *
shmem_transport_xpmem_ptr(void *target, int pe, int noderank)
{
    char *remote_ptr;

    XPMEM_GET_REMOTE_ACCESS(target, noderank, remote_ptr);
    return remote_ptr;
}

static inline
int
shmem_transport_xpmem_quiet(void)
{
    return 0;
}

static inline
int
shmem_transport_xpmem_fence(void)
{
    return 0;
}

static inline
void
shmem_transport_xpmem_put(void *target, const void *source, size_t len, int pe, int noderank)
{
    char *remote_ptr;

    XPMEM_GET_REMOTE_ACCESS(target, noderank, remote_ptr);
    if (NULL == remote_ptr) {
        fprintf(stderr, "[%03d] ERROR: target (0x%lx) outside of symmetric areas\n",
                shmem_internal_my_pe, (unsigned long) target);      
        abort();
    }

    memcpy(remote_ptr, source, len);
    __sync_synchronize();
}


static inline
void
shmem_transport_xpmem_get(void *target, const void *source, size_t len, int pe, int noderank)
{
    char *remote_ptr;

    XPMEM_GET_REMOTE_ACCESS(source, noderank, remote_ptr);
    if (NULL == remote_ptr) {
        fprintf(stderr, "[%03d] ERROR: target (0x%lx) outside of symmetric areas\n",
                shmem_internal_my_pe, (unsigned long) target);      
        abort();
    }

    __sync_synchronize();
    memcpy(target, remote_ptr, len);
}

#endif
