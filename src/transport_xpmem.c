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

#include "config.h"

#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <xpmem.h>

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"
#include "runtime.h"

struct share_info_t {
    xpmem_segid_t data_seg;
    size_t data_len;
    size_t data_off;
    xpmem_segid_t heap_seg;
    size_t heap_len;
    size_t heap_off;
};

struct shmem_transport_xpmem_peer_info_t *shmem_transport_xpmem_peers = NULL;
static struct share_info_t my_info;

#define FIND_BASE(ptr, page_size) ((char*) (((uintptr_t) ptr / page_size) * page_size))
#define FIND_LEN(ptr, len, page_size) ((((char*) ptr - FIND_BASE(ptr, page_size) + len - 1) / \
                                        page_size + 1) * page_size)

int
shmem_transport_xpmem_init(long eager_size)
{
    long page_size = sysconf(_SC_PAGESIZE);
    char *base;
    size_t len;
    int ret;

    /* setup data region */
    base = FIND_BASE(shmem_internal_data_base, page_size);
    len = FIND_LEN(shmem_internal_data_base, shmem_internal_data_length, page_size);
    my_info.data_seg = xpmem_make(base, len, XPMEM_PERMIT_MODE, (void*)0666);
    if (-1 == my_info.data_seg) {
        fprintf(stderr, "[%03d] ERROR: xpmem_make failed: %s\n",
                shmem_internal_my_pe, strerror(errno));
        return 1;
    }
    my_info.data_off = (char*) shmem_internal_data_base - (char*) base;
    my_info.data_len = len;

    /* setup heap region */
    base = FIND_BASE(shmem_internal_heap_base, page_size);
    len = FIND_LEN(shmem_internal_heap_base, shmem_internal_heap_length, page_size);
    my_info.heap_seg = xpmem_make(base, len, XPMEM_PERMIT_MODE, (void*)0666);
    if (-1 == my_info.heap_seg) {
        fprintf(stderr, "[%03d] ERROR: xpmem_make failed: %s\n",
                shmem_internal_my_pe, strerror(errno));
        return 1;
    }
    my_info.heap_off = (char*) shmem_internal_heap_base - (char*) base;
    my_info.heap_len = len;

    ret = shmem_runtime_put("xpmem-segids", &my_info, sizeof(struct share_info_t));
    if (0 != ret) {
        fprintf(stderr, "[%03d] ERROR: runtime_put failed: %d\n",
                shmem_internal_my_pe, ret);
        return 1;
    }

    return 0;
}


int
shmem_transport_xpmem_startup(void)
{
    int ret, i, peer_num, num_on_node = 0;
    struct share_info_t info;
    struct xpmem_addr addr;

    for (i = 0 ; i < shmem_internal_num_pes; ++i) {
        if (-1 != SHMEM_GET_RANK_SAME_NODE(i)) {
            num_on_node++;
        }
    }

    /* allocate space for local peers */
    shmem_transport_xpmem_peers = calloc(num_on_node, 
                                         sizeof(struct shmem_transport_xpmem_peer_info_t));
    if (NULL == shmem_transport_xpmem_peers) return 1;

    /* get local peer info and map into our address space ... */
    for (i = 0 ; i < shmem_internal_num_pes; ++i) {
        peer_num = SHMEM_GET_RANK_SAME_NODE(i);
        if (-1 == peer_num) continue;

        if (shmem_internal_my_pe == i) {
            shmem_transport_xpmem_peers[peer_num].data_ptr = 
                shmem_internal_data_base;
            shmem_transport_xpmem_peers[peer_num].heap_ptr = 
                shmem_internal_heap_base;
        } else {
            ret = shmem_runtime_get(i, "xpmem-segids", &info, sizeof(struct share_info_t));
            if (0 != ret) {
                fprintf(stderr, "[%03d] ERROR: runtime_get failed: %d\n",
                        shmem_internal_my_pe, ret);
                return 1;
            }

            shmem_transport_xpmem_peers[peer_num].data_apid =
                xpmem_get(info.data_seg, XPMEM_RDWR, XPMEM_PERMIT_MODE, (void*)0666);
            if (shmem_transport_xpmem_peers[peer_num].data_apid < 0) {
                fprintf(stderr, "[%03d] ERROR: could not get data apid: %s\n",
                        shmem_internal_my_pe, strerror(errno));
                return 1;
            }

            addr.apid = shmem_transport_xpmem_peers[peer_num].data_apid;
            addr.offset = 0;

            shmem_transport_xpmem_peers[peer_num].data_attach_ptr = 
                xpmem_attach(addr, info.data_len, NULL);
            if ((size_t) shmem_transport_xpmem_peers[peer_num].data_ptr == XPMEM_MAXADDR_SIZE) {
                fprintf(stderr, "[%03d] ERROR: could not get data segment: %s\n",
                        shmem_internal_my_pe, strerror(errno));
                return 1;
            }
            shmem_transport_xpmem_peers[peer_num].data_ptr = 
                (char*) shmem_transport_xpmem_peers[peer_num].data_attach_ptr + info.data_off;

            shmem_transport_xpmem_peers[peer_num].heap_apid =
                xpmem_get(info.heap_seg, XPMEM_RDWR, XPMEM_PERMIT_MODE, (void*)0666);
            if (shmem_transport_xpmem_peers[peer_num].heap_apid < 0) {
                fprintf(stderr, "[%03d] ERROR: could not get heap apid: %s\n",
                        shmem_internal_my_pe, strerror(errno));
                return 1;
            }

            addr.apid = shmem_transport_xpmem_peers[peer_num].heap_apid;
            addr.offset = 0;

            shmem_transport_xpmem_peers[peer_num].heap_attach_ptr = 
                xpmem_attach(addr, info.heap_len, NULL);
            if ((size_t) shmem_transport_xpmem_peers[peer_num].heap_ptr == XPMEM_MAXADDR_SIZE) {
                fprintf(stderr, "[%03d] ERROR: could not get data segment: %s\n",
                        shmem_internal_my_pe, strerror(errno));
                return 1;
            }
            shmem_transport_xpmem_peers[peer_num].heap_ptr = 
                (char*) shmem_transport_xpmem_peers[peer_num].heap_attach_ptr + info.heap_off;
        }
    }

    return 0;
}


int
shmem_transport_xpmem_fini(void)
{
    int i, peer_num;

    if (NULL != shmem_transport_xpmem_peers) {
        for (i = 0 ; i < shmem_internal_num_pes; ++i) {
            peer_num = SHMEM_GET_RANK_SAME_NODE(i);
            if (-1 == peer_num) continue;
            if (shmem_internal_my_pe == i) continue;

            if (NULL != shmem_transport_xpmem_peers[peer_num].data_ptr) {
                xpmem_detach(shmem_transport_xpmem_peers[peer_num].data_attach_ptr);
            }
                
            if (0 != shmem_transport_xpmem_peers[peer_num].data_apid) {
                xpmem_release(shmem_transport_xpmem_peers[peer_num].data_apid);
            }
                
            if (NULL != shmem_transport_xpmem_peers[peer_num].heap_ptr) {
                xpmem_detach(shmem_transport_xpmem_peers[peer_num].heap_attach_ptr);
            }
                
            if (0 != shmem_transport_xpmem_peers[peer_num].heap_apid) {
                xpmem_release(shmem_transport_xpmem_peers[peer_num].heap_apid);
            }
        }
        free(shmem_transport_xpmem_peers);
    } 

    if (0 != my_info.data_seg) {
        xpmem_remove(my_info.data_seg);
    }
    if (0 != my_info.heap_seg) {
        xpmem_remove(my_info.heap_seg);
    }

    return 0;
}

