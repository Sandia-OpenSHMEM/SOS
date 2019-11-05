/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2017 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
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
#include <sys/mman.h>
#include <fcntl.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"
#include "runtime.h"


#define MPIDI_OFI_SHMGR_NAME_MAXLEN (128)
#define MPIDI_OFI_SHMGR_NAME_PREFIX "/sos_shm_colls_area"

void shm_create_key(char *key, size_t max_size, unsigned pe, size_t num) {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  long long ticks = num; 
  snprintf(key, max_size,
      "%s-%u-%llX", MPIDI_OFI_SHMGR_NAME_PREFIX, pe, ticks);
}

void *shm_create_region(char* base, const char *key, int shm_size) {
  if(shm_size==0) return NULL;

  int fd = shm_open(key, O_RDWR | O_CREAT | O_TRUNC, 0666);
  if(fd == -1) {
    fprintf(stderr, "COLL_comm_init error shm_open with errno(%d)\n",errno);
    exit(0);
  }

  if(ftruncate(fd, shm_size) == -1) {
    fprintf(stderr, "COLL_comm_init error ftruncate\n");
    exit(0);
  }

  void *shm_base_addr = mmap(base, shm_size, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED, fd, 0);
  if(MAP_FAILED == shm_base_addr) {
    fprintf(stderr, "COLL_comm_init error mmap %s  size %d\n", key, shm_size);
    exit(0);
  }

  return shm_base_addr;
}

void *shm_attach_region(char* base, const char *key, int shm_size) {
  if(shm_size==0) return NULL;

  int fd = shm_open(key, O_RDWR, 0);
  if(fd == -1) {
    fprintf(stderr, "COLL_comm_init error shm_open\n");
    exit(0);
  }
  void *shm_base_addr = mmap(NULL, shm_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  if(MAP_FAILED == shm_base_addr) {
    fprintf(stderr, "COLL_comm_init error mmap 2 %s  size %d\n", key, shm_size);
    exit(0);
  }
  return shm_base_addr;
}


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
shmem_transport_xpmem_init(void)
{
    long page_size = sysconf(_SC_PAGESIZE);
    char *base;
    size_t len;
    int ret;
    char errmsg[256];
    char key_prefix[MPIDI_OFI_SHMGR_NAME_MAXLEN-10];
    char key[MPIDI_OFI_SHMGR_NAME_MAXLEN];

    /* setup data region */
    base = FIND_BASE(shmem_internal_data_base, page_size);
    len = FIND_LEN(shmem_internal_data_base, shmem_internal_data_length, page_size);
    my_info.data_seg = xpmem_make(base, len, XPMEM_PERMIT_MODE, (void*)0666);
    if (-1 == my_info.data_seg) {
        RETURN_ERROR_MSG("xpmem_make failed: %s\n",
                         shmem_util_strerror(errno, errmsg, 256));
        return 1;
    }
    my_info.data_off = (char*) shmem_internal_data_base - (char*) base;
    my_info.data_len = len;

    /* setup heap region */
    // Halit - trying mmap instead of XPMEM
    base = FIND_BASE(shmem_internal_heap_base, page_size);
    len  = FIND_LEN(shmem_internal_heap_base, shmem_internal_heap_length, page_size);
    shm_create_key(key_prefix, MPIDI_OFI_SHMGR_NAME_MAXLEN-10, shmem_internal_my_pe, 2);
    snprintf(key, MPIDI_OFI_SHMGR_NAME_MAXLEN, "%s-heap", key_prefix);
    void*myaddr = shm_create_region(base, key, len);
    my_info.heap_seg = 0; 
    //((int*)myaddr)[0] = 12*(shmem_internal_my_pe+1);
    //printf("shmem_transport_xpmem_startup\n");
    //shmem_runtime_barrier();
    
    if (-1 == my_info.heap_seg) {
        RETURN_ERROR_MSG("xpmem_make failed: %s\n",
                         shmem_util_strerror(errno, errmsg, 256));
        return 1;
    }
    my_info.heap_off = (char*) shmem_internal_heap_base - (char*) base;
    my_info.heap_len = len;
    

    ret = shmem_runtime_put("xpmem-segids", &my_info, sizeof(struct share_info_t));
    if (0 != ret) {
        RETURN_ERROR_MSG("runtime_put failed: %d\n", ret);
        return 1;
    }

    return 0;
}


int
shmem_transport_xpmem_startup(void)
{
    int ret, i, peer_num, num_on_node;
    char errmsg[256];
    struct share_info_t info;
    struct xpmem_addr addr;
    long page_size = sysconf(_SC_PAGESIZE);

    num_on_node = shmem_runtime_get_node_size();

    /* allocate space for local peers */
    shmem_transport_xpmem_peers = calloc(num_on_node,
                                         sizeof(struct shmem_transport_xpmem_peer_info_t));
    if (NULL == shmem_transport_xpmem_peers) return 1;

    /* get local peer info and map into our address space ... */
    for (i = 0 ; i < shmem_internal_num_pes; ++i) {
        peer_num = shmem_runtime_get_node_rank(i);
        if (-1 == peer_num) continue;

        if (shmem_internal_my_pe == i) {
            shmem_transport_xpmem_peers[peer_num].data_ptr =
                shmem_internal_data_base;
            shmem_transport_xpmem_peers[peer_num].heap_ptr =
                shmem_internal_heap_base;
        } else {
            ret = shmem_runtime_get(i, "xpmem-segids", &info, sizeof(struct share_info_t));
            if (0 != ret) {
                RETURN_ERROR_MSG("runtime_get failed: %d\n", ret);
                return 1;
            }
            
            char key_prefix[MPIDI_OFI_SHMGR_NAME_MAXLEN-10];
            char key[MPIDI_OFI_SHMGR_NAME_MAXLEN];
            int len = 0; 

            shmem_transport_xpmem_peers[peer_num].data_apid =
                xpmem_get(info.data_seg, XPMEM_RDWR, XPMEM_PERMIT_MODE, (void*)0666);

            if (shmem_transport_xpmem_peers[peer_num].data_apid < 0) {
                RETURN_ERROR_MSG("could not get data apid: %s\n",
                                 shmem_util_strerror(errno, errmsg, 256));
                return 1;
            }
            addr.apid = shmem_transport_xpmem_peers[peer_num].data_apid;
            addr.offset = 0;


            shmem_transport_xpmem_peers[peer_num].data_attach_ptr =
                xpmem_attach(addr, info.data_len, NULL);
            if ((size_t) shmem_transport_xpmem_peers[peer_num].data_ptr == XPMEM_MAXADDR_SIZE) {
                RETURN_ERROR_MSG("could not get data segment: %s\n",
                                 shmem_util_strerror(errno, errmsg, 256));
                return 1;
            }
            shmem_transport_xpmem_peers[peer_num].data_ptr =
                (char*) shmem_transport_xpmem_peers[peer_num].data_attach_ptr + info.data_off;


            addr.apid = shmem_transport_xpmem_peers[peer_num].heap_apid;
            addr.offset = 0;
            
            // Halit attach to neighbors
            len  = FIND_LEN(shmem_internal_heap_base, shmem_internal_heap_length, page_size);
            shm_create_key(key_prefix, MPIDI_OFI_SHMGR_NAME_MAXLEN-10, i, 2);
            snprintf(key, MPIDI_OFI_SHMGR_NAME_MAXLEN, "%s-heap", key_prefix);
            shmem_transport_xpmem_peers[peer_num].heap_attach_ptr = shm_attach_region(NULL, key, len);

            if ((size_t) shmem_transport_xpmem_peers[peer_num].heap_ptr == XPMEM_MAXADDR_SIZE) {
                RETURN_ERROR_MSG("could not get data segment: %s\n",
                                 shmem_util_strerror(errno, errmsg, 256));
                return 1;
            }
            shmem_transport_xpmem_peers[peer_num].heap_ptr =
                (char*) shmem_transport_xpmem_peers[peer_num].heap_attach_ptr + info.heap_off;
            //printf("PE[%d] reads data from PE[%d]: %d\n", shmem_internal_my_pe, peer_num, ((int*) shmem_transport_xpmem_peers[peer_num].heap_ptr)[0]);
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
            peer_num = shmem_runtime_get_node_rank(i);
            if (-1 == peer_num) continue;
            if (shmem_internal_my_pe == i) continue;

            if (NULL != shmem_transport_xpmem_peers[peer_num].data_ptr) {
                xpmem_detach(shmem_transport_xpmem_peers[peer_num].data_attach_ptr);
            }

            if (0 != shmem_transport_xpmem_peers[peer_num].data_apid) {
                xpmem_release(shmem_transport_xpmem_peers[peer_num].data_apid);
            }
            /*
            if (NULL != shmem_transport_xpmem_peers[peer_num].heap_ptr) {
                xpmem_detach(shmem_transport_xpmem_peers[peer_num].heap_attach_ptr);
            }

            if (0 != shmem_transport_xpmem_peers[peer_num].heap_apid) {
                xpmem_release(shmem_transport_xpmem_peers[peer_num].heap_apid);
            }*/
        }
        free(shmem_transport_xpmem_peers);
    }

    /*if (0 != my_info.data_seg) {
        xpmem_remove(my_info.data_seg);
    }

    if (0 != my_info.heap_seg) {
        xpmem_remove(my_info.heap_seg);
    }*/

    return 0;
}

