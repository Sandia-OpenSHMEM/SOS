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

#ifndef SHMEM_TRANSPORT_CMA_H
#define SHMEM_TRANSPORT_CMA_H

#ifndef _GNU_SOURCE
#error CMA transport requires definition of _GNU_SOURCE
#endif

#include <sys/uio.h>
#ifndef HAVE_LIBC_CMA
#include <sys/syscall.h>
#endif

#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <inttypes.h>

#include "shmem_internal.h"

extern pid_t shmem_transport_cma_my_pid;
extern pid_t *shmem_transport_cma_peers;

int shmem_transport_cma_init(void);
int shmem_transport_cma_startup(void);
int shmem_transport_cma_fini(void);

/*
 * Validate address is within SHMEM bounds: data and/or symHeap.
 */
#ifdef ENABLE_ERROR_CHECKING
#define CHK_ACCESS(target,name)                                         \
    do {                                                                \
        if (((void*) target > shmem_internal_data_base) &&              \
            ((char*) target < (char*) shmem_internal_data_base + shmem_internal_data_length)) { \
        } else if (((void*) target > shmem_internal_heap_base) &&       \
                   ((char*) target < (char*) shmem_internal_heap_base + shmem_internal_heap_length)) { \
        } else {                                                        \
            RAISE_ERROR_MSG("%s (0x%"PRIXPTR") outside of symmetric areas\n", \
                            name, (uintptr_t) target);                  \
        }                                                               \
    } while (0)
#else   // ! ENABLE_ERROR_CHECKING
#define CHK_ACCESS(target,id)
#endif

#ifndef HAVE_LIBC_CMA

static inline
ssize_t process_vm_readv(pid_t pid,
                const struct iovec  *lvec,
                unsigned long liovcnt,
                const struct iovec *rvec,
                unsigned long riovcnt,
                unsigned long flags)
{
  return syscall(__NR_process_vm_readv, pid, lvec, liovcnt, rvec, riovcnt, flags);
}

static inline
ssize_t process_vm_writev(pid_t pid,
              const struct iovec  *lvec,
              unsigned long liovcnt,
              const struct iovec *rvec,
              unsigned long riovcnt,
              unsigned long flags)
{
  return syscall(__NR_process_vm_writev, pid, lvec, liovcnt, rvec, riovcnt, flags);
}
#endif // !HAVE_LIBC_CMA

static inline void
shmem_transport_cma_put(void *target, const void *source, size_t len,
                        int pe, int noderank)
{
        ssize_t bytes;
        struct iovec tgt, src;
        pid_t target_pid = shmem_transport_cma_peers[noderank];

        CHK_ACCESS(target,"cma_put target");

        if ( target_pid == shmem_transport_cma_my_pid ) {
            memcpy(target, source, len);
            return;
        }

        tgt.iov_base = target;
        tgt.iov_len = len;
        src.iov_base = (void*)source;
        src.iov_len = len;
        bytes = process_vm_writev( target_pid,
                        (const struct iovec *)&src, 1,
                        (const struct iovec *)&tgt, 1, 0);

        if ( bytes < 0 || (size_t) bytes != len) {
            char errmsg[256];
            RAISE_ERROR_MSG("process_vm_writev() failed (%s)\n",
                            shmem_util_strerror(errno, errmsg, 256));
        }
}


static inline void
shmem_transport_cma_get(void *target, const void *source, size_t len, int pe,
                        int noderank)
{
        ssize_t bytes;
        struct iovec tgt, src;
        pid_t target_pid = shmem_transport_cma_peers[noderank];

        CHK_ACCESS(source,"cma_get source");

        if ( target_pid == shmem_transport_cma_my_pid ) {
            memcpy(target, source, len);
            return;
        }

        tgt.iov_base = target;
        src.iov_base = (void*)source;
        tgt.iov_len = src.iov_len = len;
        bytes = process_vm_readv(target_pid,
                                (const struct iovec *)&tgt, 1,
                                (const struct iovec *)&src, 1, 0);
        if ( bytes < 0 || (size_t) bytes != len) {
            char errmsg[256];
            RAISE_ERROR_MSG("process_vm_readv() failed (%s)\n",
                            shmem_util_strerror(errno, errmsg, 256));
        }
}

#endif /* SHMEM_TRANSPORT_CMA_H */
