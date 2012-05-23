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

#include <xpmem.h>


struct shmem_transport_xpmem_peer_info_t {
    xpmem_apid_t data_apid;
    xpmem_apid_t heap_apid;
    void *data_ptr;
    void *heap_ptr;
};

extern struct shmem_transport_xpmem_peer_info_t *shmem_transport_xpmem_peers;


int shmem_transport_xpmem_init(void);

int shmem_transport_xpmem_startup(void);

int shmem_transport_xpmem_fini(void);

#endif
