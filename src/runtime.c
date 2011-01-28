/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
 */

#include "config.h"
#include "shmem.h"

#include <portals4.h>
#include <portals4_runtime.h>
#include <stdlib.h>

ptl_handle_ni_t ni_h;
ptl_handle_le_t le_h;

void
start_pes(int npes)
{
    int ret;
    ptl_process_t *mapping;
    
    
    runtime_init();

    /* Initialize Portals */
    mapping  = malloc(sizeof(ptl_process_t) * _num_pes());
    ret = PtlInit();
    ret = PtlNIInit(PTL_IFACE_DEFAULT,
                    PTL_NI_NO_MATCHING | PTL_NI_LOGICAL,
                    PTL_PID_ANY,
                    NULL,
                    NULL,
                    _num_pes(),
                    NULL,
                    mapping,
                    &ni_h);


    /* BWB: Fix me - there should be some symmetric heap initialization here */
    
    /* Open LE to all memory */
    

    runtime_barrier();
}

int
_num_pes(void)
{
    return runtime_get_size();
}


int
_my_pe(void)
{
    return runtime_get_rank();
}

