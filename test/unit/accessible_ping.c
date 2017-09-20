/*
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 *  Copyright (c) 2017 Intel Corporation. All rights reserved.
 *  This software is available to you under the BSD license below:
 *
 *      Redistribution and use in source and binary forms, with or
 *      without modification, are permitted provided that the following
 *      conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
 * test if PE is accessible
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <shmem.h>

int
main(int argc, char* argv[])
{
    int me, npes;
    setbuf(stdout, NULL);
    shmem_init();
    me = shmem_my_pe();
    npes = shmem_n_pes();
    if (me == 0) {
        int i;
        int verbose = (NULL == getenv("MAKELEVEL")) ? 1 : 0;
        for (i = 1; i < npes; i += 1) {
            if (verbose) {
                printf("From %d: PE %d is ", me, i);
                printf("%s", shmem_pe_accessible(i) ? "" : "NOT ");
                printf("accessible\n");
            }
            if (! shmem_pe_accessible(i))
                shmem_global_exit(1);
        }
    }

    shmem_finalize();

    return 0;
}
