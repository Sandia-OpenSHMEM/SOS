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

#include <shmem.h>
#include <string.h>
#include <stdio.h>

int
main(int argc, char* argv[])
{
    long source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    static long target[10] = { 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 };
    int i, bad = 0;

    shmem_init();

    if (shmem_n_pes() == 1) {
        printf("%s: Requires number of PEs > 1\n", argv[0]);
        shmem_finalize();
        return 0;
    }

    if (shmem_my_pe() == 0) {
        for (i = 0 ; i < 10 ; ++i) {
            source[i] = shmem_long_atomic_swap(target + i, source[i], 1);
        }
    }

    shmem_barrier_all();  /* sync sender and receiver */

    if (shmem_my_pe() == 0) {
        for (i = 0 ; i < 10 ; ++i) {
            if (source[i] != i + 11) bad = 1;
        }
    } else if (shmem_my_pe() == 1) {
        for (i = 0 ; i < 10 ; ++i) {
            if (target[i] != i + 1) bad = 1;
        }
    }

    if (bad) {
        fprintf(stderr, "[%02d] error: ", shmem_my_pe());
        for (i = 0 ; i < 10 ; ++i) {
            fprintf(stderr, "%ld ", (shmem_my_pe() == 0) ? source[i] : target[i]);
        }
        fprintf(stderr, "\n");
    }

    shmem_finalize();

    return bad;
}
