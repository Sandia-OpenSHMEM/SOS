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
#include <stdlib.h>
#include <stdio.h>

static short *src_short;
static int *src_int;
static float *src_float;
static double *src_double;
static long *src_long;

static short dst_short;
static int dst_int;
static float dst_float;
static double dst_double;
static long dst_long;

static int loops = 100;

int
main(int argc, char* argv[])
{
    int me, num_pes, pe, l;
    int Verbose = 0;
    char *pgm;

    if ((pgm=strrchr(argv[0],'/'))) {
        pgm++;
    } else {
        pgm = argv[0];
    }

    if (argc > 1) {
        if (strncmp(argv[1],"-v",3) == 0) {
            Verbose=1;
        } else if (strncmp(argv[1],"-h",3) == 0) {
            fprintf(stderr,"usage: %s {-v(verbose)|h(help)}\n",pgm);
            shmem_finalize();
            exit(1);
        }
    }

    shmem_init();
    me = shmem_my_pe();
    num_pes = shmem_n_pes();

    // be a bit sane with total number of gets issued
    loops = loops / num_pes;
    if (loops < 5) loops = 5;

    for (l = 0 ; l < loops ; ++l) {
        if ((src_short = shmem_malloc(sizeof(short))) == NULL) {
            printf("PE-%d short shmem_malloc() failed?\n", me);
            shmem_global_exit(1);
        }
        *src_short = 2;

        if ((src_int = shmem_malloc(sizeof(int))) == NULL) {
            printf("PE-%d int shmem_malloc() failed?\n", me);
            shmem_global_exit(1);
        }
        *src_int = 4;

        if ((src_float = shmem_malloc(sizeof(float))) == NULL) {
            printf("PE-%d float shmem_malloc() failed?\n", me);
            shmem_global_exit(1);
        }
        *src_float = 4.0;

        if ((src_double = shmem_malloc(sizeof(double))) == NULL) {
            printf("PE-%d double shmem_malloc() failed?\n", me);
            shmem_global_exit(1);
        }
        *src_double = 8.0;

        if ((src_long = shmem_malloc(sizeof(long))) == NULL) {
            printf("PE-%d long shmem_malloc() failed?\n", me);
            shmem_global_exit(1);
        }
        *src_long = 8;

        shmem_barrier_all();

        for (pe=0 ; pe < num_pes; ++pe) {
            if (!shmem_addr_accessible(src_short,pe)) {
                printf("PE-%d local addr %p not accessible from PE-%d?\n",
                       me, (void*)src_short, pe);
                shmem_global_exit(1);
            }
            dst_short = 0;
            dst_short = shmem_short_g(src_short,pe);
            if (dst_short != 2) {
                printf("PE-%d dst_short %hd != 2?\n",me,dst_short);
                shmem_global_exit(1);
            }
            dst_int = 0;
            dst_int = shmem_int_g(src_int,pe);
            if (dst_int != 4) {
                printf("PE-%d dst_int %d != 4?\n",me,dst_int);
                shmem_global_exit(1);
            }
            dst_float = 0.0;
            dst_float = shmem_float_g(src_float,pe);
            if (dst_float != 4.0) {
                printf("PE-%d dst_float %f != 4.0?\n",me,dst_float);
                shmem_global_exit(1);
            }
            dst_double = 0.0;
            dst_double = shmem_double_g(src_double,pe);
            if (dst_double != 8.0) {
                printf("PE-%d dst_double %f != 8.0?\n",me,dst_double);
                shmem_global_exit(1);
            }
            dst_long = 0;
            dst_long = shmem_long_g(src_long,pe);
            if (dst_long != 8.0) {
                printf("PE-%d dst_long %ld != 8?\n",me,dst_long);
                shmem_global_exit(1);
            }
        }

        shmem_barrier_all();

        shmem_free(src_short);
        shmem_free(src_int);
        shmem_free(src_float);
        shmem_free(src_double);
        shmem_free(src_long);
    }

    if (Verbose)
        fprintf(stderr,"[%d] exit\n",shmem_my_pe());

    shmem_finalize();

    return 0;
}
