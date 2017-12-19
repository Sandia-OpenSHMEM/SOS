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
 * swapm - exercise shmem_*_swap()
 */

#include <shmem.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define Vprintf if (Verbose) printf

static int *src_int;
static float *src_float;
static double *src_double;
static long *src_long;
static long long *src_llong;

static int *dst_int, itmp;
static float *dst_float, ftmp;
static double *dst_double, dtmp;
static long *dst_long, ltmp;
static long long *dst_llong, lltmp;

static int loops = 10;

int
main(int argc, char* argv[])
{
    int me, num_procs, l, j;
    int Verbose = 0;

    shmem_init();
    me = shmem_my_pe();
    num_procs = shmem_n_pes();
    if ( num_procs < 2 ) {
        if (me ==0)
            printf("PE[0] requires 2 or more PEs?\n");
        shmem_finalize();
        return 0;
    }

    for (l = 0 ; l < loops ; ++l) {

        if ((src_int = shmem_malloc(2*num_procs*sizeof(int))) == NULL) {
            printf("PE-%d int shmem_malloc() failed?\n", me);
            shmem_global_exit(1);
        }
        dst_int = &src_int[num_procs];
        for(j=0; j < num_procs; j++) {
            src_int[j] = 4;
            dst_int[j] = 0;
        }

        if ((src_float = shmem_malloc(2*num_procs*sizeof(float))) == NULL) {
            printf("PE-%d float shmem_malloc() failed?\n", me);
            shmem_global_exit(1);
        }
        dst_float = &src_float[num_procs];
        for(j=0; j < num_procs; j++) {
            src_float[j] = 4.0;
            dst_float[j] = 0.0;
        }

        if ((src_double = shmem_malloc(2*num_procs*sizeof(double))) == NULL) {
            printf("PE-%d double shmem_malloc() failed?\n", me);
            shmem_global_exit(1);
        }
        dst_double = &src_double[num_procs];
        for(j=0; j < num_procs; j++) {
            src_double[j] = 8.0;
            dst_double[j] = 0.0;
        }

        if ((src_long = shmem_malloc(2*num_procs*sizeof(long))) == NULL) {
            printf("PE-%d long shmem_malloc() failed?\n", me);
            shmem_global_exit(1);
        }
        dst_long = &src_long[num_procs];
        for(j=0; j < num_procs; j++) {
            src_long[j] = 8;
            dst_long[j] = 0;
        }

        if ((src_llong = shmem_malloc(2*num_procs*sizeof(long long))) == NULL) {
            printf("PE-%d long shmem_malloc() failed?\n", me);
            shmem_global_exit(1);
        }
        dst_llong = &src_llong[num_procs];
        for(j=0; j < num_procs; j++) {
            src_llong[j] = 16;
            dst_llong[j] = 0;
        }

        shmem_barrier_all();

        if ( me != 0 ) {
            /* is 'src_*' accessible from PE0? should be. */
            if (!shmem_addr_accessible(src_int,0)) {
                printf("PE-%d local src_int %p not accessible from PE-%d?\n",
                       me, (void*)src_int, 0);
                shmem_global_exit(1);
            }
            if (!shmem_addr_accessible(src_float,0)) {
                printf("PE-%d local src_float %p not accessible from PE-%d?\n",
                       me, (void*)src_float, 0);
                shmem_global_exit(1);
            }
            if (!shmem_addr_accessible(src_double,0)) {
                printf("PE-%d local src_double %p not accessible from PE-%d?\n",
                       me, (void*)src_double, 0);
                shmem_global_exit(1);
            }
            if (!shmem_addr_accessible(src_long,0)) {
                printf("PE-%d local src_long %p not accessible from PE-%d?\n",
                       me, (void*)src_long, 0);
                shmem_global_exit(1);
            }
            if (!shmem_addr_accessible(src_llong,0)) {
                printf("PE-%d local src_llong %p not accessible from PE-%d?\n",
                       me, (void*)src_llong, 0);
                shmem_global_exit(1);
            }
        }
        shmem_barrier_all();

        if ( me == 0 ) {
            shmem_quiet();
            for(j=1; j < num_procs; j++) {
                dst_int[j] = shmem_int_atomic_swap(src_int+j,0,j);
                if (dst_int[j] != 4) {
                    printf("PE-%d dst_int[%d] %d != 4?\n",me,j,dst_int[j]);
                    shmem_global_exit(1);
                }
            }
            shmem_barrier_all();

            /* verify remote data */
            for(j=1; j < num_procs; j++) {
                itmp = shmem_int_g(src_int+j,j);
                if (itmp != 0) {
                    printf("PE-0 int PE[%d] rem(%d) != 0?\n",j,itmp);
                    shmem_global_exit(1);
                }

                /* swap back */
                dst_int[j] = shmem_int_atomic_swap(src_int+j,dst_int[j],j);
                if (dst_int[j] != 0) {
                    printf("PE-0 dst_int[%d] %d != 0?\n",j,dst_int[j]);
                    shmem_global_exit(1);
                }

                itmp = shmem_int_g(src_int+j,j);
                if (itmp != 4) {
                    printf("PE-0 PE[%d] rem %d != 4?\n",j,itmp);
                    shmem_global_exit(1);
                }
            }

            for(j=1; j < num_procs; j++) {
                dst_float[j] = shmem_float_atomic_swap(src_float+j,0.0,j);
                if (dst_float[j] != 4.0) {
                    printf("PE-0 dst_float[%d] %f != 4.0?\n",j,dst_float[j]);
                    shmem_global_exit(1);
                }

                /* verify remote data */
                ftmp = shmem_float_g(src_float+j,j);
                if (ftmp != 0.0) {
                    printf("PE-0 float rem(%f) != 0.0?\n",ftmp);
                    shmem_global_exit(1);
                }
                /* swap back */
                dst_float[j] = shmem_float_atomic_swap(src_float+j,dst_float[j],j);
                if (dst_float[j] != 0.0) {
                    printf("PE-0 dst_float[%d] %f != 0.0?\n",j,dst_float[j]);
                    shmem_global_exit(1);
                }
                ftmp = shmem_float_g(src_float+j,j);
                if (ftmp != 4.0) {
                    printf("PE-%d float rem(%f) != 4.0?\n",me,ftmp);
                    shmem_global_exit(1);
                }
            }

            for(j=1; j < num_procs; j++) {
                dst_double[j] = shmem_double_atomic_swap(src_double+j,0.0,j);
                if (dst_double[j] != 8.0) {
                    printf("PE-0 dst_double[%d] %f != 8.0?\n",j,dst_double[j]);
                    shmem_global_exit(1);
                }
                /* verify remote data */
                dtmp = shmem_double_g(src_double+j,j);
                if (dtmp != 0.0) {
                    printf("PE-0 float rem(%f) != 0.0?\n",dtmp);
                    shmem_global_exit(1);
                }
                dst_double[j] = shmem_double_atomic_swap(src_double+j,dst_double[j],j);
                if (dst_double[j] != 0.0) {
                    printf("PE-0 dst_double[%d] %f != 0.0?\n",j,dst_double[j]);
                    shmem_global_exit(1);
                }
                dtmp = shmem_double_g(src_double+j,j);
                if (dtmp != 8.0) {
                    printf("PE-0 double rem(%f) != 8.0?\n",dtmp);
                    shmem_global_exit(1);
                }
            }

            for(j=1; j < num_procs; j++) {
                dst_long[j] = shmem_long_atomic_swap(src_long+j,0,j);
                if (dst_long[j] != 8) {
                    printf("PE-0 dst_long[%d] %ld != 8?\n",j,dst_long[j]);
                    shmem_global_exit(1);
                }
            }
            shmem_barrier_all();

            /* verify remote data */
            for(j=1; j < num_procs; j++) {
                ltmp = shmem_long_g(src_long+j,j);
                if (ltmp != 0) {
                    printf("PE-0 PE[%d]long rem(%ld) != 0?\n",j,ltmp);
                    shmem_global_exit(1);
                }
                /* swap back */
                dst_long[j] = shmem_long_atomic_swap(src_long+j,dst_long[j],j);
                if (dst_long[j] != 0) {
                    printf("PE-%d dst_long[%d] %ld != 0?\n",me,j,dst_long[j]);
                    shmem_global_exit(1);
                }
                ltmp = shmem_long_g(src_long+j,j);
                if (ltmp != 8) {
                    printf("PE-%d long rem(%ld) != 8?\n",me,ltmp);
                    shmem_global_exit(1);
                }
            }

            for(j=1; j < num_procs; j++) {
                dst_llong[j] = shmem_longlong_atomic_swap(src_llong+j,0,j);
                if (dst_llong[j] != 16) {
                    printf("PE-%d dst_llong[%d] %lld != 16?\n",me,j,dst_llong[j]);
                    shmem_global_exit(1);
                }
            }
            shmem_barrier_all();

            /* verify remote data */
            for(j=1; j < num_procs; j++) {
                lltmp = shmem_longlong_g(src_llong+j,j);
                if (lltmp != 0) {
                    printf("PE-%d long long rem(%lld) != 0?\n",me,lltmp);
                    shmem_global_exit(1);
                }
                /* swap back */
                dst_llong[j] = shmem_longlong_atomic_swap(src_llong+j,dst_llong[j],j);
                if (dst_llong[j] != 0) {
                    printf("PE-%d  dst_llong[%d] %lld != 0?\n", me,j,dst_llong[j]);
                    shmem_global_exit(1);
                }
                lltmp = shmem_longlong_g(src_llong+j,j);
                if (lltmp != 16) {
                    printf("PE-%d longlong rem(%lld) != 16?\n",me,lltmp);
                    shmem_global_exit(1);
                }
            }
        }
        else {
            shmem_int_wait_until(&src_int[me],SHMEM_CMP_EQ,0);
            shmem_barrier_all();

            shmem_long_wait_until(&src_long[me],SHMEM_CMP_EQ,0);
            shmem_barrier_all();

            shmem_longlong_wait_until(&src_llong[me],SHMEM_CMP_EQ,0);
            shmem_barrier_all();
        }

        shmem_barrier_all();

        shmem_free(src_int);
        shmem_free(src_float);
        shmem_free(src_double);
        shmem_free(src_long);
        shmem_free(src_llong);
    }

    if (Verbose)
        fprintf(stderr,"[%d] exit\n",shmem_my_pe());

    shmem_finalize();

    return 0;
}
