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
 *  exercise:
 *     shmem_*_cswap()
 *     shmem_*_fadd()
 *     shmem_*_finc()
 */
#include <shmem.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define Vprintf if (Verbose) printf

static int *src_int;
static long *src_long;
static long long *src_llong;

static int dst_int, itmp;
static long dst_long, ltmp;
static long long dst_llong, lltmp;

static int loops = 5;

int
main(int argc, char* argv[])
{
    int me, num_pes, l, pe;
    int Verbose = 0;

    shmem_init();
    me = shmem_my_pe();
    num_pes = shmem_n_pes();

    if (num_pes == 1) {
        printf("%s: Requires number of PEs > 1\n", argv[0]);
        shmem_finalize();
        return 0;
    }

    for (l = 0 ; l < loops ; ++l) {

        if ((src_int = shmem_malloc(sizeof(int))) == NULL) {
            printf("PE-%d int shmem_malloc() failed?\n", me);
            shmem_global_exit(1);
        }
        *src_int = 4;
        dst_int = itmp = 0;

        if ((src_long = shmem_malloc(sizeof(long))) == NULL) {
            printf("PE-%d long shmem_malloc() failed?\n", me);
            shmem_global_exit(1);
        }
        *src_long = 8;
        dst_long = ltmp = 0;

        if ((src_llong = shmem_malloc(sizeof(long long))) == NULL) {
            printf("PE-%d long long shmem_malloc() failed?\n", me);
            shmem_global_exit(1);
        }
        *src_llong = 16;
        dst_llong = lltmp = 0;

        //printf("PE-%d malloc()s done.\n",me);
        shmem_barrier_all();

        if ( me == 0 ) {
            /* integer swap */
            itmp = shmem_int_g(src_int,1);
            Vprintf("PE-0 Initial Conditions(int) local %d rem(%d)\n",
                    dst_int,itmp);

            dst_int = shmem_int_atomic_compare_swap(src_int,*src_int,0,1);
            if (dst_int != 4) {
                printf("PE-%d dst_int %d != 4?\n",me,dst_int);
                shmem_global_exit(1);
            }
            /* verify remote data */
            itmp = shmem_int_g(src_int,1);
            if (itmp != 0) {
                printf("PE-%d rem %d != 0?\n",me,itmp);
                shmem_global_exit(1);
            }
            Vprintf("PE-0 1st int_cswap done: local %d rem(%d)\n",dst_int,itmp);

            dst_int = shmem_int_atomic_compare_swap(src_int,0,dst_int,1);
            if (dst_int != 0) {
                printf("PE-%d dst_int %d != 0?\n",me,dst_int);
                shmem_global_exit(1);
            }
            /* verify remote data */
            itmp = shmem_int_g(src_int,1);
            if (itmp != 4) {
                printf("PE-%d rem %d != 4?\n",me,itmp);
                shmem_global_exit(1);
            }
            Vprintf("PE-0 2nd int_swap done: local %d rem(%d)\n",dst_int,itmp);

            /* cswap() should not swap as cond(0) != remote(4) */
            dst_int = shmem_int_atomic_compare_swap(src_int,0,0,1);
            if (dst_int != 4) {
                printf("PE-%d int no-swap returned dst_int %d != 4?\n",
                        me,dst_int);
                shmem_global_exit(1);
            }
            /* verify previous cswap() did not swap */
            itmp = shmem_int_g(src_int,1);
            if (itmp != 4) {
                printf("PE-%d failed cond int_cswap() swapped? rem(%d) != 4?\n",
                        me,itmp);
                shmem_global_exit(1);
            }

            /* long swap */
            ltmp = shmem_long_g(src_long,1);
            Vprintf("PE-0 Initial Conditions(long) local %ld rem(%ld)\n",
                    dst_long,ltmp);

            dst_long = shmem_long_atomic_compare_swap(src_long,*src_long,0,1);
            if (dst_long != 8) {
                printf("PE-%d dst_long %ld != 8?\n",me,dst_long);
                shmem_global_exit(1);
            }
            /* verify remote data */
            ltmp = shmem_long_g(src_long,1);
            if (ltmp != 0) {
                printf("PE-%d long rem(%ld) != 0?\n",me,ltmp);
                shmem_global_exit(1);
            }
            Vprintf("PE-0 1st long_cswap done: local %ld rem(%ld)\n",
                    dst_long,ltmp);

            dst_long = shmem_long_atomic_compare_swap(src_long,0,dst_long,1);
            if (dst_long != 0) {
                printf("PE-%d dst_long %ld != 0?\n",me,dst_long);
                shmem_global_exit(1);
            }
            /* verify remote data */
            ltmp = shmem_long_g(src_long,1);
            if (ltmp != 8) {
                printf("PE-%d long rem(%ld) != 8?\n",me,ltmp);
                shmem_global_exit(1);
            }
            Vprintf("PE-0 2nd long_swap done: local %ld rem(%ld)\n",
                    dst_long,ltmp);

            /* cswap() should not swap as cond(0) != remote(8) */
            dst_long = shmem_long_atomic_compare_swap(src_long,0,0,1);
            if (dst_long != 8) {
                printf("PE-%d long no-swap returned dst_long %ld != 8?\n",
                        me,dst_long);
                shmem_global_exit(1);
            }
            /* verify previous cswap() did not swap */
            ltmp = shmem_long_g(src_long,1);
            if (ltmp != 8) {
                printf("PE-%d failed cond long_cswap() swapped? rem(%ld) != 8?\n",
                        me,ltmp);
                shmem_global_exit(1);
            }

            /* long long swap */
            lltmp = shmem_longlong_g(src_llong,1);
            Vprintf("PE-0 Initial Conditions(long long) local %lld rem(%lld)\n",
                    dst_llong,lltmp);

            dst_llong = shmem_longlong_atomic_compare_swap(src_llong,*src_llong,0,1);
            if (dst_llong != 16) {
                printf("PE-%d dst_llong %lld != 16?\n",me,dst_llong);
                shmem_global_exit(1);
            }
            /* verify remote data */
            lltmp = shmem_longlong_g(src_llong,1);
            if (lltmp != 0) {
                printf("PE-%d longlong rem(%lld) != 0?\n",me,lltmp);
                shmem_global_exit(1);
            }
            Vprintf("PE-0 1st longlong_cswap done: local %lld rem(%lld)\n",
                    dst_llong, lltmp);

            dst_llong = shmem_longlong_atomic_compare_swap(src_llong,0,dst_llong,1);
            if (dst_llong != 0) {
                printf("PE-%d dst_llong %lld != 0?\n",me,dst_llong);
                shmem_global_exit(1);
            }
            /* verify remote data */
            lltmp = shmem_longlong_g(src_llong,1);
            if (lltmp != 16) {
                printf("PE-%d long long rem(%lld) != 16?\n",me,lltmp);
                shmem_global_exit(1);
            }
            Vprintf("PE-0 2nd longlong_swap done: local %lld rem(%lld)\n",
                    dst_llong,lltmp);

            /* cswap() should not swap as cond(0) != remote(8) */
            dst_llong = shmem_longlong_atomic_compare_swap(src_llong,0,0,1);
            if (dst_llong != 16) {
                printf("PE-%d longlong no-swap returned dst_llong %lld != 16?\n",
                        me,dst_llong);
                shmem_global_exit(1);
            }
            /* verify previous cswap() did not swap */
            lltmp = shmem_longlong_g(src_llong,1);
            if (lltmp != 16) {
                printf("PE-0 failed cond longlong_cswap() swapped? rem(%lld) != 16?\n",
                        lltmp);
                shmem_global_exit(1);
            }
        }
        else {
            if (!shmem_addr_accessible(src_int,0)) {
                printf("PE-%d local src_int %p not accessible from PE-%d?\n",
                        me, (void*)src_int, 0);
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

        /* shmem_*fadd() exercise */

        if (me == 0) {
            itmp = 0;
            ltmp = 0;
            lltmp = 0;
            *src_int = 0;
            *src_long = 0;
            *src_llong = 0;
        }
        shmem_barrier_all();

        (void)shmem_int_atomic_fetch_add( &itmp, me+1, 0 );
        (void)shmem_long_atomic_fetch_add( &ltmp, me+1, 0 );
        (void)shmem_longlong_atomic_fetch_add( &lltmp, me+1, 0 );

        shmem_barrier_all();

        if (me == 0) {
            int tot;

            for(pe=0,tot=0; pe < num_pes; pe++)
                tot += pe+1;

            if ( itmp != tot )
                printf("fadd() total %d != expected %d?\n",itmp,tot);

            if ( ltmp != (long)tot )
                printf("fadd() total %ld != expected %d?\n",ltmp,tot);

            if ( lltmp != (long long)tot )
                printf("fadd() total %lld != expected %d?\n",lltmp,tot);
        }
        shmem_barrier_all();

        (void)shmem_int_atomic_fetch_inc(src_int,0);
        (void)shmem_long_atomic_fetch_inc(src_long,0);
        (void)shmem_longlong_atomic_fetch_inc(src_llong,0);

        shmem_barrier_all();

        if (me == 0) {
            int tot = num_pes;

            if ( *src_int != tot )
                printf("finc() total %d != expected %d?\n",*src_int,tot);

            if ( *src_long != (long)tot )
                printf("finc() total %ld != expected %d?\n",*src_long,tot);

            if ( *src_llong != (long long)tot )
                printf("finc() total %lld != expected %d?\n",*src_llong,tot);
        }
        shmem_barrier_all();

        shmem_free(src_int);
        shmem_free(src_long);
        shmem_free(src_llong);
    }

    if (Verbose)
        fprintf(stderr,"[%d] exit\n",shmem_my_pe());

    shmem_finalize();

    return 0;
}
