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

    start_pes(0);
    me = _my_pe();
    num_pes = _num_pes();

    for (l = 0 ; l < loops ; ++l) {

        if ((src_int = shmalloc(sizeof(int))) == NULL) {
            printf("PE-%d int shmalloc() failed?\n", me);
            exit(1);
        }
        *src_int = 4;
        dst_int = itmp = 0;

        if ((src_long = shmalloc(sizeof(long))) == NULL) {
            printf("PE-%d long shmalloc() failed?\n", me);
            exit(1);
        }
        *src_long = 8;
        dst_long = ltmp = 0;

        if ((src_llong = shmalloc(sizeof(long long))) == NULL) {
            printf("PE-%d long long shmalloc() failed?\n", me);
            exit(1);
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

            dst_int = shmem_int_cswap(src_int,*src_int,0,1);
            if (dst_int != 4) {
                printf("PE-%d dst_int %d != 4?\n",me,dst_int);
                exit(1);
            }
            /* verify remote data */
            itmp = shmem_int_g(src_int,1);
            if (itmp != 0) {
                printf("PE-%d rem %d != 0?\n",me,itmp);
                exit(1);
            }
            Vprintf("PE-0 1st int_cswap done: local %d rem(%d)\n",dst_int,itmp);

            dst_int = shmem_int_cswap(src_int,0,dst_int,1);
            if (dst_int != 0) {
                printf("PE-%d dst_int %d != 0?\n",me,dst_int);
                exit(1);
            }
            /* verify remote data */
            itmp = shmem_int_g(src_int,1);
            if (itmp != 4) {
                printf("PE-%d rem %d != 4?\n",me,itmp);
                exit(1);
            }
            Vprintf("PE-0 2nd int_swap done: local %d rem(%d)\n",dst_int,itmp);

            /* cswap() should not swap as cond(0) != remote(4) */
            dst_int = shmem_int_cswap(src_int,0,0,1);
            if (dst_int != 4) {
                printf("PE-%d int no-swap returned dst_int %d != 4?\n",
                        me,dst_int);
                exit(1);
            }
            /* verify previous cswap() did not swap */
            itmp = shmem_int_g(src_int,1);
            if (itmp != 4) {
                printf("PE-%d failed cond int_cswap() swapped? rem(%d) != 4?\n",
                        me,itmp);
                exit(1);
            }

            /* long swap */
            ltmp = shmem_long_g(src_long,1);
            Vprintf("PE-0 Initial Conditions(long) local %ld rem(%ld)\n",
                    dst_long,ltmp);

            dst_long = shmem_long_cswap(src_long,*src_long,0,1);
            if (dst_long != 8) {
                printf("PE-%d dst_long %ld != 8?\n",me,dst_long);
                exit(1);
            }
            /* verify remote data */
            ltmp = shmem_long_g(src_long,1);
            if (ltmp != 0) {
                printf("PE-%d long rem(%ld) != 0?\n",me,ltmp);
                exit(1);
            }
            Vprintf("PE-0 1st long_cswap done: local %ld rem(%ld)\n",
                    dst_long,ltmp);

            dst_long = shmem_long_cswap(src_long,0,dst_long,1);
            if (dst_long != 0) {
                printf("PE-%d dst_long %ld != 0?\n",me,dst_long);
                exit(1);
            }
            /* verify remote data */
            ltmp = shmem_long_g(src_long,1);
            if (ltmp != 8) {
                printf("PE-%d long rem(%ld) != 8?\n",me,ltmp);
                exit(1);
            }
            Vprintf("PE-0 2nd long_swap done: local %ld rem(%ld)\n",
                    dst_long,ltmp);

            /* cswap() should not swap as cond(0) != remote(8) */
            dst_long = shmem_long_cswap(src_long,0,0,1);
            if (dst_long != 8) {
                printf("PE-%d long no-swap returned dst_long %ld != 8?\n",
                        me,dst_long);
                exit(1);
            }
            /* verify previous cswap() did not swap */
            ltmp = shmem_long_g(src_long,1);
            if (ltmp != 8) {
                printf("PE-%d failed cond long_cswap() swapped? rem(%ld) != 8?\n",
                        me,ltmp);
                exit(1);
            }

            /* long long swap */
            lltmp = shmem_longlong_g(src_llong,1);
            Vprintf("PE-0 Initial Conditions(long long) local %lld rem(%lld)\n",
                    dst_llong,lltmp);

            dst_llong = shmem_longlong_cswap(src_llong,*src_llong,0,1);
            if (dst_llong != 16) {
                printf("PE-%d dst_llong %lld != 16?\n",me,dst_llong);
                exit(1);
            }
            /* verify remote data */
            lltmp = shmem_longlong_g(src_llong,1);
            if (lltmp != 0) {
                printf("PE-%d longlong rem(%lld) != 0?\n",me,lltmp);
                exit(1);
            }
            Vprintf("PE-0 1st longlong_cswap done: local %lld rem(%lld)\n",
                    dst_llong, lltmp);

            dst_llong = shmem_longlong_cswap(src_llong,0,dst_llong,1);
            if (dst_llong != 0) {
                printf("PE-%d dst_llong %lld != 0?\n",me,dst_llong);
                exit(1);
            }
            /* verify remote data */
            lltmp = shmem_longlong_g(src_llong,1);
            if (lltmp != 16) {
                printf("PE-%d long long rem(%lld) != 16?\n",me,lltmp);
                exit(1);
            }
            Vprintf("PE-0 2nd longlong_swap done: local %lld rem(%lld)\n",
                    dst_llong,lltmp);

            /* cswap() should not swap as cond(0) != remote(8) */
            dst_llong = shmem_longlong_cswap(src_llong,0,0,1);
            if (dst_llong != 16) {
                printf("PE-%d longlong no-swap returned dst_llong %lld != 16?\n",
                        me,dst_llong);
                exit(1);
            }
            /* verify previous cswap() did not swap */
            lltmp = shmem_longlong_g(src_llong,1);
            if (lltmp != 16) {
                printf("PE-0 failed cond longlong_cswap() swapped? rem(%lld) != 16?\n",
                        lltmp);
                exit(1);
            }
        }
        else {
            if (!shmem_addr_accessible(src_int,0)) {
                printf("PE-%d local src_int %p not accessible from PE-%d?\n",
                        me, (void*)src_int, 0);
                exit(1);
            }
            if (!shmem_addr_accessible(src_long,0)) {
                printf("PE-%d local src_long %p not accessible from PE-%d?\n",
                        me, (void*)src_long, 0);
                exit(1);
            }
            if (!shmem_addr_accessible(src_llong,0)) {
                printf("PE-%d local src_llong %p not accessible from PE-%d?\n",
                        me, (void*)src_llong, 0);
                exit(1);
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

        (void)shmem_int_fadd( &itmp, me+1, 0 );
        (void)shmem_long_fadd( &ltmp, me+1, 0 );
        (void)shmem_longlong_fadd( &lltmp, me+1, 0 );

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

        (void)shmem_int_finc(src_int,0);
        (void)shmem_long_finc(src_long,0);
        (void)shmem_longlong_finc(src_llong,0);

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

        shfree(src_int);
        shfree(src_long);
        shfree(src_llong);
    }

    if (Verbose)
        fprintf(stderr,"[%d] exit\n",_my_pe());

    return 0;
}
