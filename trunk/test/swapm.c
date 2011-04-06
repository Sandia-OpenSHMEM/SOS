/*
 * swapm - exercise shmem_*_swap()
 */

#include <mpp/shmem.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define Vprintf if (Verbose) printf

static int *src_int;
static float *src_float;
static double *src_double;
static long *src_long;
static long long *src_llong;

static int dst_int, itmp;
static float dst_float, ftmp;
static double dst_double, dtmp;
static long dst_long, ltmp;
static long long dst_llong, lltmp;

static int loops = 10;

int
main(int argc, char* argv[])
{
    int me, num_pes, l;
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
        dst_int = 0;

        if ((src_float = shmalloc(sizeof(float))) == NULL) {
            printf("PE-%d float shmalloc() failed?\n", me);
            exit(1);
        }
        *src_float = 4.0;
        dst_float = 0.0;

        if ((src_double = shmalloc(sizeof(double))) == NULL) {
            printf("PE-%d double shmalloc() failed?\n", me);
            exit(1);
        }
        *src_double = 8.0;
        dst_double = 0.0;

        if ((src_long = shmalloc(sizeof(long))) == NULL) {
            printf("PE-%d long shmalloc() failed?\n", me);
            exit(1);
        }
        *src_long = 8;
        dst_long = 0;

        if ((src_llong = shmalloc(sizeof(long long))) == NULL) {
            printf("PE-%d long shmalloc() failed?\n", me);
            exit(1);
        }
        *src_llong = 16;
        dst_llong = 0;

        shmem_barrier_all();

        if ( me != 0 ) {
            /* is 'src_*' accessible from PE0? should be. */
            if (!shmem_addr_accessible(src_int,0)) {
                printf("PE-%d local src_int %p not accessible from PE-%d?\n",
                       me, (void*)src_int, 0);
                exit(1);
            }
            if (!shmem_addr_accessible(src_float,0)) {
                printf("PE-%d local src_float %p not accessible from PE-%d?\n",
                       me, (void*)src_float, 0);
                exit(1);
            }
            if (!shmem_addr_accessible(src_double,0)) {
                printf("PE-%d local src_double %p not accessible from PE-%d?\n",
                       me, (void*)src_double, 0);
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

        if ( me == 0 ) {
            dst_int = shmem_int_swap(src_int,0,1);
            if (dst_int != 4) {
                printf("PE-%d dst_int %d != 4?\n",me,dst_int);
                exit(1);
            }
            /* verify remote data */
            itmp = shmem_int_g(src_int,1);
            if (itmp != 0) {
                printf("PE-0 int rem(%d) != 0?\n",itmp);
                exit(1);
            }
            /* swap back */
            dst_int = shmem_int_swap(src_int,dst_int,1);
            if (dst_int != 0) {
                printf("PE-0 dst_int %d != 0?\n",dst_int);
                exit(1);
            }
            itmp = shmem_int_g(src_int,1);
            if (itmp != 4) {
                printf("PE-0 rem %d != 4?\n",itmp);
                exit(1);
            }

            dst_float = shmem_float_swap(src_float,0.0,1);
            if (dst_float != 4.0) {
                printf("PE-0 dst_float %f != 4.0?\n",dst_float);
                exit(1);
            }
            /* verify remote data */
            ftmp = shmem_float_g(src_float,1);
            if (ftmp != 0.0) {
                printf("PE-0 float rem(%f) != 0.0?\n",ftmp);
                exit(1);
            }
            /* swap back */
            dst_float = shmem_float_swap(src_float,dst_float,1);
            if (dst_float != 0.0) {
                printf("PE-0 dst_float %f != 0.0?\n",dst_float);
                exit(1);
            }
            ftmp = shmem_float_g(src_float,1);
            if (ftmp != 4.0) {
                printf("PE-%d float rem(%f) != 4.0?\n",me,ftmp);
                exit(1);
            }

            dst_double = shmem_double_swap(src_double,0.0,1);
            if (dst_double != 8.0) {
                printf("PE-0 dst_double %f != 8.0?\n",dst_double);
                exit(1);
            }
            /* verify remote data */
            dtmp = shmem_double_g(src_double,1);
            if (dtmp != 0.0) {
                printf("PE-0 float rem(%f) != 0.0?\n",dtmp);
                exit(1);
            }
            dst_double = shmem_double_swap(src_double,dst_double,1);
            if (dst_double != 0.0) {
                printf("PE-0 dst_double %f != 0.0?\n",dst_double);
                exit(1);
            }
            dtmp = shmem_double_g(src_double,1);
            if (dtmp != 8.0) {
                printf("PE-0 double rem(%f) != 8.0?\n",dtmp);
                exit(1);
            }

            dst_long = shmem_long_swap(src_long,0,1);
            if (dst_long != 8) {
                printf("PE-0 dst_long %ld != 8?\n",dst_long);
                exit(1);
            }
            /* verify remote data */
            ltmp = shmem_long_g(src_long,1);
            if (ltmp != 0) {
                printf("PE-0 long rem(%ld) != 0?\n",ltmp);
                exit(1);
            }
            /* swap back */
            dst_long = shmem_long_swap(src_long,dst_long,1);
            if (dst_long != 0) {
                printf("PE-%d dst_long %ld != 0?\n",me,dst_long);
                exit(1);
            }
            ltmp = shmem_long_g(src_long,1);
            if (ltmp != 8) {
                printf("PE-%d long rem(%ld) != 8?\n",me,ltmp);
                exit(1);
            }

            dst_llong = shmem_longlong_swap(src_llong,0,1);
            if (dst_llong != 16) {
                printf("PE-%d dst_long %lld != 16?\n",me,dst_llong);
                exit(1);
            }
            /* verify remote data */
            lltmp = shmem_longlong_g(src_llong,1);
            if (lltmp != 0) {
                printf("PE-%d long long rem(%lld) != 0?\n",me,lltmp);
                exit(1);
            }
            /* swap back */
            dst_llong = shmem_longlong_swap(src_llong,dst_llong,1);
            if (dst_llong != 0) {
                printf("PE-%d dst_llong %lld != 0?\n",me,dst_llong);
                exit(1);
            }
            lltmp = shmem_longlong_g(src_llong,1);
            if (lltmp != 16) {
                printf("PE-%d longlong rem(%lld) != 16?\n",me,lltmp);
                exit(1);
            }
        }
        else {
            shmem_int_wait_until(src_int,SHMEM_CMP_EQ,0);
            shmem_long_wait_until(src_long,SHMEM_CMP_EQ,0);
            shmem_longlong_wait_until(src_llong,SHMEM_CMP_EQ,0);
        }

        shmem_barrier_all();

        shfree(src_int);
        shfree(src_float);
        shfree(src_double);
        shfree(src_long);
        shfree(src_llong);
    }

    if (Verbose)
        fprintf(stderr,"[%d] exit\n",_my_pe());

    return 0;
}
