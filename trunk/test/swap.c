/*
 * swap - exercise shmem_*_swap()
 */

#include <mpp/shmem.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

static int *src_int;
static float *src_float;
static double *src_double;
static long *src_long;
static long long *src_llong;

static int dst_int;
static float dst_float;
static double dst_double;
static long dst_long;
static long long dst_llong;

static int loops = 1;

int
main(int argc, char* argv[])
{
    int me, num_pes, l;
    int Verbose = 1;

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

    	if ( me == 0 ) {
			dst_int = shmem_int_swap(src_int,0,1);
			if (dst_int != 4) {
				printf("PE-%d dst_int %d != 4?\n",me,dst_int);
				exit(1);
			}

			dst_int = shmem_int_swap(src_int,dst_int,1);
			if (dst_int != 0) {
				printf("PE-%d dst_int %d != 0?\n",me,dst_int);
				exit(1);
			}
#if 0
			dst_float = 0.0;
			dst_float = shmem_float_g(src_float,pe);
			if (dst_float != 4.0) {
				printf("PE-%d dst_float %f != 4.0?\n",me,dst_float);
				exit(1);
			}
			dst_double = 0.0;
			dst_double = shmem_double_g(src_double,pe);
			if (dst_double != 8.0) {
				printf("PE-%d dst_double %f != 8.0?\n",me,dst_double);
				exit(1);
			}
			dst_long = 0;
			dst_long = shmem_long_g(src_long,pe);
			if (dst_long != 8.0) {
				printf("PE-%d dst_long %ld != 8?\n",me,dst_long);
				exit(1);
			}
#endif
    	}
		else {
#if 0
			shmem_int_wait_until(src_short,SHMEM_CMP_EQ,0);
#else
			/* is 'src_int' accessible from PE0? should be. */
			if (!shmem_addr_accessible(src_int,0)) {
				printf("PE-%d local src_int %p not accessible from PE-%d?\n",
						me, (void*)src_int, 0);
				exit(1);
			}
#endif
		}

		printf("PE-%d pre-barrier\n",me);
    	shmem_barrier_all();
		printf("PE-%d post-barrier\n",me);

		shfree(src_int);
		shfree(src_float);
		shfree(src_double);
		shfree(src_long);
		shfree(src_llong);

		printf("PE-%d free done\n",me);
    }

    if (Verbose)
		fprintf(stderr,"[%d] exit\n",_my_pe());

    return 0;
}
