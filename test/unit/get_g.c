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

    if ((pgm=strrchr(argv[0],'/')))
        pgm++;
    else
        pgm = argv[0];

	if (argc > 1) {
        if (strncmp(argv[1],"-v",3) == 0)
            Verbose=1;
        else if (strncmp(argv[1],"-h",3) == 0) {
            fprintf(stderr,"usage: %s {-v(verbose)|h(help)}\n",pgm);
            exit(1);
        }
    }

    start_pes(0);
    me = _my_pe();
    num_pes = _num_pes();

    // be a bit sane with total number of gets issued
    loops = loops / num_pes;
    if (loops < 5) loops = 5;

    for (l = 0 ; l < loops ; ++l) {
        if ((src_short = shmalloc(sizeof(short))) == NULL) {
            printf("PE-%d short shmalloc() failed?\n", me);
            exit(1);
        }
        *src_short = 2;

        if ((src_int = shmalloc(sizeof(int))) == NULL) {
            printf("PE-%d int shmalloc() failed?\n", me);
            exit(1);
        }
        *src_int = 4;

        if ((src_float = shmalloc(sizeof(float))) == NULL) {
            printf("PE-%d float shmalloc() failed?\n", me);
            exit(1);
        }
        *src_float = 4.0;

        if ((src_double = shmalloc(sizeof(double))) == NULL) {
            printf("PE-%d double shmalloc() failed?\n", me);
            exit(1);
        }
        *src_double = 8.0;

        if ((src_long = shmalloc(sizeof(long))) == NULL) {
            printf("PE-%d long shmalloc() failed?\n", me);
            exit(1);
        }
        *src_long = 8;

    	shmem_barrier_all();

    	for (pe=0 ; pe < num_pes; ++pe) {
            if (!shmem_addr_accessible(src_short,pe)) {
                printf("PE-%d local addr %p not accessible from PE-%d?\n",
                       me, (void*)src_short, pe);
                exit(1);
            }
            dst_short = 0;
            dst_short = shmem_short_g(src_short,pe);
            if (dst_short != 2) {
                printf("PE-%d dst_short %hd != 2?\n",me,dst_short);
                exit(1);
            }
            dst_int = 0;
            dst_int = shmem_int_g(src_int,pe);
            if (dst_int != 4) {
                printf("PE-%d dst_int %d != 4?\n",me,dst_int);
                exit(1);
            }
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
    	}

    	shmem_barrier_all();

        shfree(src_short);
        shfree(src_int);
        shfree(src_float);
        shfree(src_double);
        shfree(src_long);
    }

    if (Verbose)
		fprintf(stderr,"[%d] exit\n",_my_pe());

    return 0;
}
