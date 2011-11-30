/*
* prod+and_to_all [0...num_pes] 
*
* usage: pa_to_all {-v|-vv|h}
*/

#include <mpp/shmem.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

long pSync[_SHMEM_REDUCE_SYNC_SIZE];
long pSync1[_SHMEM_REDUCE_SYNC_SIZE];

#define Rfprintf if (mpe == 0) fprintf

#define N 3

#define Data_t long
#define SHMEM_AND shmem_long_and_to_all
#define FMT "%lx"

Data_t src[N];
Data_t dst[N];

#define Data2_t long double
#define SHMEM_PROD shmem_longdouble_prod_to_all
#define FMT2 "%Lf"

Data2_t src2[N];
Data2_t dst2[N];

#if (N/2) < _SHMEM_REDUCE_MIN_WRKDATA_SIZE
#define WRK_SIZE _SHMEM_REDUCE_MIN_WRKDATA_SIZE
#else
#define WRK_SIZE (N/2 + 1)	/* must be >= _SHMEM_REDUCE_MIN_WRKDATA_SIZE(8) */
#endif

Data_t pWrk[WRK_SIZE];
Data2_t pWrk1[WRK_SIZE];


int
main(int argc, char* argv[])
{
    int i, Verbose=0;
    int mpe, num_pes;
    char *pgm;
    int rc=0, rc1=0;
    Data2_t expected_prod;

    start_pes(0);
    mpe = _my_pe();
    num_pes = _num_pes();

    if ((pgm=strrchr(argv[0],'/')))
        pgm++;
    else
        pgm = argv[0];

	if (argc > 1) {
        if (strncmp(argv[1],"-vv",3) == 0)
            Verbose=2;
        else if (strncmp(argv[1],"-v",2) == 0)
            Verbose=1;
        else if (strncmp(argv[1],"-h",2) == 0) {
            if (mpe == 0)
                fprintf(stderr,"usage: %s {-v(verbose)|h(help)}\n",pgm);
            exit(1);
        }
    }

    for (i = 0; i < _SHMEM_REDUCE_SYNC_SIZE; i += 1) {
        pSync[i] = _SHMEM_SYNC_VALUE;
        pSync1[i] = _SHMEM_SYNC_VALUE;
    }

    for (i = 0; i < N; i++) {
        src[i] = (Data_t) mpe; // and
        dst[i] = (Data_t) -9;

        dst2[i] = (Data2_t) -9;
        src2[i] = (Data2_t) (mpe + 1); // proc
    }
        
    for(expected_prod=i=1; i <= num_pes; i++)
        expected_prod = expected_prod * i;

    shmem_barrier_all();

    SHMEM_AND(dst, src, N, 0, 0, num_pes, pWrk, pSync);

    shmem_barrier_all();
    SHMEM_PROD(dst2, src2, N, 0, 0, num_pes, pWrk1, pSync1);

    if (Verbose > 1) {
        printf("%d/%d	src =", mpe, num_pes );
        for (i = 0; i < N; i++) {
            printf(" "FMT, src[i]);
        }
        printf("\n");
        printf("%d/%d	dst =", mpe, num_pes );
        for (i = 0; i < N; i++) {
            printf(" "FMT, dst[i]);
        }
        printf("\n");
    }
    shmem_barrier_all();

    if ( mpe == 0 ) {
        for (i = 0; i < N; i++) {
            if (dst[i] != (Data_t) 0 ) {
                Rfprintf(stderr,
                    "[%d] and_to_all: dst[%d] "FMT" != expected "FMT"\n",
                        mpe, i, dst[i], (Data_t) 0 );
                rc = 1;
                break;
            }
        }
        if (Verbose && mpe == 0)
            printf("Test: and_to_all, %s\n",(rc ? "Failed":"Passed"));
    }
    shmem_barrier_all();

    if (Verbose > 1) {
        printf("%d/%d	src2 =", mpe, num_pes );
        for (i = 0; i < N; i++) {
            printf(" "FMT2, src2[i]);
        }
        printf("\n");
        printf("%d/%d	dst2 =", mpe, num_pes );
        for (i = 0; i < N; i++) {
            printf(" "FMT2, dst2[i]);
        }
        printf("\n");
    }
    shmem_barrier_all();

    // check PROD results
    if ( mpe == 0 ) {
        for (i = 1; i < N; i++) {
            if (dst2[i] != expected_prod ) {
                Rfprintf(stderr,
                    "[%d] prod_to_all: dst2[%d] "FMT2" != expected "FMT2"\n",
                        mpe, i, dst2[i], (Data2_t) expected_prod);
                rc1 = 1;
                break;
            }
        }
        if (Verbose && mpe == 0)
            printf("Test: prod_to_all, %s\n",(rc1 ? "Failed":"Passed"));
    }
    shmem_barrier_all();

    return rc+rc1;
}
