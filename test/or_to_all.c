/*
* or,xor_to_all [0...num_pes] 
*
* usage: or_ops {-v|h}
*/

#include <mpp/shmem.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

long pSync[_SHMEM_REDUCE_SYNC_SIZE];
long pSync1[_SHMEM_REDUCE_SYNC_SIZE];

#define N 7

#define Data_t long
#define SHMEM_OR shmem_long_or_to_all
#define FMT "%ld"

Data_t src[N];
Data_t dst[N];

#define Data1_t int
#define SHMEM_XOR shmem_int_xor_to_all
#define FMT1 "%d"

Data1_t src1[N];
Data1_t dst1[N];

#if (N/2) < _SHMEM_REDUCE_MIN_WRKDATA_SIZE
#define WRK_SIZE _SHMEM_REDUCE_MIN_WRKDATA_SIZE
#else
#define WRK_SIZE (N/2 + 1)	/* must be >= _SHMEM_REDUCE_MIN_WRKDATA_SIZE(8) */
#endif

Data_t pWrk[WRK_SIZE];
Data1_t pWrk1[WRK_SIZE];

int
main(int argc, char* argv[])
{
    int i, Verbose=0;
    int mpe, num_pes;
    char *pgm;
    int rc=0, rc1=0;
    Data_t expected_or_result = 0;
    Data1_t expected_xor_result = 0;

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
        src[i] = (Data_t) ((mpe+1) % 4);
        dst[i] = (Data_t)-9;
        dst1[i] = (Data1_t)-9;
        src1[i] = (Data1_t)(mpe % 2); // xor
    }
    expected_or_result = (Data_t) 3;
    expected_xor_result = (Data1_t) ((num_pes/2) % 2);

    shmem_barrier_all();

    SHMEM_OR(dst, src, N, 0, 0, num_pes, pWrk, pSync);

    SHMEM_XOR(dst1, src1, N, 0, 0, num_pes, pWrk1, pSync1);

    shmem_barrier_all();

    if (Verbose > 1) {
        printf("%d/%d	src =", mpe, num_pes );
        for (i = 0; i < N; i+= 1) {
            printf(" "FMT, src[i]);
        }
        printf("\n");
        printf("%d/%d	dst =", mpe, num_pes );
        for (i = 0; i < N; i+= 1) {
            printf(" "FMT, dst[i]);
        }
        printf("\n");
    }

    for (i = 0; i < N; i++) {
        if (dst[i] != expected_or_result) {
            fprintf(stderr,
                "[%d] or_to_all: dst[%d] "FMT" != expected "FMT"\n",
                    mpe, i, dst[i], expected_or_result);
            rc = 1;
        }
    }
    if (Verbose && mpe == 0)
        printf("Test: or_to_all, %s\n",(rc ? "Failed":"Passed"));

    if (Verbose > 1) {
        printf("%d/%d	src1 =", mpe, num_pes );
        for (i = 0; i < N; i+= 1) {
            printf(" "FMT1, src1[i]);
        }
        printf("\n");
        printf("%d/%d	dst1 =", mpe, num_pes );
        for (i = 0; i < N; i+= 1) {
            printf(" "FMT1, dst1[i]);
        }
        printf("\n");
    }

    // check XOR results
    for (i = 0; i < N; i++) {
        if (dst1[i] != expected_xor_result) {
            fprintf(stderr,
                "[%d] xor_to_all: dst1[%d] "FMT1" != expected "FMT1"\n",
                    mpe, i, dst1[i], expected_xor_result);
            rc1 = 1;
        }
    }
    if (Verbose && mpe == 0)
        printf("Test: xor_to_all, %s\n",(rc1 ? "Failed":"Passed"));

    shmem_barrier_all();

    return rc+rc1;
}
