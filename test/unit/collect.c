#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <shmem.h>

#define MAX_NPES 32

int32_t src[MAX_NPES];
int32_t dst[MAX_NPES*MAX_NPES];

long pSync[SHMEM_COLLECT_SYNC_SIZE];

int main(int argc, char **argv) {
    int me, npes;
    int i, j, errors = 0;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    if (npes > MAX_NPES) {
        if (me == 0)
            printf("Warning: npes > %d, exiting without performing test\n", MAX_NPES);

        shmem_finalize();
        return 0;
    }

    for (i = 0; i < SHMEM_COLLECT_SYNC_SIZE; i++)
        pSync[i] = SHMEM_SYNC_VALUE;

    for (i = 0; i < MAX_NPES; i++)
        src[i] = -1;

    for (i = 0; i < MAX_NPES*MAX_NPES; i++)
        dst[i] = -1;

    shmem_barrier_all();

    /* TEST: All PEs contribute their PE id */
    src[0] = me;

    shmem_collect32(dst, src, 1, 0, 0, npes, pSync);

    for (i = 0; i < npes; i++) {
        if (dst[i] != i) {
            printf("%d: Test 1 error, dst[%d] == %"PRId32", expected %d\n",
                   me, i, dst[i], i);
            ++errors;
        }
    }

    shmem_barrier_all();

    /* TEST: Even PEs contribute their PE id */
    src[0] = me;

    if (me % 2 == 0) {
        shmem_collect32(dst, src, 1, 0, 1, npes/2 + npes%2, pSync);

        for (i = 0; i < npes/2; i++) {
            if (dst[i] != i*2) {
                printf("%d: Test 2 error, dst[%d] == %"PRId32", expected %d\n",
                       me, i, dst[i], i);
                ++errors;
            }
        }
    }

    shmem_barrier_all();

    /* TEST: All PEs contribute a number of elements equal to PE id */
    for (i = 0; i < me; i++)
        src[i] = me+1;

    shmem_collect32(dst, src, me, 0, 0, npes, pSync);

    int idx = 0;
    for (i = 0; i < npes; i++) {
        for (j = 0; j < i; j++) {
            if (dst[idx] != i+1) {
                printf("%d: Test 3 error, dst[%d] == %"PRId32", expected %d\n",
                       me, idx, dst[idx], i+1);
                ++errors;
            }
            ++idx;
        }
    }

    shmem_finalize();

    return errors != 0;
}
