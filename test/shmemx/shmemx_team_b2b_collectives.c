#include <stdio.h>
#include <stdlib.h>
#include <shmem.h>
#include <shmemx.h>

#define NITERS 100
#define NELEMS 10

#define TEST_B2B_COLLECTIVE(NAME, ROUTINE, ...)                                 \
    do {                                                                        \
        if (me == 0) printf("%s... ", NAME);                                    \
        int i;                                                                  \
        for (i = 0; i < NITERS; i++) {                                          \
            errors += ROUTINE(__VA_ARGS__);                                     \
        }                                                                       \
        error_check(&errors, &total_errors, NAME, me);                          \
    } while (0)

void error_check(int *, int *, char *, int);

void error_check(int *errors, int *total_errors, char *routine, int me) {
    if (*errors == 0) {
        if (me == 0) printf("passed.\n");
    } else {
        printf("%s error on PE %d\n", routine, me);
        *total_errors += *errors;
        *errors = 0;
    }
    return;
}


int main(void)
{
    int errors = 0, total_errors = 0;
    shmem_init();
    int me = shmem_my_pe();

    long *dest = shmem_malloc(NELEMS * sizeof(long));
    long *src = shmem_malloc(NELEMS * sizeof(long));

    size_t i;
    for (i = 0; i < NELEMS; i++) {
        src[i] = me;
    }

    TEST_B2B_COLLECTIVE("broadcast", shmemx_long_broadcast, SHMEMX_TEAM_WORLD, dest, src, NELEMS, 0);
    TEST_B2B_COLLECTIVE("reduce", shmemx_long_sum_reduce, SHMEMX_TEAM_WORLD, dest, src, NELEMS);
    TEST_B2B_COLLECTIVE("collect", shmemx_long_collect, SHMEMX_TEAM_WORLD, dest, src, NELEMS);
    TEST_B2B_COLLECTIVE("fcollect", shmemx_long_fcollect, SHMEMX_TEAM_WORLD, dest, src, NELEMS);
    TEST_B2B_COLLECTIVE("alltoall", shmemx_long_alltoall, SHMEMX_TEAM_WORLD, dest, src, NELEMS);
    TEST_B2B_COLLECTIVE("alltoalls", shmemx_long_alltoalls, SHMEMX_TEAM_WORLD, dest, src, 1, 1, NELEMS);

    shmem_finalize();
    return total_errors;
}
