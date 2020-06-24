#include <shmem.h>
#include <shmemx.h>
#include <stdlib.h>
#include <stdio.h>

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L

#define TEST_SHMEM_WAIT_UNTIL_ALL(TYPE)                                                                              \
    do {                                                                                                             \
                                                                                                                     \
        const int my_pe = shmem_my_pe();                                                                             \
        const int npes = shmem_n_pes();                                                                              \
        TYPE *ivars = shmem_calloc(npes, sizeof(TYPE));                                                              \
        int *status = calloc(npes, sizeof(int));                                                                     \
        TYPE *cmp_values = malloc(npes * sizeof(TYPE));                                                              \
                                                                                                                     \
        int i = 0;                                                                                                   \
        int expected_sum = 0;                                                                                        \
        int total_sum = 0;                                                                                           \
                                                                                                                     \
        for(i = 0; i<npes; i++){                                                                                     \
            shmem_atomic_set(&ivars[my_pe], (TYPE)my_pe, i);                                                         \
            cmp_values[i] = i;                                                                                       \
        }                                                                                                            \
                                                                                                                     \
        expected_sum = (npes-1) * npes / 2;                                                                          \
        shmem_wait_until_all_vector(ivars, npes, status, SHMEM_CMP_EQ, cmp_values);                                 \
                                                                                                                     \
        for(i = 0; i<npes; i++){                                                                                     \
            total_sum += ivars[i];                                                                                   \
        }                                                                                                            \
                                                                                                                     \
        if (expected_sum != total_sum){                                                                              \
            rc = EXIT_FAILURE;                                                                                       \
        }                                                                                                            \
   } while(0)

#define TEST_SHMEM_WAIT_UNTIL_ANY(TYPE)                                                                              \
    do{                                                                                                              \
        const int my_pe = shmem_my_pe();                                                                             \
        const int npes = shmem_n_pes();                                                                              \
        TYPE *ivars = shmem_calloc(npes, sizeof(TYPE));                                                              \
        int *status = calloc(npes, sizeof(int));                                                                     \
        TYPE *cmp_values = malloc(npes * sizeof(TYPE));                                                              \
                                                                                                                     \
        int i = 0;                                                                                                   \
        int expected_sum = 0;                                                                                        \
        int total_sum = 0;                                                                                           \
                                                                                                                     \
        for(i = 0; i<npes; i++){                                                                                     \
            shmem_atomic_set(&ivars[my_pe], (TYPE)my_pe, i);                                                         \
            cmp_values[i] = i;                                                                                       \
        }                                                                                                            \
                                                                                                                     \
        expected_sum = (npes-1) * npes / 2;                                                                          \
                                                                                                                     \
        int ncompleted = 0;                                                                                          \
        while(ncompleted < npes){                                                                                    \
            size_t ndone = shmem_wait_until_any_vector(ivars, npes, status, SHMEM_CMP_EQ, cmp_values);              \
            status[ndone] = 1;                                                                                       \
            total_sum += ivars[ndone];                                                                               \
            ncompleted++;                                                                                            \
        }                                                                                                            \
                                                                                                                     \
        if(expected_sum != total_sum){                                                                               \
            rc = EXIT_FAILURE;                                                                                       \
        }                                                                                                            \
                                                                                                                     \
    } while(0)

#define TEST_SHMEM_WAIT_UNTIL_SOME(TYPE)                                                                             \
    do{                                                                                                              \
        const int my_pe = shmem_my_pe();                                                                             \
        const int npes = shmem_n_pes();                                                                              \
        TYPE *ivars = shmem_calloc(npes, sizeof(TYPE));                                                              \
        int *status = calloc(npes, sizeof(int));                                                                     \
        TYPE *cmp_values = malloc(npes * sizeof(TYPE));                                                              \
                                                                                                                     \
        size_t *indices  = malloc(npes * sizeof(size_t));                                                            \
                                                                                                                     \
        int i = 0;                                                                                                   \
        int expected_sum = 0;                                                                                        \
        int total_sum = 0;                                                                                           \
                                                                                                                     \
        for(i = 0; i<npes; i++){                                                                                     \
            shmem_atomic_set(&ivars[my_pe], (TYPE)my_pe, i);                                                         \
            cmp_values[i] = i;                                                                                       \
        }                                                                                                            \
                                                                                                                     \
        expected_sum = (npes-1) * npes / 2;                                                                          \
                                                                                                                     \
        int ncompleted = 0;                                                                                          \
        while(ncompleted < npes){                                                                                    \
            size_t ndone = shmem_wait_until_some_vector(ivars, npes, indices, status, SHMEM_CMP_EQ, cmp_values);    \
            for(size_t j = 0; j < ndone; j++){                                                                       \
                total_sum += ivars[indices[j]];                                                                      \
                status[indices[j]] = 1;                                                                              \
            }                                                                                                        \
            ncompleted += ndone;                                                                                     \
        }                                                                                                            \
                                                                                                                     \
        if(expected_sum != total_sum){                                                                               \
            rc = EXIT_FAILURE;                                                                                       \
        }                                                                                                            \
                                                                                                                     \
    } while(0)


#else
#define TEST_SHMEM_WAIT_UNTIL_ALL(TYPE)
#define TEST_SHMEM_WAIT_UNTIL_ANY(TYPE)
#define TEST_SHMEM_WAIT_UNTIL_SOME(TYPE)

#endif


int main(void)
{

    shmem_init();

    int rc = EXIT_SUCCESS;
    TEST_SHMEM_WAIT_UNTIL_ALL(int);
    TEST_SHMEM_WAIT_UNTIL_ALL(long);
    TEST_SHMEM_WAIT_UNTIL_ALL(long long);
    TEST_SHMEM_WAIT_UNTIL_ALL(unsigned int);
    TEST_SHMEM_WAIT_UNTIL_ALL(unsigned long);
    TEST_SHMEM_WAIT_UNTIL_ALL(unsigned long long);
    TEST_SHMEM_WAIT_UNTIL_ALL(int32_t);
    TEST_SHMEM_WAIT_UNTIL_ALL(int64_t);
    TEST_SHMEM_WAIT_UNTIL_ALL(uint32_t);
    TEST_SHMEM_WAIT_UNTIL_ALL(uint64_t);
    TEST_SHMEM_WAIT_UNTIL_ALL(size_t);
    TEST_SHMEM_WAIT_UNTIL_ALL(ptrdiff_t);

    TEST_SHMEM_WAIT_UNTIL_ANY(int);
    TEST_SHMEM_WAIT_UNTIL_ANY(long);
    TEST_SHMEM_WAIT_UNTIL_ANY(long long);
    TEST_SHMEM_WAIT_UNTIL_ANY(unsigned int);
    TEST_SHMEM_WAIT_UNTIL_ANY(unsigned long);
    TEST_SHMEM_WAIT_UNTIL_ANY(unsigned long long);
    TEST_SHMEM_WAIT_UNTIL_ANY(int32_t);
    TEST_SHMEM_WAIT_UNTIL_ANY(int64_t);
    TEST_SHMEM_WAIT_UNTIL_ANY(uint32_t);
    TEST_SHMEM_WAIT_UNTIL_ANY(uint64_t);
    TEST_SHMEM_WAIT_UNTIL_ANY(size_t);
    TEST_SHMEM_WAIT_UNTIL_ANY(ptrdiff_t);

    TEST_SHMEM_WAIT_UNTIL_SOME(int);
    TEST_SHMEM_WAIT_UNTIL_SOME(long);
    TEST_SHMEM_WAIT_UNTIL_SOME(long long);
    TEST_SHMEM_WAIT_UNTIL_SOME(unsigned int);
    TEST_SHMEM_WAIT_UNTIL_SOME(unsigned long);
    TEST_SHMEM_WAIT_UNTIL_SOME(unsigned long long);
    TEST_SHMEM_WAIT_UNTIL_SOME(int32_t);
    TEST_SHMEM_WAIT_UNTIL_SOME(int64_t);
    TEST_SHMEM_WAIT_UNTIL_SOME(uint32_t);
    TEST_SHMEM_WAIT_UNTIL_SOME(uint64_t);
    TEST_SHMEM_WAIT_UNTIL_SOME(size_t);
    TEST_SHMEM_WAIT_UNTIL_SOME(ptrdiff_t);


    shmem_finalize();
    return rc;

}
