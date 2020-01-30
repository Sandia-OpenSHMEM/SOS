#include <shmem.h>
#include <shmemx.h>
#include <stdlib.h>
#include <stdio.h>

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L

#define TEST_SHMEM_TEST_ALL(TYPE)                                                                                    \
    do {                                                                                                             \
                                                                                                                     \
        const int my_pe = shmem_my_pe();                                                                             \
        const int npes = shmem_n_pes();                                                                              \
        TYPE *ivars = shmem_calloc(npes, sizeof(TYPE));                                                              \
        int *status = calloc(npes, sizeof(int));                                                                     \
        TYPE *cmp_values = malloc(npes * sizeof(TYPE));                                                              \
                                                                                                                     \
        size_t i = 0;                                                                                                \
        int expected_sum = 0;                                                                                        \
        int total_sum = 0;                                                                                           \
                                                                                                                     \
        for(i = 0; i<npes; i++){                                                                                     \
            shmem_atomic_set(&ivars[my_pe], (TYPE)my_pe, i);                                                         \
            cmp_values[i] = i;                                                                                       \
        }                                                                                                            \
                                                                                                                     \
        expected_sum = (npes-1) * npes / 2;                                                                          \
        size_t ncompleted = 0;                                                                                       \
        while(ncompleted == 0){                                                                                      \
            ncompleted = shmemx_test_all_vector(ivars, npes, status, SHMEM_CMP_EQ, cmp_values);                      \
        }                                                                                                            \
                                                                                                                     \
        for(i = 0; i<npes; i++){                                                                                     \
            total_sum += ivars[i];                                                                                   \
        }                                                                                                            \
                                                                                                                     \
        if (expected_sum != total_sum){                                                                              \
            rc = EXIT_FAILURE;                                                                                       \
        }                                                                                                            \
   } while(0)

#define TEST_SHMEM_TEST_ANY(TYPE)                                                                                    \
    do{                                                                                                              \
        const int my_pe = shmem_my_pe();                                                                             \
        const int npes = shmem_n_pes();                                                                              \
        TYPE *ivars = shmem_calloc(npes, sizeof(TYPE));                                                              \
        int *status = calloc(npes, sizeof(int));                                                                     \
        TYPE *cmp_values = malloc(npes * sizeof(TYPE));                                                              \
                                                                                                                     \
        size_t i = 0;                                                                                                \
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
        size_t ncompleted = 0;                                                                                       \
        while(ncompleted < npes){                                                                                    \
            int ndone = shmemx_test_any_vector(ivars, npes, status, SHMEM_CMP_EQ, cmp_values);                       \
            if (ndone != SIZE_MAX){                                                                                  \
                status[ndone] = 1;                                                                                   \
                total_sum += ivars[ndone];                                                                           \
                ncompleted++;                                                                                        \
            }                                                                                                        \
        }                                                                                                            \
                                                                                                                     \
        if(expected_sum != total_sum){                                                                               \
            rc = EXIT_FAILURE;                                                                                       \
        }                                                                                                            \
                                                                                                                     \
    } while(0)                                                                                                       

#define TEST_SHMEM_TEST_SOME(TYPE)                                                                                   \
    do{                                                                                                              \
        const int my_pe = shmem_my_pe();                                                                             \
        const int npes = shmem_n_pes();                                                                              \
        TYPE *ivars = shmem_calloc(npes, sizeof(TYPE));                                                              \
        int *status = calloc(npes, sizeof(int));                                                                     \
        TYPE *cmp_values = malloc(npes * sizeof(TYPE));                                                              \
                                                                                                                     \
        size_t *indices  = malloc(npes * sizeof(size_t));                                                            \
                                                                                                                     \
        size_t i = 0;                                                                                                \
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
        size_t ncompleted = 0;                                                                                       \
        while(ncompleted < npes){                                                                                    \
            int ndone = shmemx_test_some_vector(ivars, npes, indices, status, SHMEM_CMP_EQ, cmp_values);             \
            if(ndone != 0){                                                                                          \
                for(i = 0; i < ndone; i++){                                                                          \
                    total_sum += ivars[indices[i]];                                                                  \
                    status[indices[i]] = 1;                                                                          \
                }                                                                                                    \
                ncompleted += ndone;                                                                                 \
            }                                                                                                        \
        }                                                                                                            \
                                                                                                                     \
        if(expected_sum != total_sum){                                                                               \
            rc = EXIT_FAILURE;                                                                                       \
        }                                                                                                            \
                                                                                                                     \
    } while(0)                                                                                                       


#else
#define TEST_SHMEM_TEST_ALL(TYPE)
#define TEST_SHMEM_TEST_ANY(TYPE)
#define TEST_SHMEM_TEST_SOME(TYPE)

#endif


int main(void)
{
   
    shmem_init();

    int rc = EXIT_SUCCESS;
    TEST_SHMEM_TEST_ALL(int);
    TEST_SHMEM_TEST_ALL(long);
    TEST_SHMEM_TEST_ALL(long long);
    TEST_SHMEM_TEST_ALL(unsigned int);
    TEST_SHMEM_TEST_ALL(unsigned long);
    TEST_SHMEM_TEST_ALL(unsigned long long);
    TEST_SHMEM_TEST_ALL(int32_t);
    TEST_SHMEM_TEST_ALL(int64_t);
    TEST_SHMEM_TEST_ALL(uint32_t);
    TEST_SHMEM_TEST_ALL(uint64_t);
    TEST_SHMEM_TEST_ALL(size_t);
    TEST_SHMEM_TEST_ALL(ptrdiff_t);

    TEST_SHMEM_TEST_ANY(int);
    TEST_SHMEM_TEST_ANY(long);
    TEST_SHMEM_TEST_ANY(long long);
    TEST_SHMEM_TEST_ANY(unsigned int);
    TEST_SHMEM_TEST_ANY(unsigned long);
    TEST_SHMEM_TEST_ANY(unsigned long long);
    TEST_SHMEM_TEST_ANY(int32_t);
    TEST_SHMEM_TEST_ANY(int64_t);
    TEST_SHMEM_TEST_ANY(uint32_t);
    TEST_SHMEM_TEST_ANY(uint64_t);
    TEST_SHMEM_TEST_ANY(size_t);
    TEST_SHMEM_TEST_ANY(ptrdiff_t);

    TEST_SHMEM_TEST_SOME(int);
    TEST_SHMEM_TEST_SOME(long);
    TEST_SHMEM_TEST_SOME(long long);
    TEST_SHMEM_TEST_SOME(unsigned int);
    TEST_SHMEM_TEST_SOME(unsigned long);
    TEST_SHMEM_TEST_SOME(unsigned long long);
    TEST_SHMEM_TEST_SOME(int32_t);
    TEST_SHMEM_TEST_SOME(int64_t);
    TEST_SHMEM_TEST_SOME(uint32_t);
    TEST_SHMEM_TEST_SOME(uint64_t);
    TEST_SHMEM_TEST_SOME(size_t);
    TEST_SHMEM_TEST_SOME(ptrdiff_t);


    shmem_finalize();
    return rc;    


}
