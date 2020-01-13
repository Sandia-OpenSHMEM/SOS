#include <shmem.h>
#include <shmemx.h>
#include <stdlib.h>
#include <stdio.h>

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L

#define TEST_SHMEM_WAIT_UNTIL_ALL(TYPE)                                                     \
    do {                                                                                    \
        printf("Code entered the define section\n");                                        \
        const int my_pe = shmem_my_pe();                                                    \
        const int npes = shmem_n_pes();                                                     \
        TYPE *ivars = shmem_calloc(npes, sizeof(TYPE));                                     \
        int *status = calloc(npes, sizeof(int));                                            \
        TYPE *cmp_values = malloc(npes *sizeof(TYPE));                                      \
                                                                                            \
        int i = 0;                                                                          \
        int expected_sum = 0;                                                               \
        int total_sum = 0;                                                                  \
                                                                                            \
        for(i = 0; i<npes; i++){                                                            \
            shmem_int_atomic_set(&ivars[my_pe], my_pe, i);                                  \
            cmp_values[i] = i;                                                              \
        }                                                                                   \
                                                                                            \
        shmemx_wait_until_all_vector(ivars, npes, status, SHMEM_CMP_EQ, cmp_values);        \
                                                                                            \
        for(i = 0; i<npes; i++){                                                            \
            total_sum += ivars[i];                                                          \
        }                                                                                   \
                                                                                            \
        if(my_pe == 0){                                                                     \
            printf("total sum : %d\n", total_sum);                                          \
            printf("expected sum : %d\n", expected_sum);                                    \
        }                                                                                   \
    } while(0)

#else
#define TEST_SHMEM_WAIT_UNTIL_ALL(TYPE)

#endif


int main(void)
{
   /* int total_sum = 0;
    int expected_sum = 0;
    int i;*/

    shmem_init();
    
    TEST_SHMEM_WAIT_UNTIL_ALL(int);
    /*int my_pe = shmem_my_pe();
    int npes = shmem_n_pes();

    int *ivars = shmem_calloc(npes, sizeof(int));
    int *status = calloc(npes, sizeof(int));
    int *cmp_values = malloc(npes * sizeof(int));

    for (i=0; i<npes; i++){
        shmem_int_atomic_set(&ivars[my_pe], my_pe, i);
        cmp_values[i] = i;

    }


    expected_sum = (npes)* (npes-1)/2;

    shmemx_int_wait_until_all_vector(ivars, npes, status, SHMEM_CMP_EQ, cmp_values);

    for (i = 0; i<npes; i++){
        total_sum += ivars[i];
    }

    if(my_pe == 0){
        printf("The total sum is : %d\n", total_sum);
    }*/

    shmem_finalize();
    return 0;    


}
