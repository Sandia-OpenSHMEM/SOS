#include <shmem.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>
#include <math.h>

#define NUM_POINTS 10000

int
main(int argc, char* argv[], char *envp[])
{
    int me, myshmem_n_pes;
    /*
    ** Starts/Initializes SHMEM/OpenSHMEM
    */
    shmem_init();
    /*
    ** Fetch the number or processes
    ** Some implementations use num_pes();
    */
    myshmem_n_pes = shmem_n_pes();
    /*
    ** Assign my process ID to me
    */
    me = shmem_my_pe();

    /*
    ** Allocate memory on the symmetric heap
    */
    long long* inside = shmem_malloc(myshmem_n_pes * sizeof(long long));
    if (inside == NULL) {
        printf("Symmetric Heap for 'inside' not allocated\n");
        exit(1);
    }
    long long* total = shmem_malloc(myshmem_n_pes * sizeof(long long));
    if (total == NULL) {
        printf("Symmetric Heap for 'total' not allocated\n");
        exit(1);
    }

    srand(1+me);

    long long localInside = 0, localTotal = 0;

    for(localTotal = 0; localTotal < NUM_POINTS; ++localTotal) {
        double x,y;
        x = rand()/(double)RAND_MAX;
        y = rand()/(double)RAND_MAX;

        if(x*x + y*y < 1) {
            ++localInside;
        }
    }

    /*
    ** Store each PE's local inside/total values in the inside array at an index
    ** matching the PE's unique id
    */
    inside[me] = localInside;
    total[me] = localTotal;

    shmem_barrier_all();

    /*
    ** Reduce each PE's inside & total value to index 0 of the inside array
    */
    shmem_sum_reduce(SHMEM_TEAM_WORLD, &inside[0], &inside[me], 1);
    shmem_sum_reduce(SHMEM_TEAM_WORLD, &total[0], &total[me], 1);

    if(me == 0) {
        double approx_pi = 4.0*inside[0]/(double)total[0];

        printf("Pi from %llu points on %d PEs: %lf\n",total[0],myshmem_n_pes,approx_pi);
    }

    /*
    ** Deallocate memory on the symmetric heap
    */
    shmem_free(inside);
    shmem_free(total);

    shmem_finalize();

    return 0;
}
