#include <shmem.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>
#include <math.h>

#define NUM_POINTS 10000

long long inside = 0, total = 0;

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

    srand(1+me);

    for(total = 0; total < NUM_POINTS; ++total) {
        double x,y;
        x = rand()/(double)RAND_MAX;
        y = rand()/(double)RAND_MAX;

        if(x*x + y*y < 1) {
            ++inside;
        }
    }

    shmem_barrier_all();

    /*
    ** Reduce each PE's inside & total value into the same symmetric address
    */
    shmem_sum_reduce(SHMEM_TEAM_WORLD, &inside, &inside, 1);
    shmem_sum_reduce(SHMEM_TEAM_WORLD, &total, &total, 1);

    if(me == 0) {

        double approx_pi = 4.0*inside/(double)total;
        printf("Pi from %llu points on %d PEs: %lf\n",total,myshmem_n_pes,approx_pi);
    }

    shmem_finalize();

    return 0;
}
