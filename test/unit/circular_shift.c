/* circular shift bbb into aaa */

#include <shmem.h>

int aaa, bbb;

int 
main(int argc, char* argv[])
{
    int me, neighbor;
    int ret = 0;

    shmem_init();
    bbb = me = shmem_my_pe();
    neighbor = (me + 1) % shmem_n_pes();

    shmem_barrier_all();

    shmem_int_get( &aaa, &bbb, 1, neighbor );

    shmem_barrier_all();

    if (aaa != neighbor ) ret = 1;

    shmem_finalize();

    return ret;
}
