/* circular shift bbb into aaa */

#include <shmem.h>

int aaa, bbb;

int 
main(int argc, char* argv[])
{
    int me, neighbor;
    int ret = 0;

    start_pes(0);
    bbb = me = _my_pe();
    neighbor = (me + 1) % _num_pes();

    shmem_barrier_all();

    shmem_int_get( &aaa, &bbb, 1, neighbor );

    shmem_barrier_all();

    if (aaa != neighbor ) ret = 1;

    return ret;
}
