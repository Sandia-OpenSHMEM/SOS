/* circular shift bbb into aaa */
#include <mpp/shmem.h>

int aaa, bbb;

int 
main(int argc, char* argv[])
{
    int me;
    int ret = 0;

    start_pes(0);
    me = _my_pe();
    bbb = me;

    shmem_barrier_all();

    shmem_int_get(&aaa, &bbb, 1,(_my_pe() + 1)% _num_pes());

    if (aaa != (_my_pe() + 1) % _num_pes()) ret = 1;

    shmem_barrier_all();

    return ret;
}
