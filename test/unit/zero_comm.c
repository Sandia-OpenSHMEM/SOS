#include <stdio.h>
#include <shmem.h>

int main(void) {
    shmem_init();

    shmem_putmem(NULL, NULL, 0, 0);
    shmem_getmem(NULL, NULL, 0, 0);

    shmem_putmem_nbi(NULL, NULL, 0, 0);
    shmem_getmem_nbi(NULL, NULL, 0, 0);

    shmem_int_iget(NULL, NULL, 1, 1, 0, 0);
    shmem_int_iput(NULL, NULL, 1, 1, 0, 0);

    shmem_finalize();
    return 0;
}
