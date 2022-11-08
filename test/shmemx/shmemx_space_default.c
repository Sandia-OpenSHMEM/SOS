#include <shmem.h>
#include <shmemx.h>
#include <stdio.h>

int main(void) {
    int errors = 0, i;
    int *alloc_mem;
    int count = 10;
    int src_data[10];

    shmem_init();
    int me = shmem_my_pe();
    int npes = shmem_n_pes();

    alloc_mem = shmemx_space_malloc(SHMEMX_SPACE_DEFAULT, count * sizeof(int));
    if (!alloc_mem) {
        fprintf(stderr, "Space allocation on default space failed\n");
        shmem_global_exit(1);
    }

    for (i = 0; i < count; i++)
        src_data[i] = i + me;

    shmem_barrier_all();
    shmem_int_put(alloc_mem, src_data, count, (me + 1) % npes);
    shmem_barrier_all();

    for (i = 0; i < count; i++) {
        if (alloc_mem[i] != (i + (me + npes - 1) % npes))
            errors++;
    }

    shmemx_space_free(SHMEMX_SPACE_DEFAULT, alloc_mem);
    shmem_finalize();

    return errors;
}
