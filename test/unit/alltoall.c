#include <stdio.h>
#include <stdint.h>
#include <shmem.h>

long pSync[SHMEM_ALLTOALL_SYNC_SIZE];

int is_active(int pe, int pe_start, int pe_stride, int pe_size) {
    int stride = 1 << pe_stride;

    return pe >= pe_start && pe < pe_start + pe_size * stride && (pe - pe_start) % stride == 0;
}

void alltoall_test(int32_t *out, int32_t *in, int pe_start, int pe_stride,
                   int pe_size)
{
    int me, npes, i;

    me = shmem_my_pe();
    npes = shmem_n_pes();

    if (me == pe_start)
        printf("A2A: pe_start = %d, pe_stride = %d, pe_size = %d\n", pe_start,
               pe_stride, pe_size);

    for (i = 0; i < npes; i++) {
        in[i] = me;
        out[i] = -1;
    }

    shmem_barrier_all();

    if (is_active(me, pe_start, pe_stride, pe_size))
        shmem_alltoall32(out, in, 1, pe_start, pe_stride, pe_size, pSync);

    for (i = 0; i < npes; i++) {
        int expected;

        if (is_active(me, pe_start, pe_stride, pe_size))
            expected = is_active(i, pe_start, pe_stride, pe_size) ? i : -1;
        else
            expected = -1;

        if (out[i] != expected)
            printf("[%d] out[%d] = %d, expected %d\n", me, i, out[i], expected);
    }
}


int main(int argc, char **argv) {
    int me, npes, i;
    int32_t *in, *out;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    for (i = 0; i < SHMEM_ALLTOALL_SYNC_SIZE; i++)
        pSync[i] = SHMEM_SYNC_VALUE;

    in = shmem_malloc(4 * npes);
    out = shmem_malloc(4 * npes);

    /* All PEs */
    alltoall_test(out, in, 0, 0, npes);
    /* Only PE 0, stride is invalid (should be ignored) */
    alltoall_test(out, in, 0, 13, 1);
    /* Only even PEs */
    alltoall_test(out, in, 0, 1, npes / 2 + npes % 2);

    if (npes > 1) {
        /* Remove PE n-1 */
        alltoall_test(out, in, 0, 0, npes-1);
        /* Remove PE 0 */
        alltoall_test(out, in, 1, 0, npes-1);
        /* Only odd PEs */
        alltoall_test(out, in, 1, 1, npes / 2);
    }

    shmem_finalize();
    return 0;
}
