#include <stdio.h>
#include <stdint.h>
#include <shmem.h>

#define NELEM 16

long pSync[SHMEM_ALLTOALL_SYNC_SIZE];

static int is_active(int pe, int pe_start, int pe_stride, int pe_size) {
    int stride = 1 << pe_stride;

    return pe >= pe_start && pe < pe_start + pe_size * stride && (pe - pe_start) % stride == 0;
}

static void alltoalls_test(int32_t *out, int32_t *in, int dst, int sst, int nelem,
                    int pe_start, int pe_stride, int pe_size)
{
    int me, npes, i, j, k;

    me = shmem_my_pe();
    npes = shmem_n_pes();

    if (me == pe_start)
        printf("A2AS: dst = %2d, sst = %2d, nelem = %2d, "
               "pe_start = %2d, pe_stride = %2d, pe_size = %2d\n",
               dst, sst, nelem, pe_start, pe_stride, pe_size);

    for (i = 0; i < NELEM*npes; i++) {
        in[i] = me;
        out[i] = -1;
    }

    shmem_barrier_all();

    if (is_active(me, pe_start, pe_stride, pe_size))
        shmem_alltoalls32(out, in, dst, sst, nelem,
                          pe_start, pe_stride, pe_size, pSync);

    for (i = 0; i < npes; i++) {
        int expected;

        for (j = 0; j < nelem; j++) {
            for (k = 0; k < dst; k++) {
                int idx = i*dst*nelem + j*dst + k;
                if (is_active(me, pe_start, pe_stride, pe_size))
                    expected = (k == 0 && is_active(i, pe_start, pe_stride, pe_size)) ? i : -1;
                else
                    expected = -1;

                if (out[idx] != expected)
                    printf("[%d] out[%d] = %d, expected %d\n", me, idx, out[idx], expected);
            }
        }
    }
}


int main(int argc, char **argv) {
    int npes, i;
    int32_t *in, *out;

    shmem_init();

    npes = shmem_n_pes();

    for (i = 0; i < SHMEM_ALLTOALL_SYNC_SIZE; i++)
        pSync[i] = SHMEM_SYNC_VALUE;

    in = shmem_malloc(4 * NELEM * npes);
    out = shmem_malloc(4 * NELEM * npes);

    /* All PEs */
    alltoalls_test(out, in, 1, 1, 1, 0, 0, npes); /* Same as alltoall */
    alltoalls_test(out, in, 1, 1, 0, 0, 0, npes); /* No op */
    alltoalls_test(out, in, 2, 2, NELEM/2, 0, 0, npes); /* Alternate elements */
    alltoalls_test(out, in, 2, 1, NELEM/2, 0, 0, npes); /* dst != sst */
    alltoalls_test(out, in, 1, 2, NELEM/2 - 1, 0, 0, npes); /* dst != sst */
    /* Only PE 0, stride is invalid (should be ignored) */
    alltoalls_test(out, in, 1, 1, 1, 0, 13, 1); /* Same as alltoall */
    alltoalls_test(out, in, 1, 1, 0, 0, 13, 1); /* No op */
    alltoalls_test(out, in, 2, 2, NELEM/2, 0, 13, 1); /* Alternate elements */
    alltoalls_test(out, in, 2, 1, NELEM/2, 0, 13, 1); /* dst != sst */
    alltoalls_test(out, in, 1, 2, NELEM/2 - 1, 0, 13, 1); /* dst != sst */
    /* Only even PEs */
    alltoalls_test(out, in, 1, 1, 1, 0, 1, npes / 2 + npes % 2); /* Same as alltoall */
    alltoalls_test(out, in, 1, 1, 0, 0, 1, npes / 2 + npes % 2); /* No op */
    alltoalls_test(out, in, 2, 2, NELEM/2, 0, 1, npes / 2 + npes % 2); /* Alternate elements */
    alltoalls_test(out, in, 2, 1, NELEM/2, 0, 1, npes / 2 + npes % 2); /* dst != sst */
    alltoalls_test(out, in, 1, 2, NELEM/2 - 1, 0, 1, npes / 2 + npes % 2); /* dst != sst */

    if (npes > 1) {
        /* Remove PE n-1 */
        alltoalls_test(out, in, 1, 1, 1, 0, 0, npes-1); /* Same as alltoall */
        alltoalls_test(out, in, 1, 1, 0, 0, 0, npes-1); /* No op */
        alltoalls_test(out, in, 2, 2, NELEM/2, 0, 0, npes-1); /* Alternate elements */
        alltoalls_test(out, in, 2, 1, NELEM/2, 0, 0, npes-1); /* dst != sst */
        alltoalls_test(out, in, 1, 2, NELEM/2 - 1, 0, 0, npes-1); /* dst != sst */
        /* Remove PE 0 */
        alltoalls_test(out, in, 1, 1, 1, 1, 0, npes-1); /* Same as alltoall */
        alltoalls_test(out, in, 1, 1, 0, 1, 0, npes-1); /* No op */
        alltoalls_test(out, in, 2, 2, NELEM/2, 1, 0, npes-1); /* Alternate elements */
        alltoalls_test(out, in, 2, 1, NELEM/2, 1, 0, npes-1); /* dst != sst */
        alltoalls_test(out, in, 1, 2, NELEM/2 - 1, 1, 0, npes-1); /* dst != sst */
        /* Only odd PEs */
        alltoalls_test(out, in, 1, 1, 1, 1, 1, npes / 2); /* Same as alltoall */
        alltoalls_test(out, in, 1, 1, 0, 1, 1, npes / 2); /* No op */
        alltoalls_test(out, in, 2, 2, NELEM/2, 1, 1, npes / 2); /* Alternate elements */
        alltoalls_test(out, in, 2, 1, NELEM/2, 1, 1, npes / 2); /* dst != sst */
        alltoalls_test(out, in, 1, 2, NELEM/2 - 1, 1, 1, npes / 2); /* dst != sst */
    }

    shmem_finalize();
    return 0;
}
