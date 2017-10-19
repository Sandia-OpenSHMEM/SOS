#include <stdio.h>
#include <stdlib.h>
#include <shmem.h>

#define LEN  8192 /* Full buffer length */
#define PLEN 512  /* Length of each pipeline stage */

int in_buf[LEN], out_buf[LEN];

int main(void) {
    int i, j, *pbuf[2];
    shmem_ctx_t ctx[2];

    shmem_init();
    int me = shmem_my_pe();
    int npes = shmem_n_pes();

    pbuf[0] = shmem_malloc(PLEN * npes * sizeof(int));
    pbuf[1] = shmem_malloc(PLEN * npes * sizeof(int));

    int ret_0 = shmem_ctx_create(0, &ctx[0]);
    int ret_1 = shmem_ctx_create(0, &ctx[1]);
    if (ret_0 || ret_1) shmem_global_exit(1);

    for (i = 0; i < LEN; i++) {
        in_buf[i] = me; out_buf[i] = 0;
    }

    /* Index of ctx and pbuf (p_idx) for current pipeline stage (p) */
    int p_idx = 0, p = 0;
    for (i = 1; i <= npes; i++)
        shmem_ctx_int_put_nbi(ctx[p_idx], &pbuf[p_idx][PLEN*me],
                              &in_buf[PLEN*p], PLEN, (me+i) % npes);

    /* Issue communication for pipeline stage p, then accumulate results for
     * stage p-1 */
    for (p = 1; p < LEN/PLEN; p++) {
        p_idx ^= 1;
        for (i = 1; i <= npes; i++)
            shmem_ctx_int_put_nbi(ctx[p_idx], &pbuf[p_idx][PLEN*me],
                                  &in_buf[PLEN*p], PLEN, (me+i) % npes);

        shmem_ctx_quiet(ctx[p_idx^1]);
        shmem_sync_all();
        for (i = 0; i < npes; i++)
            for (j = 0; j < PLEN; j++)
                out_buf[PLEN*(p-1)+j] += pbuf[p_idx^1][PLEN*i+j];
    }

    shmem_ctx_quiet(ctx[p_idx]);
    shmem_sync_all();
    for (i = 0; i < npes; i++)
        for (j = 0; j < PLEN; j++)
            out_buf[PLEN*(p-1)+j] += pbuf[p_idx][PLEN*i+j];

    int errors = 0;
    int expected = (npes - 1) * npes / 2;
    for (i = 0; i < LEN; i++) {
        if (out_buf[i] != expected) {
            printf("%d: out_buf[%d] = %d, expected %d\n", me, i, out_buf[i], expected);
            errors++;
        }
    }

    shmem_finalize();
    return errors;
}
