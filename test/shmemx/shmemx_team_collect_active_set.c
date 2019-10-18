/*
 *  Copyright (c) 2019 Intel Corporation. All rights reserved.
 *  This software is available to you under the BSD license below:
 *
 *      Redistribution and use in source and binary forms, with or
 *      without modification, are permitted provided that the following
 *      conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <shmem.h>
#include <shmemx.h>

#define MAX_NPES 32

int64_t src[MAX_NPES];
int64_t dst[MAX_NPES*MAX_NPES];

int main(void)
{
    int i, me, npes;
    int errors = 0;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    if (npes > MAX_NPES) {
        if (me == 0)
            printf("Test requires fewer than %d PEs\n", MAX_NPES);
        shmem_finalize();
        return 0;
    }

    for (i = 0; i < MAX_NPES; i++)
        src[i] = me;

    for (i = 0; i < MAX_NPES*MAX_NPES; i++)
        dst[i] = -1;

    if (me == 0)
        printf("Shrinking team size test\n");

    shmemx_team_t old_team, new_team;
    shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 1, npes, NULL, 0, &old_team);

    /* A total of npes-1 tests are performed, where the active set in each test
     * includes PEs i..npes-1 and each PE contributes PE ID elements */
    for (i = 1; i < npes; i++) {
        int j, k;
        int idx = 0;

        if (me == i)
            printf(" + team size %d\n", npes-i);

        shmemx_team_split_strided(old_team, 1, 1, npes-i, NULL, 0, &new_team);

        if (new_team != SHMEMX_TEAM_INVALID) {

            shmemx_collect(new_team, dst, src, me);

            /* Validate destination buffer data */
            for (j = 0; j < npes - i; j++) {
                for (k = 0; k < i+j; k++, idx++) {
                    if (dst[idx] != i+j) {
                        printf("%d: Expected dst[%d] = %d, got dst[%d] = %"PRId64", iteration %d\n",
                               me, idx, i+j, idx, dst[idx], i);
                        errors++;
                    }
                }
            }

            /* Validate unused destination buffer */
            for ( ; idx < MAX_NPES*MAX_NPES; idx++) {
                if (dst[idx] != -1) {
                    printf("%d: Expected dst[%d] = %d, got dst[%d] = %"PRId64", iteration %d\n",
                           me, idx, -1, idx, dst[idx], i);
                    errors++;
                }
            }

            /* Reset for next iteration */
            for (j = 0; j < MAX_NPES*MAX_NPES; j++)
                dst[j] = -1;

            shmemx_sync(new_team);
        }

        shmemx_team_destroy(old_team);

        shmemx_team_split_strided(new_team, 0, 1, npes-i, NULL, 0, &old_team);

        shmemx_team_destroy(new_team);
    }

    shmem_finalize();

    return errors != 0;
}
