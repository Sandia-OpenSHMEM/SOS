/*
 *  Copyright (c) 2022 Intel Corporation. All rights reserved.
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

#include <shmem.h>
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    int i, me, npes;
    int errors = 0;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    if (npes < 2) {
        if (me == 0)
            printf("Test requires 2 or more PEs\n");
        shmem_finalize();
        return 0;
    }

    int *src = shmem_malloc(npes * sizeof(int));
    int *dst = shmem_malloc(npes * sizeof(int));

    for (i = 0; i < npes; i++)
        src[i] = i;

    for (i = 0; i < npes; i++)
        dst[i] = -1;

    if (me == 0)
        printf("-1 stride:\n");

    shmem_team_t new_team;
    shmem_team_split_strided(SHMEM_TEAM_WORLD, npes - 1, -1, npes, NULL, 0, &new_team);

    if (new_team != SHMEM_TEAM_INVALID) {
        int new_team_id = shmem_team_translate_pe(SHMEM_TEAM_WORLD, me, new_team);

        if (new_team_id != -1 && new_team_id == abs(me - npes + 1)) {
            printf("world team ID = %d, new team ID = %d\n", me, new_team_id);
        } else {
            errors++;
        }
    }

    shmem_int_sum_reduce(new_team, dst, src, npes);

    for (i = 0; i < npes; i++) {
        if (dst[i] != i * npes) {
            printf("ERROR: dst[i] = %d, expected %d\n", dst[i], i * npes);
            errors++;
        }
    }

    shmem_team_destroy(new_team);

    for (i = 0; i < npes; i++)
        dst[i] = -1;

    if (me == 0)
        printf("-2 stride:\n");

    shmem_team_split_strided(SHMEM_TEAM_WORLD, npes - 1, -2, npes/2, NULL, 0, &new_team);

    if (new_team != SHMEM_TEAM_INVALID) {
        int new_team_id = shmem_team_translate_pe(SHMEM_TEAM_WORLD, me, new_team);

        if (new_team_id != -1 && new_team_id == abs((npes - me - 1)/2)) {
            printf("world team ID = %d, new team ID = %d\n", me, new_team_id);
        } else {
            errors++;
        }
    }

    if (new_team != SHMEM_TEAM_INVALID) {
        shmem_int_sum_reduce(new_team, dst, src, npes);

        for (i = 0; i < npes; i++) {
            if (dst[i] != i * (npes/2)) {
                printf("ERROR: dst[i] = %d, expected %d\n", dst[i], i * (npes/2));
                errors++;
            }
        }
    }

    shmem_team_destroy(new_team);
    shmem_free(src);
    shmem_free(dst);
    shmem_finalize();

    return errors != 0;
}
