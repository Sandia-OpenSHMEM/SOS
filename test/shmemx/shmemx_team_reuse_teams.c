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


int main(void)
{
    int i, me, npes;
    int ret = 0;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    if (me == 0)
        printf("Reuse teams test\n");

    shmemx_team_t old_team, new_team;
    ret += shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 1, npes, NULL, 0, &old_team);

    /* A total of npes-1 tests are performed, where the active set in each test
     * includes PEs i..npes-1 and each PE contributes PE ID elements */
    for (i = 1; i < npes; i++) {

        if (me == i) {
            printf("%d: creating new team (start, stride, size): %d, %d, %d\n", me,
                shmemx_team_translate_pe(old_team, 1, SHMEMX_TEAM_WORLD), 1, npes-i);
        }

        ret += shmemx_team_split_strided(old_team, 1, 1, npes-i, NULL, 0, &new_team);

        shmemx_team_destroy(old_team);

        ret += shmemx_team_split_strided(new_team, 0, 1, npes-i, NULL, 0, &old_team);

        shmemx_team_destroy(new_team);
    }

    shmem_finalize();

    return ret != 0;
}
