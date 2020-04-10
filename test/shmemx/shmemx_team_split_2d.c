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
#include <shmem.h>
#include <shmemx.h>

static int check_2d(shmemx_team_t parent_team, int xdim) {
    int me = shmemx_team_my_pe(parent_team);

    shmemx_team_t xteam = SHMEMX_TEAM_INVALID;
    shmemx_team_t yteam = SHMEMX_TEAM_INVALID;

    int ret = shmemx_team_split_2d(parent_team, xdim, NULL, 0, &xteam, NULL, 0, &yteam);
    int errors = 0;

    if (ret == 0) {
        int me_x = shmemx_team_my_pe(xteam);
        int me_y = shmemx_team_my_pe(yteam);
        int npes_x = shmemx_team_n_pes(xteam);
        int npes_y = shmemx_team_n_pes(yteam);

        if (xteam == SHMEMX_TEAM_INVALID || yteam == SHMEMX_TEAM_INVALID) {
            printf("%d: Error, received an invalid team\n", shmem_my_pe());
            ++errors;
        }

        /* Try converting the PE ids from xteam and yteam to parent and global
         * PE indices and compare with the expected indices */
        for (int i = 0; i < npes_x; i++) {
            int expected_parent = me_y * xdim + i; /* row (fixed) + column */
            int pe_parent       = shmemx_team_translate_pe(xteam, i, parent_team);
            int pe_world        = shmemx_team_translate_pe(xteam, i, SHMEMX_TEAM_WORLD);
            int expected_world  = shmemx_team_translate_pe(parent_team, expected_parent, SHMEMX_TEAM_WORLD);

            if (expected_parent != pe_parent) {
                printf("%d: xteam[%d] expected parent PE id %d, got %d\n",
                        me, i, expected_parent, pe_parent);
                errors++;
            }

            if (expected_world != pe_world) {
                printf("%d: xteam[%d] expected world PE id %d, got %d\n",
                        me, i, expected_world, pe_world);
                errors++;
            }
        }

        for (int i = 0; i < npes_y; i++) {
            int expected_parent = i * xdim + me_x; /* row + column (fixed) */
            int pe_parent       = shmemx_team_translate_pe(yteam, i, parent_team);
            int pe_world        = shmemx_team_translate_pe(yteam, i, SHMEMX_TEAM_WORLD);
            int expected_world  = shmemx_team_translate_pe(parent_team, expected_parent, SHMEMX_TEAM_WORLD);

            if (expected_parent != pe_parent) {
                printf("%d: yteam[%d] expected parent PE id %d, got %d\n",
                        me, i, expected_parent, pe_parent);
                errors++;
            }

            if (expected_world != pe_world) {
                printf("%d: yteam[%d] expected world PE id %d, got %d\n",
                        me, i, expected_world, pe_world);
                errors++;
            }
        }
    }
    else {
        printf("%d: 2d split failed\n", shmem_my_pe());
    }

    if (xteam != SHMEMX_TEAM_INVALID)
        shmemx_team_destroy(xteam);
    if (yteam != SHMEMX_TEAM_INVALID)
        shmemx_team_destroy(yteam);

    return errors != 0;
}

int main(void) {
    int errors = 0, me, npes, ret;
    shmemx_team_t even_team;

    shmem_init();

    me   = shmem_my_pe();
    npes = shmem_n_pes();

    if (me == 0) printf("Performing 2d split test on SHMEM_TEAM_WORLD\n");

    errors += check_2d(SHMEMX_TEAM_WORLD, 1);
    errors += check_2d(SHMEMX_TEAM_WORLD, 2);
    errors += check_2d(SHMEMX_TEAM_WORLD, 3);

    ret = shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 2, (npes-1)/2 + 1,
                                    NULL, 0, &even_team);

    if (ret == 0) {
        if (me == 0) printf("Performing 2d split test on even team\n");

        errors += check_2d(even_team, 1);
        errors += check_2d(even_team, 2);
        errors += check_2d(even_team, 3);
    } else {
        if (me == 0) printf("Unable to create even team\n");
    }

    shmem_finalize();
    return errors != 0;
}
