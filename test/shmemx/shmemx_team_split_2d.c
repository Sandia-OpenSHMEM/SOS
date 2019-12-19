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

static int check_2d(shmemx_team_t team, int xdim) {
    int me = shmemx_team_my_pe(team);

    shmemx_team_t xteam, yteam;

    int ret = shmemx_team_split_2d(team, xdim, NULL, 0, &xteam, NULL, 0, &yteam);
    int errors = 0;

    if (ret == 0) {
        int npes_x = shmemx_team_n_pes(xteam);
        int npes_y = shmemx_team_n_pes(yteam);

        if (xteam == SHMEMX_TEAM_INVALID || yteam == SHMEMX_TEAM_INVALID) {
            printf("%d: Error, received an invalid team\n", shmem_my_pe());
            ++errors;
        }

        /* Try converting the PE ids from xteam and yteam to global indices and
         * compare with the expected indices */
        for (int i = 0; i < npes_x; i++) {
            int pe_g       = shmemx_team_translate_pe(xteam, i, SHMEMX_TEAM_WORLD);
            int expected_g = shmemx_team_translate_pe(team, me/xdim * xdim + i, SHMEMX_TEAM_WORLD);

            if (pe_g != expected_g) {
                printf("%d: xteam pe %d expected %d, got %d\n", me, i, pe_g, expected_g);
                errors++;
            }
        }

        for (int i = 0; i < npes_y; i++) {
            int pe_g = shmemx_team_translate_pe(yteam, i, SHMEMX_TEAM_WORLD);
            int expected_g = shmemx_team_translate_pe(team, me % xdim + i * xdim, SHMEMX_TEAM_WORLD);

            if (pe_g != expected_g) {
                printf("%d: yteam pe %d expected %d, got %d\n", me, i, pe_g, expected_g);
                errors++;
            }
        }

        shmemx_team_destroy(xteam);
        shmemx_team_destroy(yteam);
    }

    return errors != 0;
}

int main(void) {
    int errors = 0, npes, ret;
    shmemx_team_t even_team;

    shmem_init();

    npes = shmem_n_pes();

    errors += check_2d(SHMEMX_TEAM_WORLD, 1);
    errors += check_2d(SHMEMX_TEAM_WORLD, 2);
    errors += check_2d(SHMEMX_TEAM_WORLD, 3);

    ret = shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 2, npes/2 + 1, NULL, 0, &even_team);

    if (ret == 0) {
        errors += check_2d(even_team, 1);
        errors += check_2d(even_team, 2);
        errors += check_2d(even_team, 3);
    }

    shmem_finalize();
    return errors != 0;
}
