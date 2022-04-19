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

int main(void) {
    int i, n, errors = 0;
    int me, team_me, npes;

    static int shr_world_data, shr_team_data = -1;
    int * shr_heap;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();


    shr_world_data = me;

    shmem_barrier_all();


    /* Check shmem_team_ptr on data segment */
    for (i = n = 0; i < npes; i++) {
        int * world_ptr = (int *) shmem_team_ptr(SHMEM_TEAM_WORLD, &shr_world_data, i);

        if (world_ptr != NULL) {

            int shr_world_peer = *world_ptr;
            ++n;

            if (shr_world_peer != i) {
                printf("%2d: Error, shr_world_data(%d) = %d, expected %d\n", me, i, shr_world_peer, i);
                errors++;
            }
        }
        else if (i == me) {
            printf("%2d: Error, shmem_world_ptr(data) returned NULL for my PE\n", me);
            errors++;
        }
    }

    printf("%2d: Found %d world team data segment peer(s)\n", me, n);
    fflush(NULL);
    shmem_barrier_all();

    /* Check shmem_team_ptr on heap segment */
    shr_heap = shmem_malloc(sizeof(int));
    *shr_heap = me;

    for (i = n = 0; i < npes; i++) {
        int * world_ptr = (int *) shmem_team_ptr(SHMEM_TEAM_WORLD, shr_heap, i);

        if (world_ptr != NULL) {
            int shr_world_peer = *world_ptr;
            ++n;

            if (shr_world_peer != i) {
                printf("%2d: Error, shr_world_heap(%d) = %d, expected %d\n", me, i, shr_world_peer, i);
                errors++;
            }
        }
        else if (i == me) {
            printf("%2d: Error, shmem_team_ptr(heap) returned NULL for my PE\n", me);
            errors++;
        }
    }

    printf("%2d: Found %d world team heap segment peer(s)\n", me, n);
    fflush(NULL);
    shmem_barrier_all();

    if (npes == 1) {
        shmem_finalize();
        return errors != 0;
    }

    // Create a team of PEs w/ odd numbered id's
    shmem_team_t new_team;
    shmem_team_split_strided(SHMEM_TEAM_WORLD, 1, 2, npes / 2, NULL, 0, &new_team);
      if (new_team != SHMEM_TEAM_INVALID) {
          team_me = shmem_team_my_pe(new_team);
          shr_team_data = team_me;

          /* Check shmem_team_ptr on data segment */
          for (i = n = 0; i < npes / 2; i++) {
              int * team_ptr = (int *) shmem_team_ptr(new_team, &shr_team_data, i);

              if (team_ptr != NULL) {
                  int shr_team_peer = *team_ptr;
                  ++n;

                  if (shr_team_peer != i) {
                      printf("%2d: Error, shr_team_data(%d) = %d, expected %d\n", team_me, i, shr_team_peer, i);
                      errors++;
                  }
              }
            else if (i == team_me) {
                printf("%2d: Error, i = %d, shmem_team_ptr(data) returned NULL for my PE\n", team_me, i);
                errors++;
            }
        }
        printf("%2d: Found %d new team data segment peer(s)\n", team_me, n);
        fflush(NULL);
    }

    shmem_barrier_all();

    /* Check shmem_team_ptr on heap segment */
    int * shr_team_heap = shmem_malloc(sizeof(int));
    if (new_team != SHMEM_TEAM_INVALID) {
      *shr_team_heap = team_me;

      for (i = n = 0; i < npes / 2; i++) {
          int * team_ptr = (int *) shmem_team_ptr(new_team, shr_team_heap, i);

          if (team_ptr != NULL) {
              int shr_team_peer = *team_ptr;
              ++n;

              if (shr_team_peer != i) {
                  printf("%2d: Error, shr_team_heap(%d) = %d, expected %d\n", me, i, shr_team_peer, i);
                  errors++;
              }
          }
          else if (i == team_me) {
              printf("%2d: Error, shmem_team_ptr(heap) returned NULL for my PE\n", team_me);
              errors++;
          }
      }
      printf("%2d: Found %d new team heap segment peer(s)\n", team_me, n);
    }

    shmem_finalize();

    return errors != 0;
}
