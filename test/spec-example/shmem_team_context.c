/*
 *  This test program is derived from an example program in the
 *  OpenSHMEM specification.
 */

#include <shmem.h>
#include <shmemx.h>
#include <stdio.h>


int main(void)
{
  static int sum = 0, val2, val3;
  shmemx_team_t team_2s, team_3s;
  shmem_ctx_t ctx_2s, ctx_3s;
  shmemx_team_config_t conf;

  shmem_init();

  int npes = shmem_n_pes();
  conf.num_contexts = 1;
  long cmask = SHMEMX_TEAM_NUM_CONTEXTS;

  /* Create team with PEs numbered 0, 2, 4, ... */
  shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 2, (npes+1) / 2, &conf, cmask, &team_2s);
  /* Sync between splits from same parent team into teams with overlapping membership */
  shmemx_team_sync(SHMEMX_TEAM_WORLD);
  /* Create team with PEs numbered 0, 3, 6, ... */
  shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 3, (npes+2) / 3, &conf, cmask, &team_3s);

  /* Create a context on the 2's team. */
  if (team_2s != SHMEMX_TEAM_INVALID) {
      int ret = shmemx_team_create_ctx(team_2s, 0, &ctx_2s);
      if (ret != 0) ctx_2s = SHMEMX_CTX_INVALID;
  }

  /* Create a context on the 3's team. */
  if (team_3s != SHMEMX_TEAM_INVALID) {
      int ret = shmemx_team_create_ctx(team_3s, 0, &ctx_3s);
      if (ret != 0) ctx_3s = SHMEMX_CTX_INVALID;
  }

  /* Within each team, put my PE number to my neighbor in a ring-based manner. */
  if (ctx_2s != SHMEMX_CTX_INVALID) {
      int pe = shmemx_team_my_pe(team_2s);
      shmem_ctx_int_put(ctx_2s, &val2, &pe, 1, (pe + 1) % shmemx_team_n_pes(team_2s));
  }

  if (ctx_3s != SHMEMX_CTX_INVALID) {
      int pe = shmemx_team_my_pe(team_3s);
      shmem_ctx_int_put(ctx_3s, &val3, &pe, 1, (pe + 1) % shmemx_team_n_pes(team_3s));
  }

  /* Quiet all contexts and synchronize all PEs to complete the data transfers. */
  shmem_ctx_quiet(ctx_2s);
  shmem_ctx_quiet(ctx_3s);
  shmemx_team_sync(SHMEMX_TEAM_WORLD);

  /* Sum the values among PE's that are in both the 2's and 3's teams on PE 0. */
  if (team_3s != SHMEMX_TEAM_INVALID && team_2s != SHMEMX_TEAM_INVALID)
      shmem_int_atomic_add(&sum, val2 + val3, 0);

  /* Quiet the context and synchronize PEs to complete the operation */
  shmem_quiet();
  shmemx_team_sync(SHMEMX_TEAM_WORLD);

  /* Validate result */
  if (shmem_my_pe() == 0) {
      int vsum = 0;
      for (int i = 0; i < npes; i ++) {
          if (i % 2 == 0 && i % 3 == 0) {
              vsum += ((i - 2) < 0) ? shmemx_team_n_pes(team_2s) - 1 :
                  shmemx_team_translate_pe(SHMEMX_TEAM_WORLD, i - 2, team_2s);
              vsum += ((i - 3) < 0) ? shmemx_team_n_pes(team_3s) - 1 :
                  shmemx_team_translate_pe(SHMEMX_TEAM_WORLD, i - 3, team_3s);
          }
      }
      if (sum != vsum) {
          fprintf(stderr, "Unexpected result\n");
          shmem_global_exit(1);
      }
  }

  /* Destroy contexts before teams */
  shmem_ctx_destroy(ctx_2s);
  shmemx_team_destroy(team_2s);

  shmem_ctx_destroy(ctx_3s);
  shmemx_team_destroy(team_3s);

  shmem_finalize();
}
