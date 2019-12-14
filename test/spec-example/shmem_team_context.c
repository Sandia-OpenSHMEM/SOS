/*
 *  This test program is derived from an example program in the
 *  OpenSHMEM specification.
 */

#include <shmem.h>
#include <shmemx.h>
#include <stdio.h>


int main(void)
{
  static int sum = 0, val_2, val_3;
  shmemx_team_t team_2, team_3;
  shmem_ctx_t ctx_2, ctx_3;
  shmemx_team_config_t conf;

  shmem_init();

  int npes  = shmem_n_pes();
  int my_pe = shmem_my_pe();
  conf.num_contexts = 1;
  long cmask = SHMEMX_TEAM_NUM_CONTEXTS;

  /* Create team_2 with PEs numbered 0, 2, 4, ... */
  shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 2, (npes + 1) / 2, &conf, cmask, &team_2);

  /* Create team_3 with PEs numbered 0, 3, 6, ... */
  shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 3, (npes + 2) / 3, &conf, cmask, &team_3);

  /* Create a context on team_2. */
  if (team_2 != SHMEMX_TEAM_INVALID) {
      int ret = shmemx_team_create_ctx(team_2, 0, &ctx_2);
      if (ret != 0) ctx_2 = SHMEMX_CTX_INVALID;
  }

  /* Create a context on team_3. */
  if (team_3 != SHMEMX_TEAM_INVALID) {
      int ret = shmemx_team_create_ctx(team_3, 0, &ctx_3);
      if (ret != 0) ctx_3 = SHMEMX_CTX_INVALID;
  }

  /* Within each team, put my PE number to my neighbor in a ring-based manner. */
  if (ctx_2 != SHMEMX_CTX_INVALID) {
      int pe = shmemx_team_my_pe(team_2);
      shmem_ctx_int_put(ctx_2, &val_2, &pe, 1, (pe + 1) % shmemx_team_n_pes(team_2));
  }

  if (ctx_3 != SHMEMX_CTX_INVALID) {
      int pe = shmemx_team_my_pe(team_3);
      shmem_ctx_int_put(ctx_3, &val_3, &pe, 1, (pe + 1) % shmemx_team_n_pes(team_3));
  }

  /* Quiet both contexts and synchronize all PEs to complete the data transfers. */
  shmem_ctx_quiet(ctx_2);
  shmem_ctx_quiet(ctx_3);
  shmemx_team_sync(SHMEMX_TEAM_WORLD);

  /* Sum the values among PEs that are in both team_2 and team_3 on PE 0 with ctx_2. */
  if (team_3 != SHMEMX_TEAM_INVALID && team_2 != SHMEMX_TEAM_INVALID)
      shmem_ctx_int_atomic_add(ctx_2, &sum, val_2 + val_3, 0);

  /* Quiet the context and synchronize PEs to complete the operation. */
  shmem_ctx_quiet(ctx_2);
  shmemx_team_sync(SHMEMX_TEAM_WORLD);

  /* Validate the result. */
  if (shmem_my_pe() == 0) {
      int vsum = 0;
      for (int i = 0; i < npes; i ++) {
          if (i % 2 == 0 && i % 3 == 0) {
              vsum += ((i - 2) < 0) ? shmemx_team_n_pes(team_2) - 1 :
                  shmemx_team_translate_pe(SHMEMX_TEAM_WORLD, i - 2, team_2);
              vsum += ((i - 3) < 0) ? shmemx_team_n_pes(team_3) - 1 :
                  shmemx_team_translate_pe(SHMEMX_TEAM_WORLD, i - 3, team_3);
          }
      }
      if (sum != vsum) {
          fprintf(stderr, "Unexpected result, npes = %d, vsum = %d, sum = %d\n", shmem_n_pes(), vsum, sum);
          shmem_global_exit(1);
      }
  }

  /* Destroy contexts before teams. */
  shmem_ctx_destroy(ctx_2);
  shmemx_team_destroy(team_2);

  shmem_ctx_destroy(ctx_3);
  shmemx_team_destroy(team_3);

  shmem_finalize();
}
