#include <shmem.h>
#include <shmemx.h>
#include <stdio.h>

#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

int isum, ival2, ival3;

int my_ctx_translate_pe(shmem_ctx_t, int, shmem_ctx_t);
shmem_ctx_t my_team_create_ctx(shmemx_team_t);
void my_send_to_neighbor(shmem_ctx_t, int *);

int my_ctx_translate_pe(shmem_ctx_t src_ctx, int src_pe, shmem_ctx_t dest_ctx)
{
  if (src_ctx == SHMEMX_CTX_INVALID) {
    return -1;
  }
  if (dest_ctx == SHMEMX_CTX_INVALID) {
    return -1;
  }
  
  shmemx_team_t src_team, dest_team;
  shmemx_ctx_get_team(src_ctx, &src_team);
  shmemx_ctx_get_team(dest_ctx, &dest_team);
  return shmemx_team_translate_pe(src_team, src_pe, dest_team);
}

shmem_ctx_t my_team_create_ctx(shmemx_team_t team) {
  if (team == SHMEMX_TEAM_INVALID) {
    return SHMEMX_CTX_INVALID;
  }

  shmem_ctx_t ctx;
  if (shmemx_team_create_ctx(team, 0, &ctx) != 0) {
    fprintf (stderr, "Failed to create context for PE team\n");
    return SHMEMX_CTX_INVALID;
  }
  return ctx;
}

void my_send_to_neighbor(shmem_ctx_t ctx, int *val)
{
  if (ctx == SHMEMX_CTX_INVALID) {
    fprintf (stderr, "Send to neighbor fail due to invalid context\n");
    return;
  }
  
  shmemx_team_t team;
  shmemx_ctx_get_team(ctx, &team);
  int pe = shmemx_team_my_pe(team);
  int npes = shmemx_team_n_pes(team);
  int rpe = (pe + 1) % npes;

  // put my pe number in the buffer on my right hand neighbor
  shmem_ctx_int_put(ctx, val, &pe, 1, rpe);
}



int main(int argc, char** argv)
{
  shmem_init();

  int npes = shmem_n_pes();
  isum = 0;

  shmemx_team_t team_2s, team_3s;
  shmem_ctx_t ctx_2s, ctx_3s;
  shmemx_team_config_t conf;
  conf.num_contexts = 1;
  long cmask = SHMEMX_TEAM_NUM_CONTEXTS;

  if (npes < 4) {
      fprintf(stderr, "Not enough PEs, please run at least 4\n");
      shmem_global_exit(0);
  }

  // Create team with PEs numbered 0, 2, 4, ...
  shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 2, npes / 2, &conf, cmask, &team_2s);
  // Sync between splits from same parent team into teams with overlapping membership
  shmemx_team_sync(SHMEMX_TEAM_WORLD);
  // Create team with PEs numbered 0, 3, 6, ...
  shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 3, npes / 3, &conf, cmask, &team_3s);

  ctx_2s = my_team_create_ctx(team_2s);
  ctx_3s = my_team_create_ctx(team_3s);

  // Send some values using the two team contexts contexts
  ival2 = 2;
  ival3 = 3;
  my_send_to_neighbor(ctx_2s, &ival2);
  my_send_to_neighbor(ctx_3s, &ival3);

  // Quiet all contexts and synchronize all PEs to complete the data transfers
  shmem_ctx_quiet(ctx_2s);
  shmem_ctx_quiet(ctx_3s);
  shmemx_team_sync(SHMEMX_TEAM_WORLD);

  // We will add up some results on pe 4 of team_3s using ctx_2s
  if ((team_3s != SHMEMX_TEAM_INVALID) && (team_2s != SHMEMX_TEAM_INVALID)) {
    int _pe4_of_3s_in_2s = my_ctx_translate_pe(ctx_3s, 4, ctx_2s);

    if (_pe4_of_3s_in_2s < 0) {
      fprintf (stderr, "Fail to translate pe 4 from 3s context to 2s context\n");
    }
    else {
      // Add up the results on pe 4 of the 3s team, using the 2s team context
      shmem_ctx_int_atomic_add(ctx_2s, &isum, ival2 + ival3, _pe4_of_3s_in_2s);
    }
  }

  // Quiet the context and synchronize PEs to complete the operation
  shmem_ctx_quiet(ctx_2s);
  shmemx_team_sync(SHMEMX_TEAM_WORLD);

  if (shmemx_team_my_pe(team_3s) == 4) {
    printf ("The total value on PE 4 of the 3s team is %d\n", isum);
  }

  // Destroy contexts before teams
  shmem_ctx_destroy(ctx_2s);
  shmemx_team_destroy(&team_2s);

  shmem_ctx_destroy(ctx_3s);
  shmemx_team_destroy(&team_3s);

  shmem_finalize();
}
