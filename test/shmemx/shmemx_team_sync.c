#include <stdio.h>
#include <shmem.h>
#include <shmemx.h>

int main(void)
{
   static int x = 10101;

   shmemx_team_t         twos_team, threes_team;
   shmemx_team_config_t *config;

   shmem_init();
   config   = NULL;
   int me   = shmem_my_pe();
   int npes = shmem_n_pes();

   int odd_npes = npes % 2;

   if (npes < 3) {
       shmem_global_exit(1);
   }

   shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 2, npes / 2, config, 0,
                             &twos_team);

   /* The "threes" team below overlaps with the "twos" team, so
    * synchronize on the parent team */
   shmemx_sync(SHMEMX_TEAM_WORLD);

   shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 3, npes / 3 + odd_npes,
                             config, 0, &threes_team);

   if (twos_team != SHMEMX_TEAM_NULL) {
      /* put the value 2 to the next team member in a circular fashion */
      shmem_p(&x, 2, (me + 2) % npes);
      shmem_quiet();
      shmemx_sync(twos_team);
   }

   if (threes_team != SHMEMX_TEAM_NULL) {
      /* put the value 3 to the next team member in a circular fashion */
      shmem_p(&x, 3, (me + 3) % npes);
      shmem_quiet();
      shmemx_sync(threes_team);
   }

   if (me % 3 == 0 && x != 3) {
      shmem_global_exit(3);
   }
   else if (me % 2 == 0 && x != 2) {
      shmem_global_exit(2);
   }
   else if (x != 10101)  {
      shmem_global_exit(1);
   }

   shmem_finalize();
   return 0;
}
