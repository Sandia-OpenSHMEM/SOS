/*
 *  This test program is derived from an example program in the
 *  OpenSHMEM specification.
 */

#include <stdio.h>
#include <shmem.h>

#if !(defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L)
#define shmem_p shmem_int_p
#endif

int main(void)
{
   static int x = 10101;

   shmem_team_t         twos_team   = SHMEM_TEAM_INVALID;
   shmem_team_t         threes_team = SHMEM_TEAM_INVALID;
   shmem_team_config_t *config;

   shmem_init();
   config   = NULL;
   int me   = shmem_my_pe();
   int npes = shmem_n_pes();

   if (npes > 2)
       shmem_team_split_strided(SHMEM_TEAM_WORLD, 2, 2, (npes-1) / 2, config,
                                 0, &twos_team);

   if (npes > 3)
       shmem_team_split_strided(SHMEM_TEAM_WORLD, 3, 3, (npes-1) / 3, config,
                                 0, &threes_team);

   int my_pe_twos   = shmem_team_my_pe(twos_team);
   int my_pe_threes = shmem_team_my_pe(threes_team);
   int npes_twos    = shmem_team_n_pes(twos_team);
   int npes_threes  = shmem_team_n_pes(threes_team);

   if (twos_team != SHMEM_TEAM_INVALID) {
      /* put the value 2 to the next team member in a circular fashion */
      shmem_p(&x, 2, shmem_team_translate_pe(twos_team, (my_pe_twos + 1) %
                                              npes_twos, SHMEM_TEAM_WORLD));
      shmem_quiet();
      shmem_sync(twos_team);
   }

   shmem_sync(SHMEM_TEAM_WORLD);

   if (threes_team != SHMEM_TEAM_INVALID) {
      /* put the value 3 to the next team member in a circular fashion */
      shmem_p(&x, 3, shmem_team_translate_pe(threes_team, (my_pe_threes + 1) %
                                              npes_threes, SHMEM_TEAM_WORLD));
      shmem_quiet();
      shmem_sync(threes_team);
   }

   if (me && me % 3 == 0) {
      if (x != 3) shmem_global_exit(3);
   }
   else if (me && me % 2 == 0) {
      if (x != 2) shmem_global_exit(2);
   }
   else if (x != 10101)  {
      shmem_global_exit(1);
   }

   shmem_finalize();
   return 0;
}
