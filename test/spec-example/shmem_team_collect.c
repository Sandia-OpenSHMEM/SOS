/*
 *  This test program is derived from an example program in the
 *  OpenSHMEM specification.
 */

#include <stdio.h>
#include <stdlib.h>
#include <shmem.h>

int main(void)
{
   static long lock = 0;

   shmem_init();
   int me          = shmem_my_pe();
   int npes        = shmem_n_pes();
   int my_nelem    = me + 1; /* linearly increasing number of elements with PE */
   int total_nelem = (npes * (npes + 1)) / 2;

   int *source = (int *)shmem_malloc(npes * sizeof(int)); /* symmetric alloc */
   int *dest   = (int *)shmem_malloc(total_nelem * sizeof(int));

   for (int i = 0; i < my_nelem; i++)
      source[i] = (me * (me + 1)) / 2 + i;
   for (int i = 0; i < total_nelem; i++)
      dest[i] = -9999;

   /* Wait for all PEs to initialize source/dest: */
   shmem_team_sync(SHMEM_TEAM_WORLD);

   shmem_int_collect(SHMEM_TEAM_WORLD, dest, source, my_nelem);

   shmem_set_lock(&lock); /* Lock prevents interleaving printfs */
   printf("%d: %d", me, dest[0]);
   for (int i = 1; i < total_nelem; i++)
      printf(", %d", dest[i]);
   printf("\n");
   shmem_clear_lock(&lock);

   for (int i = 0; i < total_nelem; i++)
       if (dest[i] != i)
           shmem_global_exit(1);

   shmem_finalize();
   return 0;
}
