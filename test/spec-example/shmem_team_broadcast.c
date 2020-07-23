/*
 *  This test program is derived from an example program in the
 *  OpenSHMEM specification.
 */

#include <stdio.h>
#include <stdlib.h>
#include <shmem.h>
#include <shmemx.h>

int main(void)
{
   static long source[4], dest[4];

   shmem_init();
   int me = shmem_my_pe();

   if (me == 0)
      for (int i = 0; i < 4; i++)
         source[i] = i;

   shmem_long_broadcast(SHMEM_TEAM_WORLD, dest, source, 4, 0);

   printf("%d: %ld, %ld, %ld, %ld\n", me, dest[0], dest[1], dest[2], dest[3]);

   if (me != 0)
       for (int i = 0; i < 4; i++)
           if (dest[i] != i)
               shmem_global_exit(1);

   shmem_finalize();
   return 0;
}
