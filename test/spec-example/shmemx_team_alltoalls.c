/*
 *  This test program is derived from an example program in the
 *  OpenSHMEM specification.
 */

#include <stdio.h>
#include <inttypes.h>
#include <shmem.h>
#include <shmemx.h>

int main(void)
{
   shmem_init();
   int me = shmem_my_pe();
   int npes = shmem_n_pes();

   const int count = 2;
   const ptrdiff_t dst = 2;
   const ptrdiff_t sst = 3;
   int64_t* dest = (int64_t*) shmem_malloc(count * dst * npes * sizeof(int64_t));
   int64_t* source = (int64_t*) shmem_malloc(count * sst * npes * sizeof(int64_t));

   /* assign source values */
   for (int pe = 0; pe < npes; pe++) {
      for (int i = 0; i < count; i++) {
         source[sst * ((pe * count) + i)] = me + pe;
         dest[dst * ((pe * count) + i)] = 9999;
      }
   }
   /* wait for all PEs to initialize source/dest */
   shmemx_team_sync(SHMEMX_TEAM_WORLD);

   /* alltoalls on all PES */
   shmemx_int64_alltoalls(SHMEMX_TEAM_WORLD, dest, source, dst, sst, count);

   /* verify results */
   for (int pe = 0; pe < npes; pe++) {
      for (int i = 0; i < count; i++) {
         int j = dst * ((pe * count) + i);
         if (dest[j] != pe + me) {
            printf("[%d] ERROR: dest[%d]=%" PRId64 ", should be %d\n",
               me, j, dest[j], pe + me);
          }
       }
   }

   shmem_free(dest);
   shmem_free(source);
   shmem_finalize();
   return 0;
}
