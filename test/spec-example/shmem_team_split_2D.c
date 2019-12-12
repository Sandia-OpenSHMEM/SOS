/*
 *  This test program is derived from an example program in the
 *  OpenSHMEM specification.
 */

#include <stdio.h>
#include <shmem.h>
#include <shmemx.h>

int main(void)
{
  int xdim = 3;
  int ydim = 4;

  shmem_init();
  int pe = shmem_my_pe();
  int npes = shmem_n_pes();

  if (npes < (xdim*ydim)) {
    fprintf(stderr, "Not enough PEs to create 4x3xN layout\n");
    shmem_global_exit(1);
  }

  int zdim = (npes / (xdim*ydim)) + ( ((npes % (xdim*ydim)) > 0) ? 1 : 0 );
  shmemx_team_t xteam, yzteam, yteam, zteam;


  shmemx_team_split_2d(SHMEMX_TEAM_WORLD, xdim, NULL, 0, &xteam, NULL, 0, &yzteam);
  // No synchronization is needed between these split operations
  // yzteam is immediately ready to be used in collectives
  shmemx_team_split_2d(yzteam, ydim, NULL, 0, &yteam, NULL, 0, &zteam);

  // We don't need the yzteam anymore
  shmemx_team_destroy(yzteam);

  int my_x = shmemx_team_my_pe(xteam);
  int my_y = shmemx_team_my_pe(yteam);
  int my_z = shmemx_team_my_pe(zteam);

  for (int zdx = 0; zdx < zdim; zdx++)
    for (int ydx = 0; ydx < ydim; ydx++)
      for (int xdx = 0; xdx < xdim; xdx++) {
        if ((my_x == xdx) && (my_y == ydx) && (my_z == zdx)) {
          printf ("(%d, %d, %d) is me = %d\n", my_x, my_y, my_z, pe);
        }
        shmemx_team_sync(SHMEMX_TEAM_WORLD);
      }

  shmem_finalize();
}
