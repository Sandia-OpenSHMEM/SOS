/*
 *  This test program is derived from an example program in the
 *  OpenSHMEM specification.
 */

#include <shmem.h>
#include <stdio.h>
#include <math.h>

/*  Find x and y such that x * y == npes and abs(x - y) is minimized.  */
static void find_xy_dims(int npes, int *x, int *y) {
  for(int divider = ceil(sqrt(npes)); divider >= 1; divider--)
    if (npes % divider == 0) {
      *x = divider;
      *y = npes / divider;
      return;
    }
}

/*  Find x, y, and z such that x * y * z == npes and
 *  abs(x - y) + abs(x - z) + abs(y - z) is minimized.  */
static void find_xyz_dims(int npes, int *x, int *y, int *z) {
  *x = *y = *z = 1;
  for(int divider = ceil(cbrt(npes)); divider >= 1; divider--)
    if (npes % divider == 0) {
      *x = divider;
      find_xy_dims(npes / divider, y, z);
      return;
    }
}

int main(void) {
  int xdim, ydim, zdim;

  shmem_init();
  int mype = shmem_my_pe();
  int npes = shmem_n_pes();

  find_xyz_dims(npes, &xdim, &ydim, &zdim);

  if (shmem_my_pe() == 0) printf("xdim = %d, ydim = %d, zdim = %d\n", xdim, ydim, zdim);

  shmem_team_t xteam, yzteam, yteam, zteam;

  shmem_team_split_2d(SHMEM_TEAM_WORLD, xdim, NULL, 0, &xteam, NULL, 0, &yzteam);
  // yzteam is immediately ready to be used in collectives
  shmem_team_split_2d(yzteam, ydim, NULL, 0, &yteam, NULL, 0, &zteam);

  // We don't need the yzteam anymore
  shmem_team_destroy(yzteam);

  int my_x = shmem_team_my_pe(xteam);
  int my_y = shmem_team_my_pe(yteam);
  int my_z = shmem_team_my_pe(zteam);

  for (int zdx = 0; zdx < zdim; zdx++) {
    for (int ydx = 0; ydx < ydim; ydx++) {
      for (int xdx = 0; xdx < xdim; xdx++) {
        if ((my_x == xdx) && (my_y == ydx) && (my_z == zdx)) {
          printf("(%d, %d, %d) is mype = %d\n", my_x, my_y, my_z, mype);
        }
        shmem_team_sync(SHMEM_TEAM_WORLD);
      }
    }
  }

  shmem_finalize();
  return 0;
}
