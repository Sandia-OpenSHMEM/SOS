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
    shmem_global_exit(0);
  }

  int zdim = (npes / (xdim*ydim)) + ( ((npes % (xdim*ydim)) > 0) ? 1 : 0 );
  shmemx_team_t xteam, yzteam, yteam, zteam;

  shmemx_team_split_2d(SHMEMX_TEAM_WORLD, xdim, NULL, 0, &xteam, NULL, 0, &yzteam);
  // No synchronization is needed between these split operations
  // yzteam is immediately ready to be used in collectives
  shmemx_team_split_2d(yzteam, ydim, NULL, 0, &yteam, NULL, 0, &zteam);

  // We don't need the yzteam anymore
  shmemx_team_destroy(&yzteam);

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


/*
 * Example split of SHMEM_TEAM_WORLD, size 16 into 3D
 * xdim = 3, ydim = 4 -> final dimensions are 3x4x2
 *
 * First split of SHMEM_TEAM_WORLD, xdim=3
 * results in 6 xteams and 3 yzteam
 **********************************************
 *        x=0 | x=1 | x=2 |
 *      -------------------
 * yz=0 |   0 |   1 |   2 | <-- xteam
 * yz=1 |   3 |   4 |   5 | <-- xteam
 * yz=2 |   6 |   7 |   8 | <-- xteam
 * yz=3 |   9 |  10 |  11 | <-- xteam
 * yz=4 |  12 |  13 |  14 | <-- xteam
 * yz=5 |  15 |           | <-- xteam
 *         ^      ^     ^
 *         { yzteams are columns }
 **********************************************
 *
 * Second split of yzteam for x=0, ydim=4
 * results in 2 yteams and 4 zteams
 **********************************************
 *       y=0 | y=1 | y=2 | y=3 |
 *     -------------------------
 * z=0 |   0 |   3 |   6 |   9 | <-- yteam
 * z=1 |  12 |  15 |           | <-- yteam
 *         ^     ^     ^     ^
 *         { zteams are columns }
 **********************************************
 *
 * Second split of yzteam for x=1, ydim=4
 * results in 2 yteams and 4 zteams
 **********************************************
 *       y=0 | y=1 | y=2 | y=3 |
 *     -------------------------
 * z=0 |   1 |   4 |   7 |  10 | <-- yteam
 * z=1 |  13 |     |           | <-- yteam
 *         ^     ^     ^     ^
 *         { zteams are columns }
 **********************************************
 *
 * Second split of yzteam for x=2, ydim=4
 * results in 2 yteams and 4 zteams
 **********************************************
 *       y=0 | y=1 | y=2 | y=3 |
 *     -------------------------
 * z=0 |   2 |   5 |   8 |  11 | <-- yteam
 * z=1 |  14 |     |           | <-- yteam
 *         ^     ^     ^     ^
 *         { zteams are columns }
 **********************************************
 *
 * Final number of teams for each dimension:
 * 6 xteams, these are teams where (z,y) is fixed and x varies
 * 6 yteams, these are teams where (x,z) is fixed and y varies
 * 12 zteams, these are teams where (x,y) is fixed and z varies
 *
 * Expected output:
 * (0, 0, 0) is me = 0
 * (1, 0, 0) is me = 1
 * (2, 0, 0) is me = 2
 * (0, 1, 0) is me = 3
 * (1, 1, 0) is me = 4
 * (2, 1, 0) is me = 5
 * (0, 2, 0) is me = 6
 * (1, 2, 0) is me = 7
 * (2, 2, 0) is me = 8
 * (0, 3, 0) is me = 9
 * (1, 3, 0) is me = 10
 * (2, 3, 0) is me = 11
 * (0, 0, 1) is me = 12
 * (1, 0, 1) is me = 13
 * (2, 0, 1) is me = 14
 * (0, 1, 1) is me = 15
*/

