/*
 *
 * Copyright (c) 2011, 2012 
 *   University of Houston System and Oak Ridge National Laboratory.
 * 
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * o Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * 
 * o Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * 
 * o Neither the name of the University of Houston System, Oak Ridge
 *   National Laboratory nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */
/* Application for solving heat conduction task and generate the image file.
 * Original source: Adopted/ported from the source url http://www.kiam.ru/MVS/documents/k100/examples/progrev_shmem_cpu.cpp  
 * Sample run: oshrun -np 2 shmem_heat_image ROWS COLS
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <shmem.h>
#include <sys/time.h>
#include <unistd.h>

static double
gettime (void)
{
  struct timeval tv;
  gettimeofday (&tv, 0);
  return (tv.tv_sec * 1000000 + tv.tv_usec);
}

static double
dt (double *tv1, double *tv2)
{
  return (*tv1 - *tv2);
}


// dimension.h
#define DIM2( basetype, name, w1 ) basetype (*name)[w1]
#define DIM3( basetype, name, w1, w2 ) basetype (*name)[w1][w2]
#define DIM4( basetype, name, w1, w2, w3 ) basetype (*name)[w1][w2][w3]
#define DIM5( basetype, name, w1, w2, w3, w4 ) basetype (*name)[w1][w2][w3][w4]
#define DIM6( basetype, name, w1, w2, w3, w4, w5 ) basetype (*name)[w1][w2][w3][w4][w5]
#define DIM7( basetype, name, w1, w2, w3, w4, w5, w6 ) basetype (*name)[w1][w2][w3][w4][w5][w6]
// file name of output image 
#ifdef PORTALS_SHMEM_H
#define FILENAME "/dev/null"
#else
#define FILENAME "tmp.image"
#endif
// Change here the number of steps, the cell geometry, etc
#define NITER 5000
#define STEPITER 1000
#define delx 0.5
#define dely 0.25
// end change here.

static void
itstep (int mx, int my, void *pf, void *pnewf, void *pr, double rdx2,
	double rdy2, double beta)
{
  DIM2 (double, f, my) = (typeof (f)) pf;
  DIM2 (double, newf, my) = (typeof (newf)) pnewf;
  DIM2 (double, r, my) = (typeof (r)) pr;
  int i, j, mx1, my1;
  mx1 = mx - 1;
  my1 = my - 1;
  for (i = 1; i < mx1; i++)
    {
      for (j = 1; j < my1; j++)
	{
	  newf[i][j] =
	    ((f[i - 1][j] + f[i + 1][j]) * rdx2 +
	     (f[i][j - 1] + f[i][j + 1]) * rdy2 - r[i][j]) * beta;
	}
    }
}


int
main (int argc, char **argv)
{
  int i, j, n, mx1, my1, my_number, n_of_nodes, totalmx, partmx, leftmx,
    mx, my;
  FILE *fp;
  double t, tv[2];
  double rdx2, rdy2, beta;

  start_pes (0);
  my_number = _my_pe ();
  n_of_nodes = _num_pes ();

  if (1 == argc) {
      my = totalmx = mx = (10 > (n_of_nodes * 2) ) ? 10 : n_of_nodes * 2;
  } else if (argc != 3)
    {
      if (!my_number)
	fprintf (stderr, "Usage: %s <nrows> <ncolumns>\n", argv[0]);
      return (-1);
    } else {
      totalmx = mx = (int) atol (argv[1]);
      my = (int) atol (argv[2]);
  }

  if (my < 1)
    {
      if (!my_number)
	fprintf (stderr, "Number of columns (%d) should be positive\n", my);
      return (-1);
    }

/* Compute the number of rows per node: */
  mx = (totalmx + n_of_nodes - 1) / n_of_nodes;

/* This is the number of rows for all but the last: */
  partmx = mx;

/* This is the number of rows for the last: */
/* It cannot be greater than partmx, but it can be non-positive: */
  leftmx = totalmx - partmx * (n_of_nodes - 1);
  if (leftmx < 1)
    {
      if (!my_number)
	fprintf (stderr, "Cannot distribute rows, too many processors\n");
      return (77);
    }

  if (my_number == (n_of_nodes - 1))
    mx = leftmx;
/* End rows distribution. */
  partmx += 2;
  mx += 2;
  my += 2;

/* Here we know the array sizes, so make the arrays themselves: */
  {
    DIM2 (double, f, my);
    DIM2 (double, newf, my);
    DIM2 (double, r, my);
    typeof (f) pf[2];
    int curf;

    f = (typeof (f)) shmalloc (2 * partmx * sizeof (*f));
    r = (typeof (r)) malloc (mx * sizeof (*r));
    if ((!f) || (!r))
      {
	fprintf (stderr, "Cannot allocate, exiting\n");
	exit (-1);
      }
    curf = 0;
    pf[0] = f;
    pf[1] = f + partmx;
    newf = pf[1];
    rdx2 = 1. / delx / delx;
    rdy2 = 1. / dely / dely;
    beta = 1.0 / (2.0 * (rdx2 + rdy2));
    if (!my_number)
      {
	printf
	  ("Solving heat conduction task on %d by %d grid by %d processors\n",
	   totalmx, my - 2, n_of_nodes);
	fflush (stdout);
      }

    for (i = 0; i < mx; i++)
      {
	for (j = 0; j < my; j++)
	  {
	    if (((i == 0) && (my_number == 0)) || (j == 0)
		|| ((i == (mx - 1)) && (my_number == (n_of_nodes - 1)))
		|| (j == (my - 1)))
	      newf[i][j] = f[i][j] = 1.0;
	    else
	      newf[i][j] = f[i][j] = 0.0;
	    r[i][j] = 0.0;
	  }
      }

    mx1 = mx - 1;
    my1 = my - 1;
    shmem_barrier_all ();
/* Iteration loop: */
    tv[0] = gettime ();

    for (n = 0; n < NITER; n++)
      {

	if (!my_number)
	  {
	    if (!(n % STEPITER))
	      printf ("Iteration %d\n", n);
	  }
	/* Step of calculation starts here: */
	f = pf[curf];
	newf = pf[1 - curf];
	itstep (mx, my, f, newf, r, rdx2, rdy2, beta);
	/* Do all the transfers: */
	shmem_barrier_all ();
	if (my_number > 0)
	  shmem_double_put (&(newf[partmx - 1][1]), &(newf[1][1]), my - 2,
			    my_number - 1);
	if (my_number < (n_of_nodes - 1))
	  shmem_double_put (&(newf[0][1]), &(newf[mx - 2][1]), my - 2,
			    my_number + 1);
	shmem_barrier_all ();
	/* swap the halves: */

	curf = 1 - curf;
      }

    if (!my_number)
      {
	tv[1] = gettime ();
	t = dt (&tv[1], &tv[0]);
	printf ("Elapsed time: %4.2f sec\n", t / 1000000.0);
#ifndef PORTALS_SHMEM_H
	printf ("Output image file in current directory\n");
#endif
	fp = fopen (FILENAME, "w");
	fclose (fp);
      }

    for (j = 0; j < n_of_nodes; j++)
      {
	shmem_barrier_all ();
	if (j == my_number)
	  {
	    fp = fopen (FILENAME, "a");
	    for (i = 1; i < (mx - 1); i++)
	      fwrite (&(newf[i][1]), my - 2, sizeof (newf[0][0]), fp);
	    fclose (fp);
	  }
      }
  }

  return 0;
}
