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

/*
 * SGI/SHMEM version of the C "pi" program that is part of the MPICH
 * distribution
 *
 * MPI version is:
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory (of MPICH distribution).
 */

#include <shmem.h>
#include <stdlib.h>
#include <sys/time.h>

#include <stdio.h>
#include <math.h>

static const double PI25DT = 3.141592653589793238462643;

double
f (double a)
{
  return (4.0 / (1.0 + a * a));
}

/*
 * these all need to be symmetric as shmem targets
 */
int n;

long pSync[_SHMEM_BCAST_SYNC_SIZE];

double mypi, pi;
double pWrk[_SHMEM_REDUCE_SYNC_SIZE];

/*
 *
 */

int
main (int argc, char *argv[])
{
  int myid, numprocs, i;
  double h, sum, x;
  struct timeval startwtime, endwtime;

  start_pes (0);
  numprocs = _num_pes ();
  myid = _my_pe ();

  if (myid == 0)
    {
      if (argc > 1)
	n = atoi (argv[1]);	/* # rectangles on command line */
      else
	n = 10000;		/* default # of rectangles */

      gettimeofday (&startwtime, NULL);
    }

  /* initialize sync array */
  for (i = 0; i < _SHMEM_BCAST_SYNC_SIZE; i += 1)
    pSync[i] = _SHMEM_SYNC_VALUE;
  shmem_barrier_all ();

  /* send "n" out to everyone */
  shmem_broadcast32 (&n, &n, 1, 0, 0, 0, numprocs, pSync);

  /* do partial computation */
  h = 1.0 / (double) n;
  sum = 0.0;
  /* A slightly better approach starts from large i and works back */
  for (i = myid + 1; i <= n; i += numprocs)
    {
      x = h * ((double) i - 0.5);
      sum += f (x);
    }
  mypi = h * sum;

  /* wait for everyone to finish */
  shmem_barrier_all ();

  /* add up partial pi computations into PI */
  shmem_double_sum_to_all (&pi, &mypi, 1, 0, 0, numprocs, pWrk, pSync);

  /* "master" PE summarizes */
  if (myid == 0)
    {
      double elapsed;
      gettimeofday (&endwtime, NULL);
      elapsed = (endwtime.tv_sec - startwtime.tv_sec) * 1000.0;	/* sec to ms */
      elapsed += (endwtime.tv_usec - startwtime.tv_usec) / 1000.0;	/* us to ms */
      printf ("pi is approximately %.16f, Error is %.16f\n",
	      pi, fabs (pi - PI25DT));
      printf ("run time = %f ms\n", elapsed);
      fflush (stdout);
    }

  return 0;
}
