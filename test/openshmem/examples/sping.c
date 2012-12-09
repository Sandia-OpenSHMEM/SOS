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
*/

/*
 * The ping-pong example from Quadrics
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include <unistd.h>
#include <shmem.h>

int getSize (char *str)
{
  int             size;
  char            mod[32];

  switch (sscanf (str, "%d%1[mMkK]", &size, mod))
    {
    case 1:
      return (size);

    case 2:
      switch (*mod)
	{
	case 'm':
	case 'M':
	  return (size << 20);

	case 'k':
	case 'K':
	  return (size << 10);

	default:
	  return (size);
	}

    default:
      return (-1);
    }
}

double gettime()
{
  struct timeval tv;
  gettimeofday(&tv, 0);
  return (tv.tv_sec * 1000000 + tv.tv_usec);
}

double dt (double *tv1, double *tv2)
{
  return (*tv1 - *tv2);
}

void usage (char *name)
{
  fprintf (stderr, "Usage: %s [flags] nwords [maxWords] [incWords]\n", name);
  fprintf (stderr, "       %s -h\n", name);
  exit (1);
}

void help (char *name)
{
  printf ("Usage: %s [flags] nwords [maxWords] [incWords]\n", name);
  printf ("\n");
  printf ("   Flags may be any of\n");
  printf ("      -n number              repititions to time\n");
  printf ("      -e                     everyone print timing info\n");
  printf ("      -h                     print this info\n");
  printf ("\n");
  printf ("   Numbers may be postfixed with 'k' or 'm'\n");
  printf ("\n");
  exit (0);
}

void printStats (int proc, int peer, int doprint, int now, double t)
{
  if (doprint || (proc & 1))
    printf("%3d pinged %3d: %8d words %9.2f uSec %8.2f MB/s\n",
	   proc, peer, now, t, sizeof(long)*now/(t));
}

volatile int tony = 1;

int main (int argc, char *argv[])
{
  double         t,tv[2];
    
  int            reps = 10000;
  int            doprint = 0;
  char          *progName;
  int            minWords = 1;
  int            maxWords = 1;
  int            incWords;
  int            nwords;
  int            nproc;
  int            proc;
  int            peer;
  int            c;
  int            r;
  int            i;
  long          *rbuf;
  long          *tbuf;
   
  start_pes(0);

  proc = _my_pe();
  nproc = _num_pes();

  for (progName = argv[0] + strlen(argv[0]);
       progName > argv[0] && *(progName - 1) != '/';
       progName--)
    ;

  while ((c = getopt (argc, argv, "n:eh")) != -1)
    switch (c)
      {
      case 'n':
	if ((reps = getSize (optarg)) <= 0)
	  usage (progName);
	break;

      case 'e':
	doprint++;
	break;

      case 'h':
	help (progName);

      default:
	usage (progName);
      }

  if (optind == argc)
    minWords = 1;
  else if ((minWords = getSize (argv[optind++])) <= 0)
    usage (progName);

  if (optind == argc)
    maxWords = minWords;
  else if ((maxWords = getSize (argv[optind++])) < minWords)
    usage (progName);

  if (optind == argc)
    incWords = 0;
  else if ((incWords = getSize (argv[optind++])) < 0)
    usage (progName);

  if (!(rbuf = (long *)shmalloc(maxWords * sizeof(long))))
    {
      perror ("Failed memory allocation");
      exit (1);
    }
  memset (rbuf, 0, maxWords * sizeof (long)); 
  shmem_barrier_all();

  if (!(tbuf = (long *)malloc(maxWords * sizeof(long))))
    {
      perror ("Failed memory allocation");
      exit (1);
    }

  if (nproc == 1)
    return 0;

  for (i = 0; i < maxWords; i++)
    tbuf[i] = 1000 + (i & 255);

  if (doprint)
    printf ("%d(%d): Shmem PING reps %d minWords %d maxWords %d incWords %d\n",
	    proc, nproc, reps, minWords, maxWords, incWords);

  shmem_barrier_all();
   
  peer = proc ^ 1;
  if (peer >= nproc)
    doprint = 0;

  for (nwords = minWords;
       nwords <= maxWords;
       nwords = incWords ? nwords + incWords : nwords ? 2 * nwords : 1)
    {
      r = reps;

      shmem_barrier_all();

      tv[0] = gettime();
           
      if (peer < nproc)
        {
	  if (proc & 1)
            {
              r--;
              shmem_wait(&rbuf[nwords-1], 0);
              rbuf[nwords-1] = 0;
            }
           
	  while (r-- > 0)
	    {
              shmem_long_put(rbuf, tbuf, nwords, peer);
              shmem_wait(&rbuf[nwords-1], 0); 
              rbuf[nwords-1] = 0;
            }
           
	  if (proc & 1)
	    shmem_long_put(rbuf, tbuf, nwords, peer);
        }
         
      tv[1] = gettime();


      t = dt (&tv[1], &tv[0]) / (2 * reps);
        
      shmem_barrier_all();
         
      printStats (proc, peer, doprint, nwords, t);
    }

  shmem_barrier_all();

  free(tbuf);
  shfree(rbuf);

  return 0;
}
