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


/* Application for 2d heat transfer modelling using 3 methods.
 * Original source: Adopted/ported from the mpi version at source url http://www.cct.lsu.edu/~estrabd/2dheat.php  
 * Sample run: oshrun -np 2 shmem_2dheat -h 10 -w 10 -v -m 1 // for method 1 of jacobi 
 */

#define _WIDTH   20
#define _HEIGHT  20
#define H       1.0
#define _EPSILON 0.1
/* 
  methods:
  1 - jacobi
  2 - gauss-seidel
  3 - sor
*/
#define _METHOD 2
#define ITERMAX 10
#define T_SRC0  550.0
#define ROOT    0

/* Includes */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/time.h>
#include <unistd.h>
#include <shmem.h>

/* declare functions */
int get_start (int rank);
int get_end (int rank);
int get_num_rows (int rank);
void init_domain (float **domain_ptr, int rank);
void jacobi (float **current_ptr, float **next_ptr);
void gauss_seidel (float **current_ptr, float **next_ptr);
void sor (float **current_ptr, float **next_ptr);
float get_val_par (float *above_ptr, float **domain_ptr, float *below_ptr,
		   int rank, int i, int j);
void enforce_bc_par (float **domain_ptr, int rank, int i, int j);
int global_to_local (int rank, int row);
float f (int i, int j);
float get_convergence_sqd (float **current_ptr, float **next_ptr, int rank);

/* declare and set globals */
int WIDTH = _WIDTH;
int HEIGHT = _HEIGHT;
int meth = _METHOD;
float EPSILON = _EPSILON;

long pSync[_SHMEM_REDUCE_SYNC_SIZE];
float pWrk[_SHMEM_REDUCE_MIN_WRKDATA_SIZE];

float convergence;
float convergence_sqd, local_convergence_sqd;

/* Function pointer to solver method of choice */
void (*method) ();
double
gettime ()
{
  struct timeval tv;
  gettimeofday (&tv, 0);
  return (tv.tv_sec * 1000000 + tv.tv_usec);
}

double
dt (double *tv1, double *tv2)
{
  return (*tv1 - *tv2);
}

int p, my_rank;

int
main (int argc, char **argv)
{
  /* arrays used to contain each PE's rows - specify cols, no need to spec rows */
  float **U_Curr;
  float **U_Next;
  /* helper variables */
  /* available iterator  */
  int i, j, k, m, n;
  int per_proc, remainder, my_start_row, my_end_row, my_num_rows;
  int verbose = 0;
  int show_time = 0;
  double time;
  double t, tv[2];

  /*OpenSHMEM initilization*/
  start_pes (0);
  p = _num_pes ();
  my_rank = _my_pe ();

  if (p > 8) {
      fprintf(stderr, "Ignoring test when run with more than 8 pes\n");
      return 77;
  }

  /* argument processing done by everyone */
  int c, errflg;
  extern char *optarg;
  extern int optind, optopt;

  while ((c = getopt (argc, argv, "e:h:m:tw:v")) != -1)
    {
      switch (c)
	{
	case 'e':
	  EPSILON = atof (optarg);
	  break;
	case 'h':
	  HEIGHT = atoi (optarg);
	  break;
	case 'm':
	  /* selects the numerical methods */
	  switch (atoi (optarg))
	    {
	    case 1:		/* jacobi */
	      meth = 1;
	      break;
	    case 2:		/* gauss-seidel */
	      meth = 2;
	      break;
	    case 3:		/* sor */
	      meth = 3;
	      break;
	    }
	  break;
	case 't':
	  show_time++;		/* overridden by -v (verbose) */
	  break;
	case 'w':
	  WIDTH = atoi (optarg);
	  break;
	case 'v':
	  verbose++;
	  break;
	  /* handle bad arguments */
	case ':':		/* -h or -w without operand */
	  if (ROOT == my_rank)
	    fprintf (stderr, "Option -%c requires an operand\n", optopt);
	  errflg++;
	  break;
	case '?':
	  if (ROOT == my_rank)
	    fprintf (stderr, "Unrecognized option: -%c\n", optopt);
	  errflg++;
	  break;
	}
    }

  if (ROOT == my_rank && argc < 2)
    {
      printf ("Using defaults: -h 20 -w 20 -m 2\n");
    }

//  if (0 < errflg) 
//      exit(EXIT_FAILURE);


  /* wait for user to input runtime params */
 
  for (i = 0; i < _SHMEM_REDUCE_SYNC_SIZE; i += 1)
    pSync[i] = _SHMEM_SYNC_VALUE;

  shmem_barrier_all ();


  /* broadcast method to use  */
  
  shmem_broadcast32 (&meth, &meth, 1, 0, 0, 0, p, pSync);
  switch (meth)
    {
    case 1:
      method = &jacobi;
      break;
    case 2:
      method = &gauss_seidel;
      break;
    case 3:
      method = &sor;
      break;
    }

  /* let each processor decide what rows(s) it owns */
  my_start_row = get_start (my_rank);
  my_end_row = get_end (my_rank);
  my_num_rows = get_num_rows (my_rank);

  if (0 < verbose)
    printf ("proc %d contains (%d) rows %d to %d\n", my_rank, my_num_rows,
	    my_start_row, my_end_row);
  fflush (stdout);

  /* allocate 2d array */
  U_Curr = (float **) malloc (sizeof (float *) * my_num_rows);
  U_Curr[0] =
    (float *) malloc (sizeof (float) * my_num_rows * (int) floor (WIDTH / H));
  for (i = 1; i < my_num_rows; i++)
    {
      U_Curr[i] = U_Curr[i - 1] + (int) floor (WIDTH / H);
    }

  /* allocate 2d array */
  U_Next = (float **) malloc (sizeof (float *) * my_num_rows);
  U_Next[0] =
    (float *) malloc (sizeof (float) * my_num_rows * (int) floor (WIDTH / H));
  for (i = 1; i < my_num_rows; i++)
    {
      U_Next[i] = U_Next[i - 1] + (int) floor (WIDTH / H);
    }

  /* initialize global grid */
  init_domain (U_Curr, my_rank);
  init_domain (U_Next, my_rank);

  /* iterate for solution */
  if (my_rank == ROOT)
    {
     
      tv[0] = gettime ();
    }
  k = 1;
  while (1)
    {
      method (U_Curr, U_Next);

      local_convergence_sqd = get_convergence_sqd (U_Curr, U_Next, my_rank);
     
      shmem_barrier_all ();
      shmem_float_sum_to_all (&convergence_sqd, &local_convergence_sqd, 1, 0,
			      0, p, pWrk, pSync);
      if (my_rank == ROOT)
	{
	  convergence = sqrt (convergence_sqd);
	  if (verbose == 1)
	    {
	      printf ("L2 = %f\n", convergence);
	    }
	}

      /* broadcast method to use */
     
      shmem_barrier_all ();
      shmem_broadcast32 (&convergence, &convergence, 1, 0, 0, 0, p, pSync);
      if (convergence <= EPSILON)
	{
	  break;
	}

      /* copy U_Next to U_Curr */
      for (j = my_start_row; j <= my_end_row; j++)
	{
	  for (i = 0; i < (int) floor (WIDTH / H); i++)
	    {
	      U_Curr[j - my_start_row][i] = U_Next[j - my_start_row][i];
	    }
	}
      k++;
      //MPI_Barrier(MPI_COMM_WORLD);    
      shmem_barrier_all ();
    }


  /* say something at the end */
  if (my_rank == ROOT)
    {
      //time = MPI_Wtime() - time;
      tv[1] = gettime ();
      t = dt (&tv[1], &tv[0]);
      printf
	("Estimated time to convergence in %d iterations using %d processors on a %dx%d grid is %f seconds\n",
	 k, p, (int) floor (WIDTH / H), (int) floor (HEIGHT / H),
	 t / 1000000.0);
    }

  //MPI_Finalize();
  exit (EXIT_SUCCESS);
  return 0;
}

 /* used by each PE to compute the sum of the squared diffs between current iteration and previous */

float
get_convergence_sqd (float **current_ptr, float **next_ptr, int rank)
{
  int i, j, my_start, my_end, my_num_rows;
  float sum;

  my_start = get_start (rank);
  my_end = get_end (rank);
  my_num_rows = get_num_rows (rank);

  sum = 0.0;
  for (j = my_start; j <= my_end; j++)
    {
      for (i = 0; i < (int) floor (WIDTH / H); i++)
	{
	  sum +=
	    pow (next_ptr[global_to_local (rank, j)][i] -
		 current_ptr[global_to_local (rank, j)][i], 2);
	}
    }
  return sum;
}

 /* implements parallel jacobi methods */

void
jacobi (float **current_ptr, float **next_ptr)
{
  int i, j, my_start, my_end, my_num_rows;
  float *U_Curr_Above = (float *) shmalloc ((sizeof (float)) * ((int) floor (WIDTH / H)));	/* 1d array holding values from bottom row of PE above */
  float *U_Curr_Below = (float *) shmalloc ((sizeof (float)) * ((int) floor (WIDTH / H)));	/* 1d array holding values from top row of PE below */
  float *U_Send_Buffer = (float *) shmalloc ((sizeof (float)) * ((int) floor (WIDTH / H)));	/* 1d array holding values that are currently being sent */

  //MPI_Request request;
  //MPI_Status status;
  //MPI_Comm_size(MPI_COMM_WORLD,&p); 
  //MPI_Comm_rank(MPI_COMM_WORLD,&my_rank);

  my_start = get_start (my_rank);
  my_end = get_end (my_rank);
  my_num_rows = get_num_rows (my_rank);

  /*
   * Communicating ghost rows - only bother if p > 1
   */

  if (p > 1)
    {
      /* send/receive bottom rows */
      if (my_rank < (p - 1))
	{
	  /* populate send buffer with bottow row */
	  for (i = 0; i < (int) floor (WIDTH / H); i++)
	    {
	      U_Send_Buffer[i] = current_ptr[my_num_rows - 1][i];
	    }
	  /* non blocking send */
	  //MPI_Isend(U_Send_Buffer,(int)floor(WIDTH/H),MPI_FLOAT,my_rank+1,0,MPI_COMM_WORLD,&request);
	  shmem_float_put (U_Curr_Above, U_Send_Buffer,
			   (int) floor (WIDTH / H), my_rank + 1);
	}
      //if (my_rank > ROOT) {
      /* blocking receive */
      //MPI_Recv(U_Curr_Above,(int)floor(WIDTH/H),MPI_FLOAT,my_rank-1,0,MPI_COMM_WORLD,&status);
      //}
      //MPI_Barrier(MPI_COMM_WORLD);
      shmem_barrier_all ();

      /* send/receive top rows */
      if (my_rank > ROOT)
	{
	  /* populate send buffer with top row */
	  for (i = 0; i < (int) floor (WIDTH / H); i++)
	    {
	      U_Send_Buffer[i] = current_ptr[0][i];
	    }
	  /* non blocking send */
	  //MPI_Isend(U_Send_Buffer,(int)floor(WIDTH/H),MPI_FLOAT,my_rank-1,0,MPI_COMM_WORLD,&request);
	  shmem_float_put (U_Curr_Below, U_Send_Buffer,
			   (int) floor (WIDTH / H), my_rank - 1);
	}
      //if (my_rank < (p-1)) {
      /* blocking receive */
      //MPI_Recv(U_Curr_Below,(int)floor(WIDTH/H),MPI_FLOAT,my_rank+1,0,MPI_COMM_WORLD,&status);      
      //}  
      //MPI_Barrier(MPI_COMM_WORLD);
      shmem_barrier_all ();
    }

  /* Jacobi method using global addressing */
  for (j = my_start; j <= my_end; j++)
    {
      for (i = 0; i < (int) floor (WIDTH / H); i++)
	{
	  next_ptr[j - my_start][i] =
	    .25 *
	    (get_val_par
	     (U_Curr_Above, current_ptr, U_Curr_Below, my_rank, i - 1,
	      j) + get_val_par (U_Curr_Above, current_ptr, U_Curr_Below,
				my_rank, i + 1,
				j) + get_val_par (U_Curr_Above, current_ptr,
						  U_Curr_Below, my_rank, i,
						  j - 1) +
	     get_val_par (U_Curr_Above, current_ptr, U_Curr_Below, my_rank, i,
			  j + 1) - (pow (H, 2) * f (i, j)));
	  enforce_bc_par (next_ptr, my_rank, i, j);
	}
    }
    shfree(U_Send_Buffer);
    shfree(U_Curr_Below);
    shfree(U_Curr_Above);
}

 /* implements parallel g-s method */

void
gauss_seidel (float **current_ptr, float **next_ptr)
{
  int i, j, my_start, my_end, my_num_rows;
  float *U_Curr_Above = (float *) shmalloc ((sizeof (float)) * ((int) floor (WIDTH / H)));	/* 1d array holding values from bottom row of PE above */
  float *U_Curr_Below = (float *) shmalloc ((sizeof (float)) * ((int) floor (WIDTH / H)));	/* 1d array holding values from top row of PE below */
  float *U_Send_Buffer = (float *) shmalloc ((sizeof (float)) * ((int) floor (WIDTH / H)));	/* 1d array holding values that are currently being sent */
  //float U_Curr_Above[(int)floor(WIDTH/H)];  /* 1d array holding values from bottom row of PE above */
  //float U_Curr_Below[(int)floor(WIDTH/H)];  /* 1d array holding values from top row of PE below */
  //float U_Send_Buffer[(int)floor(WIDTH/H)]; /* 1d array holding values that are currently being sent */
  float W = 1.0;

  //MPI_Request request;
  //MPI_Status status;

  //MPI_Comm_size(MPI_COMM_WORLD,&p);
  //MPI_Comm_rank(MPI_COMM_WORLD,&my_rank);

  my_start = get_start (my_rank);
  my_end = get_end (my_rank);
  my_num_rows = get_num_rows (my_rank);

  /*
   * Communicating ghost rows - only bother if p > 1
   */

  if (p > 1)
    {
      /* send/receive bottom rows */
      if (my_rank < (p - 1))
	{
	  /* populate send buffer with bottow row */
	  for (i = 0; i < (int) floor (WIDTH / H); i++)
	    {
	      U_Send_Buffer[i] = current_ptr[my_num_rows - 1][i];
	    }
	  /* non blocking send */
	  //MPI_Isend(U_Send_Buffer,(int)floor(WIDTH/H),MPI_FLOAT,my_rank+1,0,MPI_COMM_WORLD,&request);
	  shmem_float_put (U_Curr_Above, U_Send_Buffer,
			   (int) floor (WIDTH / H), my_rank + 1);
	}
      //if (my_rank > ROOT) {
      /* blocking receive */
      //MPI_Recv(U_Curr_Above,(int)floor(WIDTH/H),MPI_FLOAT,my_rank-1,0,MPI_COMM_WORLD,&status);
      //}
      //MPI_Barrier(MPI_COMM_WORLD);
      shmem_barrier_all ();

      /* send/receive top rows */
      if (my_rank > ROOT)
	{
	  /* populate send buffer with top row */
	  for (i = 0; i < (int) floor (WIDTH / H); i++)
	    {
	      U_Send_Buffer[i] = current_ptr[0][i];
	    }
	  /* non blocking send */
	  //MPI_Isend(U_Send_Buffer,(int)floor(WIDTH/H),MPI_FLOAT,my_rank-1,0,MPI_COMM_WORLD,&request);
	  shmem_float_put (U_Curr_Below, U_Send_Buffer,
			   (int) floor (WIDTH / H), my_rank - 1);
	}
      //if (my_rank < (p-1)) {
      /* blocking receive */
      //MPI_Recv(U_Curr_Below,(int)floor(WIDTH/H),MPI_FLOAT,my_rank+1,0,MPI_COMM_WORLD,&status);
      //}
      //MPI_Barrier(MPI_COMM_WORLD);
      shmem_barrier_all ();
    }

  /* solve next reds (i+j odd) */
  for (j = my_start; j <= my_end; j++)
    {
      for (i = 0; i < (int) floor (WIDTH / H); i++)
	{
	  if ((i + j) % 2 != 0)
	    {
	      next_ptr[j - my_start][i] =
		get_val_par (U_Curr_Above, current_ptr, U_Curr_Below, my_rank,
			     i, j) + (W / 4) * (get_val_par (U_Curr_Above,
							     current_ptr,
							     U_Curr_Below,
							     my_rank, i - 1,
							     j) +
						get_val_par (U_Curr_Above,
							     current_ptr,
							     U_Curr_Below,
							     my_rank, i + 1,
							     j) +
						get_val_par (U_Curr_Above,
							     current_ptr,
							     U_Curr_Below,
							     my_rank, i,
							     j - 1) +
						get_val_par (U_Curr_Above,
							     current_ptr,
							     U_Curr_Below,
							     my_rank, i,
							     j + 1) -
						4 *
						(get_val_par
						 (U_Curr_Above, current_ptr,
						  U_Curr_Below, my_rank, i,
						  j)) - (pow (H, 2) * f (i,
									 j)));
	      enforce_bc_par (next_ptr, my_rank, i, j);
	    }
	}
    }
  /* solve next blacks (i+j) even .... using next reds */
  for (j = my_start; j <= my_end; j++)
    {
      for (i = 0; i < (int) floor (WIDTH / H); i++)
	{
	  if ((i + j) % 2 == 0)
	    {
	      next_ptr[j - my_start][i] =
		get_val_par (U_Curr_Above, current_ptr, U_Curr_Below, my_rank,
			     i, j) + (W / 4) * (get_val_par (U_Curr_Above,
							     next_ptr,
							     U_Curr_Below,
							     my_rank, i - 1,
							     j) +
						get_val_par (U_Curr_Above,
							     next_ptr,
							     U_Curr_Below,
							     my_rank, i + 1,
							     j) +
						get_val_par (U_Curr_Above,
							     next_ptr,
							     U_Curr_Below,
							     my_rank, i,
							     j - 1) +
						get_val_par (U_Curr_Above,
							     next_ptr,
							     U_Curr_Below,
							     my_rank, i,
							     j + 1) -
						4 *
						(get_val_par
						 (U_Curr_Above, next_ptr,
						  U_Curr_Below, my_rank, i,
						  j)) - (pow (H, 2) * f (i,
									 j)));
	      enforce_bc_par (next_ptr, my_rank, i, j);
	    }
	}
    }
    shfree(U_Send_Buffer);
    shfree(U_Curr_Below);
    shfree(U_Curr_Above);
}


 /* implements parallels sor method */

void
sor (float **current_ptr, float **next_ptr)
{
  int i, j, my_start, my_end, my_num_rows;
  float *U_Curr_Above = (float *) shmalloc ((sizeof (float)) * ((int) floor (WIDTH / H)));	/* 1d array holding values from bottom row of PE above */
  float *U_Curr_Below = (float *) shmalloc ((sizeof (float)) * ((int) floor (WIDTH / H)));	/* 1d array holding values from top row of PE below */
  float *U_Send_Buffer = (float *) shmalloc ((sizeof (float)) * ((int) floor (WIDTH / H)));	/* 1d array holding values that are currently being sent */
  //float U_Curr_Above[(int)floor(WIDTH/H)];  /* 1d array holding values from bottom row of PE above */
  //float U_Curr_Below[(int)floor(WIDTH/H)];  /* 1d array holding values from top row of PE below */
  //float U_Send_Buffer[(int)floor(WIDTH/H)]; /* 1d array holding values that are currently being sent */
  float W = 1.5;

  //MPI_Request request;
  //MPI_Status status;

  //MPI_Comm_size(MPI_COMM_WORLD,&p);
  //MPI_Comm_rank(MPI_COMM_WORLD,&my_rank);

  my_start = get_start (my_rank);
  my_end = get_end (my_rank);
  my_num_rows = get_num_rows (my_rank);

  /*
   * Communicating ghost rows - only bother if p > 1
   */

  if (p > 1)
    {
      /* send/receive bottom rows */
      if (my_rank < (p - 1))
	{
	  /* populate send buffer with bottow row */
	  for (i = 0; i < (int) floor (WIDTH / H); i++)
	    {
	      U_Send_Buffer[i] = current_ptr[my_num_rows - 1][i];
	    }
	  /* non blocking send */
	  //MPI_Isend(U_Send_Buffer,(int)floor(WIDTH/H),MPI_FLOAT,my_rank+1,0,MPI_COMM_WORLD,&request);
	  shmem_float_put (U_Curr_Above, U_Send_Buffer,
			   (int) floor (WIDTH / H), my_rank + 1);
	}
      //if (my_rank > ROOT) {
      /* blocking receive */
      //MPI_Recv(U_Curr_Above,(int)floor(WIDTH/H),MPI_FLOAT,my_rank-1,0,MPI_COMM_WORLD,&status);
      //}
      //MPI_Barrier(MPI_COMM_WORLD);
      shmem_barrier_all ();

      /* send/receive top rows */
      if (my_rank > ROOT)
	{
	  /* populate send buffer with top row */
	  for (i = 0; i < (int) floor (WIDTH / H); i++)
	    {
	      U_Send_Buffer[i] = current_ptr[0][i];
	    }
	  /* non blocking send */
	  //MPI_Isend(U_Send_Buffer,(int)floor(WIDTH/H),MPI_FLOAT,my_rank-1,0,MPI_COMM_WORLD,&request);
	  shmem_float_put (U_Curr_Below, U_Send_Buffer,
			   (int) floor (WIDTH / H), my_rank - 1);
	}
      //if (my_rank < (p-1)) {
      /* blocking receive */
      //MPI_Recv(U_Curr_Below,(int)floor(WIDTH/H),MPI_FLOAT,my_rank+1,0,MPI_COMM_WORLD,&status);
      //}
      //MPI_Barrier(MPI_COMM_WORLD);
      shmem_barrier_all ();
    }

  /* solve next reds (i+j odd) */
  for (j = my_start; j <= my_end; j++)
    {
      for (i = 0; i < (int) floor (WIDTH / H); i++)
	{
	  if ((i + j) % 2 != 0)
	    {
	      next_ptr[j - my_start][i] =
		get_val_par (U_Curr_Above, current_ptr, U_Curr_Below, my_rank,
			     i, j) + (W / 4) * (get_val_par (U_Curr_Above,
							     current_ptr,
							     U_Curr_Below,
							     my_rank, i - 1,
							     j) +
						get_val_par (U_Curr_Above,
							     current_ptr,
							     U_Curr_Below,
							     my_rank, i + 1,
							     j) +
						get_val_par (U_Curr_Above,
							     current_ptr,
							     U_Curr_Below,
							     my_rank, i,
							     j - 1) +
						get_val_par (U_Curr_Above,
							     current_ptr,
							     U_Curr_Below,
							     my_rank, i,
							     j + 1) -
						4 *
						(get_val_par
						 (U_Curr_Above, current_ptr,
						  U_Curr_Below, my_rank, i,
						  j)) - (pow (H, 2) * f (i,
									 j)));
	      enforce_bc_par (next_ptr, my_rank, i, j);
	    }
	}
    }
  /* solve next blacks (i+j) even .... using next reds */
  for (j = my_start; j <= my_end; j++)
    {
      for (i = 0; i < (int) floor (WIDTH / H); i++)
	{
	  if ((i + j) % 2 == 0)
	    {
	      next_ptr[j - my_start][i] =
		get_val_par (U_Curr_Above, current_ptr, U_Curr_Below, my_rank,
			     i, j) + (W / 4) * (get_val_par (U_Curr_Above,
							     next_ptr,
							     U_Curr_Below,
							     my_rank, i - 1,
							     j) +
						get_val_par (U_Curr_Above,
							     next_ptr,
							     U_Curr_Below,
							     my_rank, i + 1,
							     j) +
						get_val_par (U_Curr_Above,
							     next_ptr,
							     U_Curr_Below,
							     my_rank, i,
							     j - 1) +
						get_val_par (U_Curr_Above,
							     next_ptr,
							     U_Curr_Below,
							     my_rank, i,
							     j + 1) -
						4 *
						(get_val_par
						 (U_Curr_Above, next_ptr,
						  U_Curr_Below, my_rank, i,
						  j)) - (pow (H, 2) * f (i,
									 j)));
	      enforce_bc_par (next_ptr, my_rank, i, j);
	    }
	}
    }
    shfree(U_Send_Buffer);
    shfree(U_Curr_Below);
    shfree(U_Curr_Above);
}

 /* enforces bcs in in serial and parallel */

void
enforce_bc_par (float **domain_ptr, int rank, int i, int j)
{
  /* enforce bc's first */
  if (i == ((int) floor (WIDTH / H / 2) - 1) && j == 0)
    {
      /* This is the heat source location */
      domain_ptr[j][i] = T_SRC0;
    }
  else if (i <= 0 || j <= 0 || i >= ((int) floor (WIDTH / H) - 1)
	   || j >= ((int) floor (HEIGHT / H) - 1))
    {
      /* All edges and beyond are set to 0.0 */
      domain_ptr[global_to_local (rank, j)][i] = 0.0;
    }
}

 /* returns appropriate values for requested i,j */

float
get_val_par (float *above_ptr, float **domain_ptr, float *below_ptr, int rank,
	     int i, int j)
{
  float ret_val;

  //MPI_Comm_size(MPI_COMM_WORLD,&p); 

  /* enforce bc's first */
  if (i == ((int) floor (WIDTH / H / 2) - 1) && j == 0)
    {
      /* This is the heat source location */
      ret_val = T_SRC0;
    }
  else if (i <= 0 || j <= 0 || i >= ((int) floor (WIDTH / H) - 1)
	   || j >= ((int) floor (HEIGHT / H) - 1))
    {
      /* All edges and beyond are set to 0.0 */
      ret_val = 0.0;
    }
  else
    {
      /* Else, return value for matrix supplied or ghost rows */
      if (j < get_start (rank))
	{
	  if (rank == ROOT)
	    {
	      /* not interested in above ghost row */
	      ret_val = 0.0;
	    }
	  else
	    {
	      ret_val = above_ptr[i];
	      /*printf("%d: Used ghost (%d,%d) row from above = %f\n",rank,i,j,above_ptr[i]);
	         fflush(stdout); */
	    }
	}
      else if (j > get_end (rank))
	{
	  if (rank == (p - 1))
	    {
	      /* not interested in below ghost row */
	      ret_val = 0.0;
	    }
	  else
	    {
	      ret_val = below_ptr[i];
	      /*printf("%d: Used ghost (%d,%d) row from below = %f\n",rank,i,j,below_ptr[i]);
	         fflush(stdout); */
	    }
	}
      else
	{
	  /* else, return the value in the domain asked for */
	  ret_val = domain_ptr[global_to_local (rank, j)][i];
	  /*printf("%d: Used real (%d,%d) row from self = %f\n",rank,i,global_to_local(rank,j),domain_ptr[global_to_local(rank,j)][i]);
	     fflush(stdout); */
	}
    }
  return ret_val;

}

 /* initialized domain to 0.0 - could be where grid file is read in */

void
init_domain (float **domain_ptr, int rank)
{
  int i, j, start, end, rows;
  start = get_start (rank);
  end = get_end (rank);
  rows = get_num_rows (rank);

  for (j = start; j < end; j++)
    {
      for (i = 0; i < (int) floor (WIDTH / H); i++)
	{
	  domain_ptr[j - start][i] = 0.0;
	}
    }
}

 /* computes start row for given PE */

int
get_start (int rank)
{
  /* computer row divisions to each proc */
  int per_proc, start_row, remainder;
  //MPI_Comm_size(MPI_COMM_WORLD,&p);  

  /* get initial whole divisor */
  per_proc = (int) floor (HEIGHT / H) / p;
  /* get number of remaining */
  remainder = (int) floor (HEIGHT / H) % p;
  /* there is a remainder, then it distribute it to the first "remainder" procs */
  if (rank < remainder)
    {
      start_row = rank * (per_proc + 1);
    }
  else
    {
      start_row = rank * (per_proc) + remainder;
    }
  return start_row;
}

 /* computes end row for given PE */

int
get_end (int rank)
{
  /* computer row divisions to each proc */
  int per_proc, remainder, end_row;
  //MPI_Comm_size(MPI_COMM_WORLD,&p);  
  per_proc = (int) floor (HEIGHT / H) / p;
  remainder = (int) floor (HEIGHT / H) % p;
  if (rank < remainder)
    {
      end_row = get_start (rank) + per_proc;
    }
  else
    {
      end_row = get_start (rank) + per_proc - 1;
    }
  return end_row;
}

 /* calcs number of rows for given PE */

int
get_num_rows (int rank)
{
  return 1 + get_end (rank) - get_start (rank);
}

int
global_to_local (int rank, int row)
{
  return row - get_start (rank);
}

  /* 
   * f - function that would be non zero if there was an internal heat source
   */

float
f (int i, int j)
{
  return 0.0;
}
