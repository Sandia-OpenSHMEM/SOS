/*   
     ctx_test 

     Updated by Joe Doyle joseph.doyle@intel.com 6/13/2016
     ver 0.1 Written by J. Robichaux joseph.robichaux@intel.com 6/5/2015 

*/
#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <omp.h>
#include <shmem.h>
#include <shmemx.h>


#define DBLSEC(x) ((double) x.tv_sec + 1.0e-9 * (double) x.tv_nsec) 

int main(int argc, char *argv[])
{
  /* Declarations */
  int thread_level = SHMEMX_THREAD_SERIALIZED;
  int tl;
  int buf_size = 1024;
  int niterations = 1; //10000;
  int ndomains = 1;
  int nctx = 1;
  int nthreads = 1;
  unsigned int bidirectional = 0;

  int c, i;
  int mype, npes, dest;
  unsigned int do_run;
  char **inbuf, **rbuf;
  int *flag;
  double *runtime, maxtime;
  int *permctx;
  struct timespec timer_start[32], timer_end[32] ;
  shmemx_domain_t *rdomains;
  shmemx_ctx_t *sctx;

  /* Command line args */
  while ((c = getopt(argc, argv, "123b:c:d:i:n:B")) != -1)
    switch (c)
    {
      case '1':
        thread_level = SHMEMX_THREAD_SINGLE;
        break;
      case '2':
        thread_level = SHMEMX_THREAD_SERIALIZED;
        break;
      case '3':
        thread_level = SHMEMX_THREAD_MULTIPLE;
        break;
      case 'b':
        buf_size = atoi(optarg);
        break;
      case 'c':
        nctx = atoi(optarg);
        break;
      case 'i':
        niterations = atoi(optarg);
        break;
      case 'd':
        ndomains = atoi(optarg);
        break;
      case 'n':
        i = atoi(optarg);
        omp_set_num_threads(i);
        break;
      case 'B':
        bidirectional = 1;
        break;
      default:
        abort();
    }

  /* Get number of threads. Will work event if set OMP_NUM_THREADS env */
/* #pragma omp master */
  nthreads = omp_get_max_threads();
  printf("%d threads\n",nthreads);

  if (ndomains > nctx) 
  {
    perror("Number of domains greater than number of contexts. Exiting\n");
    return -1;
  }

  if (nctx > nthreads) 
  {
    perror("Number of contexts greater than number of threads. Exiting\n");
    return -1;
  }

  shmemx_init_thread(thread_level,&tl);
  if(tl != thread_level) {
    fprintf(stderr,"Could not initialize with requested thread "
            "level %d: got %d\n",thread_level,tl);
    return -1;
  }
  /* start_pes(0); */

  npes = shmem_n_pes();
  mype = shmem_my_pe();

  /* Mallocs, Shmallocs, and initialization */
  flag = (int *) shmem_malloc(sizeof(int));

  inbuf = (char **) malloc(sizeof(char *) * nthreads);
  for (i=0; i < nthreads; i++) 
    inbuf[i] = (char *) shmem_malloc(sizeof(char) * buf_size);

  rbuf = (char **) malloc(sizeof(char *) * nthreads);
  for (i=0; i < nthreads; i++) 
    rbuf[i] = (char *) shmem_malloc(sizeof(char) * buf_size);

  /* Initialize data in parallel so mem is affinitized */
#pragma omp parallel
  {
    int mythd = omp_get_thread_num();
    memset(inbuf[mythd], 1, buf_size);
    memset(rbuf[mythd], 0, buf_size);
  }

  rdomains = (shmemx_domain_t *) malloc(sizeof(shmemx_domain_t) * ndomains);
  sctx = (shmemx_ctx_t *) malloc(sizeof(shmemx_ctx_t) * nctx);
  permctx = (int *) malloc(sizeof(int) * nthreads);
  runtime = (double *) malloc(sizeof(double) * nthreads);

  for (i=0; i<nthreads; i++) permctx[i] = i % nctx;

  /* Set up destinations, for now assume that npes is even,
     first half of pes communicate
     */
  dest = (mype + npes/2) % npes;
  /* Initialize resource domains */
  shmemx_domain_create(thread_level, ndomains, rdomains);

  /* Create contexts */
/* #pragma omp for schedule(static,1) */
  for (int ii=0; ii<nctx; ii++) 
  {
    shmemx_ctx_create(SHMEMX_DOMAIN_DEFAULT, &sctx[ii]);
  }

  /* Timing loop */
  do_run = (mype < npes/2) || bidirectional;
  *flag = 0;
  shmem_barrier_all();
#pragma omp parallel
  {
    int n, p, mythd;
    mythd = omp_get_thread_num();
    p = permctx[mythd];
    if (do_run) 
    {
      clock_gettime(CLOCK_REALTIME, &timer_start[mythd]);
      for (n=0; n< niterations; n++) 
      {
        shmemx_ctx_putmem(inbuf[mythd], rbuf[mythd],
            buf_size, dest, sctx[p]);
        shmemx_ctx_quiet(sctx[p]);
      }
      clock_gettime(CLOCK_REALTIME, &timer_end[mythd]);
    }
  }

  /* Print output in PE order  */
  if (mype != 0) shmem_int_wait(flag,1);

  if (do_run) { 
    maxtime = 0.0;
    for (i=0; i<nthreads; i++) 
    {
      runtime[i] = (DBLSEC(timer_end[i]) - DBLSEC(timer_start[i]))/niterations;
      maxtime = (runtime[i] > maxtime) ?  runtime[i] : maxtime;
    }

    printf("pe %d of %d\n", mype, npes);
    printf("Numthreads : %d\n", nthreads);
    printf("Numdomains : %d\n", ndomains);
    printf("Thread_level : %d\n",thread_level);
    printf("Maxtime_per_it : %e\n",maxtime);
    for (i=0; i<nthreads; i++) 
      printf("t_it[%d] = %e\n",  i, runtime[i]);
  }

  if (mype != npes-1) shmem_int_inc(flag, mype+1);

  shmem_barrier_all();

  /* Wrap up , frees & destroys TBD */
  for (i=nthreads-1; i >= 0; i--) 
  {
    shmem_free(rbuf[i]);
    shmem_free(inbuf[i]);
  }
  free(rbuf); free(inbuf);
  /* shmem_free(flag); */
  shmem_free(flag);
  printf("finishing off pe=%d\n",mype);
  /* Wonder what would happen if domain destoyed before ctx? */
  for (i=nctx-1; i >= 0; i--) 
    shmemx_ctx_destroy(sctx[i]);
  free(sctx); 

  shmemx_domain_destroy(ndomains, rdomains);
  free(rdomains);

  free(runtime);
  free(permctx);

  /* Adios */
  shmem_finalize();
  return 0;
}
