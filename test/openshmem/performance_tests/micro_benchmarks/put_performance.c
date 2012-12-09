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


/* Performance test for shmem_XX_put (latency and bandwidth) */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <shmem.h>
#include <sys/time.h>

long double time_taken;

long pSync[_SHMEM_REDUCE_SYNC_SIZE];
long double pWrk[_SHMEM_REDUCE_MIN_WRKDATA_SIZE];

//#define N_ELEMENTS 25600/*Data size chosen to be able to capture time required*/
  int
main(void)
{
  int i,j,k;
  int *target;
  int *source;
  int me, npes;
  int nxtpe;
  struct timeval start, end;
  long double start_time,end_time;

  int N_ELEMENTS = (4194304*2)/sizeof(int);

  start_pes(0);
  me = _my_pe();
  npes = _num_pes();

  for (i = 0; i < _SHMEM_BCAST_SYNC_SIZE; i += 1)
  {
    pSync[i] = _SHMEM_SYNC_VALUE;
  }
  nxtpe = (me+1)%npes;
  source = (int *) shmalloc( N_ELEMENTS * sizeof(*source) );
  target = (int *) shmalloc( N_ELEMENTS * sizeof(*target) );

  if(me == 0)
    printf("Put performance test results:\nSize (Bytes)\t\tTime (Microseconds)\t\tBandwidth (Bytes/Second)\n");

  for (i = 0; i < N_ELEMENTS; i += 1) {
    source[i] = i + 1;
    target[i] = -90;
  }
  shmem_barrier_all();

  /*For int put we take average of all the times realized by a pair of PEs, thus
   * reducing effects of physical location of PEs*/
  for (i=1;i<=N_ELEMENTS;i=i*2)
  {
    time_taken = 0;

    for(j=0;j<10000;j++){
      gettimeofday(&start, NULL);

      start_time = (start.tv_sec * 1000000.0) + start.tv_usec;

      shmem_int_put(target, source, i,nxtpe);

      gettimeofday(&end, NULL);

      end_time = (end.tv_sec * 1000000.0) + end.tv_usec;

      time_taken = time_taken + (end_time - start_time);

    }
    shmem_longdouble_sum_to_all(&time_taken, &time_taken,1, 0, 0, npes, pWrk, pSync);


    if(me == 0){
      time_taken = time_taken/(npes*10000); /*Average time across all PEs for one put*/
      if (i*sizeof(i) < 1048576)
        printf("%ld \t\t\t\t %lf\t\t\t\t %lf\n",i*sizeof(i),
               (double)time_taken,(double)((i*sizeof(i))/(time_taken*1000000.0)));
      else
        printf("%ld \t\t\t %lf\t\t\t\t %lf\n",i*sizeof(i),
               (double)time_taken,(double)((i*sizeof(i))/(time_taken*1000000.0)));

    }

  }
  shmem_barrier_all();

  shfree(target);
  shfree(source);
  return 0;
}
