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
/* Performance test for shmem_barrier*/

#include <stdio.h>
#include <shmem.h>
#include <sys/time.h>

#define NPES 4

long pSync[_SHMEM_BCAST_SYNC_SIZE];
int x = 10101;

int
main()
{
  int me, npes, src;
  int i,j;
  struct timeval start, end;
  long time_taken,start_time,end_time;

  for (i = 0; i < _SHMEM_BCAST_SYNC_SIZE; i += 1) {
    pSync[i] = _SHMEM_SYNC_VALUE;
  }

  start_pes(0);
  me = _my_pe();
  npes = _num_pes();
  src = me-1;
  time_taken = 0;

  for (i=0;i<10000;i++){
    if (me != 0) {
      shmem_int_p(&x,src*(i+1), me-1);
    }
    else
      shmem_int_p(&x,src*(i+1), npes-1);
    shmem_barrier_all();

    gettimeofday(&start, NULL);
    start_time = (start.tv_sec * 1000000.0) + start.tv_usec;

    shmem_barrier(0, 0, npes, pSync);

    gettimeofday(&end, NULL);
    end_time = (end.tv_sec * 1000000.0) + end.tv_usec;
    time_taken = time_taken + (end_time - start_time);

  }
  /*printf("%d: x = %d\n", me, x);*/
  if(me == 0)
    printf("Time required for a barrier, with %d PEs is %ld microseconds\n",npes,time_taken/10000);

  return 0;
}
