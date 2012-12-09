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


/*Tests shmem_barrier call*/

#include <stdio.h>

#include <shmem.h>


long pSync[_SHMEM_BCAST_SYNC_SIZE];
int x = 10101;

int
main()
{
  int me, npes;
  int i;

  start_pes(0);
  me = _my_pe();
  npes = _num_pes();

  for (i = 0; i < _SHMEM_BCAST_SYNC_SIZE; i += 1) {
    pSync[i] = _SHMEM_SYNC_VALUE;
  }

  if(npes > 1){

    shmem_int_p(&x, 4, (me+1)%npes);

    shmem_barrier_all();

    if(me==npes-1){
      if(x==4)
        printf("Test shmem_barrier_all: Passed\n");
      else
        printf("Test shmem_barrier_all: Failed\n");
    } 

    x=-9;
    shmem_barrier_all();

    /*NOTE: Since the specification does not require pending communication
     * requeste to have completed the test is not strictly correct and
     * even correct implementations may not pass this test. */
    
      if(me==0)
        shmem_int_p(&x, 4, 1);

      shmem_barrier(0, 0, npes, pSync);

      if(me==1){
        if(x==4)
          printf("Test shmem_barrier: Passed\n");
        else
          printf("Test shmem_barrier: Failed\n");
      } 
  }
  else{
    printf("Number of PEs must be > 1 to test barrier, test skipped\n");

  }  
  return 0;
}
