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


/*Tests shmem_broadcast32 shmem_broadcast64 calls
 * PE 0 broadcasts to all other PEs
 * source and destination arrays are shmalloc-ed
 * */
#include <stdio.h>
#include <stdlib.h>

#include <shmem.h>

long pSync[_SHMEM_BCAST_SYNC_SIZE];

  int
main(void)
{
  int i,success32, success64;
  int *targ;
  int *src;
  long *target;
  long *source;
  int me, npes;

  start_pes(0);
  me = _my_pe();
  npes = _num_pes();
  success32 = 0;
  success64 = 0;

  if(npes > 1){
    for (i = 0; i < _SHMEM_BCAST_SYNC_SIZE; i += 1) {
      pSync[i] = _SHMEM_SYNC_VALUE;
    }

    /*Test shmem_broadcast32*/
    src = (int *) shmalloc( npes * sizeof(*src) );
    for (i = 0; i < npes; i += 1) {
      src[i] = i + 1;
    }

    targ = (int *) shmalloc( npes * sizeof(*targ) );
    for (i = 0; i < npes; i += 1) {
      targ[i] = -999;
    }


    shmem_barrier_all();

    shmem_broadcast32(targ, src, npes, 0, 0, 0, npes, pSync);


    if(me == 1){
      for (i = 0; i < npes; i++) {
        if( targ[i] != (i+1))
          success32=1;
      }
      if(success32==1)
        printf("Test shmem_broadcast32: Failed\n");
      else
        printf("Test shmem_broadcast32: Passed\n");
    }

    shmem_barrier_all();

    /*Test shmem_broadcast64*/

    source = (long *) shmalloc( npes * sizeof(*source) );
    for (i = 0; i < npes; i += 1) {
      source[i] = i + 1;
    }

    target = (long *) shmalloc( npes * sizeof(*target) );
    for (i = 0; i < npes; i += 1) {
      target[i] = -999;
    }


    shmem_barrier_all();

    shmem_broadcast64(target, source, npes, 0, 0, 0, npes, pSync);



    if(me == 1){
      for (i = 0; i < npes; i++) {
        if( target[i] != (i+1))
          success64=1;
      }
      if(success64==1)
        printf("Test shmem_broadcast64: Failed\n");
      else
        printf("Test shmem_broadcast64: Passed\n");
    }

    shfree(targ);
    shfree(src);
    shfree(target);
    shfree(source);
  }
  else{
    printf("Number of PEs must be > 1 to test broadcast, test skipped\n");
  }
  success32 = 0;
  success64 = 0;

  if(npes > 3){
    for (i = 0; i < _SHMEM_BCAST_SYNC_SIZE; i += 1) {
      pSync[i] = _SHMEM_SYNC_VALUE;
    }

    /*Test strided shmem_broadcast32*/
    src = (int *) shmalloc( npes * sizeof(*src) );
    for (i = 0; i < npes; i += 1) {
      src[i] = i + 1;
    }

    targ = (int *) shmalloc( npes * sizeof(*targ) );
    for (i = 0; i < npes; i += 1) {
      targ[i] = -999;
    }


    shmem_barrier_all();

    if(me%2==0)
	    shmem_broadcast32(targ, src, npes, 0, 0, 1, npes/2, pSync);


    if(me == 2){
	    for (i = 0; i < npes; i++) {
		    if( targ[i] != (i+1))
			    success32=1;
	    }
      if(success32==1)
        printf("Test strided shmem_broadcast32: Failed\n");
      else
        printf("Test strided shmem_broadcast32: Passed\n");
    }

    shmem_barrier_all();

    /*Test strided shmem_broadcast64*/

    source = (long *) shmalloc( npes * sizeof(*source) );
    for (i = 0; i < npes; i += 1) {
      source[i] = i + 1;
    }

    target = (long *) shmalloc( npes * sizeof(*target) );
    for (i = 0; i < npes; i += 1) {
      target[i] = -999;
    }


    shmem_barrier_all();
    if(me%2==0)
	    shmem_broadcast64(target, source, npes, 0, 0, 1, npes/2, pSync);


    if(me == 2){
	    for (i = 0; i < npes; i++) {
		    if( target[i] != (i+1))
			    success64=1;
      }
      if(success64==1)
        printf("Test strided shmem_broadcast64: Failed\n");
      else
        printf("Test strided shmem_broadcast64: Passed\n");
    }

    shfree(targ);
    shfree(src);
    shfree(target);
    shfree(source);
  }
  else{
    printf("Number of PEs must be > 3 to test broadcast, test skipped\n");
    return 0;
  }
}
