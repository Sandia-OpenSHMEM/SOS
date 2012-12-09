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
 * shmem_int_wait,  shmem_int_wait_until,  shmem_long_wait,
 * shmem_long_wait_until, shmem_longlong_wait,  shmem_longlong_wait_until, shmem_short_wait,
 * shmem_short_wait_until
 *
 * Tests conditational wait (shmem_long_wait) call
 * PE 1 waits for PE 0 to send something other than 9.
 * Send 4 9s to test wait condition, then some random values until != 9.
 */

#include <stdio.h>
#include <shmem.h>
#include <sys/time.h>
#include <stdlib.h>
#include <unistd.h>
int
main(void)
{
  int me, npes;
  long *dest;

  {
    time_t now;
    time(&now);
    srand( now + getpid() );
  }

  start_pes(0);
  me = _my_pe();
  npes = _num_pes();
  long src = 9L;

  if(npes>1){
  dest = (long *) shmalloc( sizeof(*dest) );

  *dest = 9L;
  shmem_barrier_all();

  if (me == 0) {
    int i;
    for (i = 0; i < 4; i += 1) {
      
      shmem_long_put(dest, &src, 1, 1);
    }
    for (i = 0; i < 10; i += 1) {
      long src = rand() % 10;
      shmem_long_put(dest, &src, 1, 1);
      if (src != 9L)
        break;
    }
  }

  shmem_barrier_all();

  if (me == 1) {
    shmem_long_wait(dest, 9L);
    printf("Test for conditional wait: Passed\n");
  }

  shmem_barrier_all();
  
  *dest = 9L;
  shmem_barrier_all();

  if (me == 0) {
    int i;
    for (i = 0; i < 4; i += 1) {
      long src = 9L;
      shmem_long_put(dest, &src, 1, 1);
    }
    for (i = 0; i < 10; i += 1) {
      long src = rand() % 10;
      shmem_long_put(dest, &src, 1, 1);
      if (src != 9L)
        break;
    }
  }

  shmem_barrier_all();

  if (me == 1) {
    shmem_long_wait_until(dest, SHMEM_CMP_NE, 9L);
    printf("Test for explicit conditional wait: Passed\n");
  }

  shmem_barrier_all();
  }
  else
    printf("Test for conditional wait requires more than 1 PE, test skipped\n");

  return 0;
}
