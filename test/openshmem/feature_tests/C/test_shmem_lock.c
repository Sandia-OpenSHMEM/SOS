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

/* Tests shmem_set_lock, shmem_test_lock
 * and shmem_clear_lock calls*/

#include <stdio.h>
#include <stdlib.h>
#include <shmem.h>

long L = 0;
int x;

int
main(int argc, char **argv)
{
  int me, npes;
  int slp;
  int ret_val;
  int new_val;

  start_pes(0);
  me = _my_pe();
  npes = _num_pes();
  x=0;
  ret_val = -1;

  if(npes > 1){

    slp = 1;
    shmem_barrier_all();

    shmem_set_lock(&L);

    shmem_int_get(&new_val,&x,1,0);
    new_val++;
    shmem_int_put(&x,&new_val,1,0);  /* increment x on PE 0 */
    shmem_quiet(); 

    shmem_clear_lock(&L);

    shmem_barrier_all();
 
    if(me == 0){
      if(x == npes)
        printf("Test for set, and clear lock: Passed\n");
      else
        printf("Test for set, and clear lock: Failed\n");
      x=0;
    }
    shmem_barrier_all();

      do {
        ret_val = shmem_test_lock(&L);
      } while ( ret_val == 1 );

    shmem_int_get(&new_val,&x,1,0);
    new_val++;
    shmem_int_put(&x,&new_val,1,0);  /* increment x on PE 0 */
    shmem_quiet(); 

    shmem_clear_lock(&L);

    shmem_barrier_all();

    if(me == 0){
      if(x == npes)
        printf("Test for test lock: Passed\n");
      else
        printf("Test for test lock: Failed\n");

    }
    shmem_barrier_all();
  }
  else
    printf("Number of PEs must be > 1 to test locks, test skipped\n");
  return 0;
}
