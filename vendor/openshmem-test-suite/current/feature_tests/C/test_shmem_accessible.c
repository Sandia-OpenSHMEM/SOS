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



/* Test whether various types of variables are accessible
 * Test if all PEs are accessible
 */

#include <stdio.h>
#include <shmem.h>
  
static int
check_it(void *addr)
{
  return shmem_addr_accessible(addr, 1);
}

long global_target;
static int static_target;

int
main(int argc, char *argv[])
{
  long local_target;
  int *shm_target;
  char *msg = "Test Address Accessible: Passed";
  int me,npes,i;
  int pe_acc_success=0;

  start_pes(0);
  me = _my_pe();
  npes = _num_pes();

  shm_target = (int *) shmalloc(sizeof(int));

  shmem_barrier_all();

  if (me == 0) {

    if (! check_it(&global_target)) { /* long global: yes */
      printf("Test Global Address Accessible: Failed\n");
    }
    else{
      printf("Test Global Address Accessible: Passed\n");  
    }
    if (! check_it(&static_target)) { /* static int global: yes */
      printf("Test Static Global Address Accessible: Failed\n");
    }
    else{
      printf("Test Static Global Address Accessible: Passed\n");  
    }
    if (check_it(&local_target)) { /* main() stack: no  */
      printf("Test Stack Address Accessible: Failed\n");

    }
    else{
      printf("Test Stack Address Accessible: Passed\n");  
    }
    if (! check_it(shm_target)) { /* shmalloc: yes */

      printf("Test Shmalloc-ed Address Accessible: Failed\n");
    }
    else{
      printf("Test Shmalloc-ed Address Accessible: Passed\n");  
    }


    for(i=1;i<npes;i++){

      if(shmem_pe_accessible(i)!=1){
        pe_acc_success=1;
      }

    }
    if(pe_acc_success==1){
      printf("Test shmem_pe_accessible: Failed\n");
    }
    else{
      printf("Test shmem_pe_accessible: Passed\n");  
    }


  }

  shfree(shm_target);

  return 0;
}
