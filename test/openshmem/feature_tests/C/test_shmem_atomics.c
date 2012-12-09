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



/* Tests all atomics   
 * shmem_int_swap, shmem_float_swap, shmem_long_swap, shmem_double_swap, shmem_longlong_swap,
 * shmem_longlong_cswap, shmem_long_cswap, shmem_int_cswap,
 * shmem_long_fadd, shmem_int_fadd,  shmem_longlong_fadd,  
 * shmem_long_finc, shmem_int_finc, shmem_longlong_finc,
 */

#include <stdio.h>

#include <shmem.h>

int success1_p2 ;
int success2_p2 ;
int success3_p2 ;
int success4_p2 ;
int success5_p2 ;

  int
main()
{
  int me, npes;

  int *target1;
  float *target2;
  long *target3;
  double *target4;
  long long  *target5;

  int swapped_val1, new_val1;
  float swapped_val2, new_val2;
  long swapped_val3, new_val3;
  double swapped_val4, new_val4;
  long long  swapped_val5, new_val5;

  int success =1;
  int success1_p1 ;
  int success2_p1 ;
  int success3_p1 ;
  int success4_p1 ; 
  int success5_p1 ;

  start_pes(0);
  me = _my_pe();
  npes = _num_pes();

  shmem_barrier_all();

  /*Checks if there are atleast 2 executing PEs*/

  if(npes>1){

    target1 = (int*) shmalloc( sizeof(*target1) );
    target2 = (float *) shmalloc( sizeof(*target2) );
    target3 = (long *) shmalloc( sizeof(*target3) );
    target4 = (double *) shmalloc( sizeof(*target4) );
    target5 = (long long*) shmalloc( sizeof(*target5) );

    *target1 = *target2 = *target3 = *target4 = *target5 = me;
    new_val1 = new_val2 = new_val3 = new_val4 = new_val5 = me;
    success1_p1 = success1_p2 = success2_p1 = success2_p2 = success3_p1 = success3_p2 = success4_p1 = success4_p2 = success5_p1 = success5_p2 = -1;

    shmem_barrier_all();

    swapped_val1 = shmem_int_swap(target1, new_val1, (me + 1) % npes);
    swapped_val2 = shmem_float_swap(target2, new_val2, (me + 1) % npes);
    swapped_val3= shmem_long_swap(target3, new_val3, (me + 1) % npes);
    swapped_val4 = shmem_double_swap(target4, new_val4, (me + 1) % npes);
    swapped_val5 = shmem_longlong_swap(target5, new_val5, (me + 1) % npes);


    /* To validate the working of swap we need to check the value received at the PE that initiated the swap 
     *  as well as the target PE
     */

    if(me == 0){
      if(swapped_val1 == 1){
        success1_p1=1 ;
      }
      if(swapped_val2 == 1){
        success2_p1=1 ;
      }
      if(swapped_val3 == 1){
        success3_p1=1 ;
      }
      if(swapped_val4 == 1){
        success4_p1=1 ;
      }
      if(swapped_val5 == 1){
        success5_p1=1 ;
      }
    }

    if(me == 1){
      if(target1 == 0){
        shmem_int_put(&success1_p2, &success, 1, 0);
      }
      if(target2 == 0){
        shmem_int_put(&success2_p2, &success, 1, 0);
      }
      if(target3 == 0){
        shmem_int_put(&success3_p2, &success, 1, 0);
      }
      if(target4 == 0){
        shmem_int_put(&success4_p2, &success, 1, 0);
      }
      if(target5 == 0){
        shmem_int_put(&success5_p2, &success, 1, 0);
      }
    }

    shmem_barrier_all();

    if(me == 0){
      if(success1_p1 && success1_p2){
        printf("Test shmem_int_swap: Passed\n"); 
      }		
      else{
        printf("Test shmem_int_swap: Failed\n");
      }

      if(success2_p1 && success2_p2){
        printf("Test shmem_float_swap: Passed\n");  
      }
      else{
        printf("Test shmem_float_swap: Failed\n");
      }

      if(success3_p1 && success3_p2){
        printf("Test shmem_long_swap: Passed\n"); 
      }		
      else{
        printf("Test shmem_long_swap: Failed\n");
      }

      if(success4_p1 && success4_p2){
        printf("Test shmem_double_swap: Passed\n");  
      }
      else{
        printf("Test shmem_double_swap: Failed\n");
      }

      if(success5_p1 && success5_p2){
        printf("Test shmem_longlong_swap: Passed\n");  
      }
      else{
        printf("Test shmem_longlong_swap: Failed\n");
      }

    }
    shmem_barrier_all();



    /* Test conditional swaps
     * shmem_longlong_cswap, shmem_long_cswap, shmem_int_cswap,
     */

    *target1 =  *target3 =  *target5 = me;
    new_val1 =  new_val3 =  new_val5 = me;
    success1_p1 = success1_p2 =  success3_p1 = success3_p2 = success5_p1 = success5_p2 = -1;

    shmem_barrier_all();

    swapped_val1 = shmem_int_cswap(target1, me+1, (long)me, 1);
    swapped_val3 = shmem_long_cswap(target3, me+1, (long)me, 1);
    swapped_val5 = shmem_longlong_cswap(target5, me+1, (long)me, 1);


    /* To validate the working of conditionalswap we need to check the value received at the PE that initiated 
     * the conditional swap as well as the target PE
     */

    if(me == 0){
      if(swapped_val1 == 1){
        success1_p1=1 ;
      }

      if(swapped_val3 == 1){
        success3_p1=1 ;
      }

      if(swapped_val5 == 1){
        success5_p1=1 ;
      }
    }

    if(me == 1){
      if(*target1 == 0){
        shmem_int_put(&success1_p2, &success, 1, 0);
      }

      if(*target3 == 0){
        shmem_int_put(&success3_p2, &success, 1, 0);
      }

      if(*target5 == 0){
        shmem_int_put(&success5_p2, &success, 1, 0);
      }
    }

    shmem_barrier_all();

    if(me == 0){
      if(success1_p1 && success1_p2){
        printf("Test shmem_int_cswap: Passed\n"); 
      }		
      else{
        printf("Test shmem_int_cswap: Failed\n");
      }

      if(success3_p1 && success3_p2){
        printf("Test shmem_long_cswap: Passed\n"); 
      }		
      else{
        printf("Test shmem_long_cswap: Failed\n");
      }

      if(success5_p1 && success5_p2){
        printf("Test shmem_longlong_cswap: Passed\n");  
      }
      else{
        printf("Test shmem_longlong_cswap: Failed\n");
      }

    }
    shmem_barrier_all();

    /* Test shmem_long_fadd, shmem_int_fadd,  shmem_longlong_fadd */

    *target1 =  *target3 =  *target5 = me;
    new_val1 =  new_val3 =  new_val5 = me;
    success1_p1 = success1_p2 =  success3_p1 = success3_p2 = success5_p1 = success5_p2 = -1;

    shmem_barrier_all();

    swapped_val1 = shmem_int_fadd(target1, 1, 0);
    swapped_val3 = shmem_long_fadd(target3, 1, 0);
    swapped_val5 = shmem_longlong_fadd(target5, 1, 0);


    /* To validate the working of fetch and add we need to check the old value received at the PE that initiated 
     * the fetch and increment as well as the new value on the target PE
     */

    if(me != 0){
      if(swapped_val1 == 0 ){
        success1_p1=1 ;
      }

      if(swapped_val3 == 0){
        success3_p1=1 ;
      }

      if(swapped_val5 == 0){
        success5_p1=1 ;
      }
    }

    if(me == 0){
      if(*target1 == npes-1){
        shmem_int_put(&success1_p2, &success, 1, npes-1);
      }

      if(*target3 == npes-1){
        shmem_int_put(&success3_p2, &success, 1, npes-1);
      }

      if(*target5 == npes-1){
        shmem_int_put(&success5_p2, &success, 1, npes-1);
      }
    }

    shmem_barrier_all();

    if(me == npes-1){
      if(success1_p1 && success1_p2){
        printf("Test shmem_int_fadd: Passed\n"); 
      }		
      else{
        printf("Test shmem_int_fadd: Failed\n");
      }

      if(success3_p1 && success3_p2){
        printf("Test shmem_long_fadd: Passed\n"); 
      }		
      else{
        printf("Test shmem_long_fadd: Failed\n");
      }

      if(success5_p1 && success5_p2){
        printf("Test shmem_longlong_fadd: Passed\n");  
      }
      else{
        printf("Test shmem_longlong_fadd: Failed\n");
      }

    }
    shmem_barrier_all();

    /* Test shmem_long_finc, shmem_int_finc, shmem_longlong_finc */

    *target1 =  *target3 =  *target5 = me;
    new_val1 =  new_val3 =  new_val5 = me;
    success1_p1 = success1_p2 =  success3_p1 = success3_p2 = success5_p1 = success5_p2 = -1;

    shmem_barrier_all();

    swapped_val1 = shmem_int_finc(target1, 0);
    swapped_val3 = shmem_long_finc(target3, 0);
    swapped_val5 = shmem_longlong_finc(target5, 0);


    /* To validate the working of fetch and increment we need to check the old value received at the PE that initiated 
     * the fetch and increment as well as the new value on the target PE
     */

    if(me != 0){
      if(swapped_val1 == 0 ){
        success1_p1=1 ;
      }

      if(swapped_val3 == 0){
        success3_p1=1 ;
      }

      if(swapped_val5 == 0){
        success5_p1=1 ;
      }
    }

    if(me == 0){
      if(*target1 == npes-1){
        shmem_int_put(&success1_p2, &success, 1, npes-1);
      }

      if(*target3 == npes-1){
        shmem_int_put(&success3_p2, &success, 1, npes-1);
      }

      if(*target5 == npes-1){
        shmem_int_put(&success5_p2, &success, 1, npes-1);
      }
    }

    shmem_barrier_all();

    if(me == npes-1){
      if(success1_p1 && success1_p2){
        printf("Test shmem_int_finc: Passed\n"); 
      }		
      else{
        printf("Test shmem_int_finc: Failed\n");
      }

      if(success3_p1 && success3_p2){
        printf("Test shmem_long_finc: Passed\n"); 
      }		
      else{
        printf("Test shmem_long_finc: Failed\n");
      }

      if(success5_p1 && success5_p2){
        printf("Test shmem_longlong_finc: Passed\n");  
      }
      else{
        printf("Test shmem_longlong_finc: Failed\n");
      }

    }
    shmem_barrier_all();

    shfree(target1);
    shfree(target2);
    shfree(target3);
    shfree(target4);
    shfree(target5);

  }
  else{
    printf("Number of PEs must be > 1 to test shmem atomics, test skipped\n");
  } 
  return 0;
}
