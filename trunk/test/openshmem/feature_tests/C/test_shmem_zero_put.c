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
 * Calls tested
 * shmem_short_put, shmem_int_put, shmem_long_put, shmem_longdouble_put,
 * shmem_longlong_put, shmem_double_put, shmem_float_put,
 * TODO: shmem_complexf_put, shmem_complexd_put
 * shmem_putmem, shmem_put32, shmem_put64, shmem_put128
 * shmem_double_p, shmem_float_p, shmem_int_p, shmem_long_p, shmem_short_p
 *
 * All PEs put an array to right neighbor
 */

#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <shmem.h>

#define N 7

int
main(int argc, char **argv)
{
  int i,j;
  int nextpe;
  int me, npes;
  int success1,success2,success3, success4, success5, success6, success7, success8;

  short src1[N];
  int src2[N];
  long src3[N];
  long double src4[N];
  long long src5[N];
  double src6[N];
  float src7[N];
  char *src8;
  short src9;
  int src10;
  long src11;
  double src12;
  float src13;

  short *dest1;
  int *dest2;
  long *dest3;
  long double *dest4;
  long long *dest5;
  double *dest6;
  float *dest7;
  char *dest8;
  short *dest9;
  int *dest10;
  long *dest11;
  double *dest12;
  float *dest13;


  start_pes(0);
  me = _my_pe();
  npes = _num_pes();

  if(npes>1){

    success1 =0;
    success2 =0;
    success3 =0;
    success4 =0;
    success5 =0;
    success6 =0;
    success7 =0;
    success8 =0;
    src8 = (char *)malloc(N*sizeof(char));

    for (i = 0; i < N; i += 1) {
      src1[i] = (short)me;
      src2[i] = me;
      src3[i] = (long)me;
      src4[i] = (long double)me;
      src5[i] = (long long)me;
      src6[i] = (double)me;
      src7[i] = (float)me;
      src8[i] = (char)me;
    }
    src9 = (short)me;
    src10 = me;
    src11 =(long)me;
    src12 = (double)me;
    src13 = (float)me;


    dest1 = (short *)shmalloc( N * sizeof(*dest1) );
    dest2 = (int *)shmalloc( N * sizeof(*dest2) );
    dest3 = (long *)shmalloc( N * sizeof(*dest3) );
    dest4 = (long double *)shmalloc( N * sizeof(*dest4) );
    dest5 = (long long*)shmalloc( N * sizeof(*dest5) );
    dest6 = (double *)shmalloc( N * sizeof(*dest6) );
    dest7 = (float *)shmalloc( N * sizeof(*dest7) );
    dest8 = (char *)shmalloc( 4 * sizeof(*dest8) );
    dest9 = (short *)shmalloc( sizeof(*dest9) );
    dest10 = (int *)shmalloc( sizeof(*dest10) );
    dest11 = (long *)shmalloc( sizeof(*dest11) );
    dest12 = (double *)shmalloc( sizeof(*dest12) );
    dest13 = (float *)shmalloc( sizeof(*dest13) );

    for (i = 0; i < N; i += 1) {
      dest1[i] = -9;
      dest2[i] = -9;
      dest3[i] = -9;
      dest4[i] = -9;
      dest5[i] = -9;
      dest6[i] = -9;
      dest7[i] = -9.0;
      dest8[i] = -9;
    }
    *dest9 = -9;
    *dest10 = -9;
    *dest11 = -9;
    *dest12 = -9;
    *dest13 = -9.0;

    nextpe = (me + 1) % npes;

    /*Testing shmem_short_put, shmem_short_put, shmem_int_put, shmem_long_put, shmem_longdouble_put, shmem_longlong_put, shmem_double_put, shmem_float_put, shmem_putmem*/
    shmem_barrier_all();

    shmem_short_put(dest1, src1, 0, nextpe);
    shmem_int_put(dest2, src2, 0, nextpe);
    shmem_long_put(dest3, src3, 0, nextpe);
    shmem_longdouble_put(dest4, src4, 0, nextpe);
    shmem_longlong_put(dest5, src5, 0, nextpe);
    shmem_double_put(dest6, src6, 0, nextpe);
    shmem_float_put(dest7, src7, 0, nextpe);
    shmem_putmem(dest8, src8, 0, nextpe);

    shmem_barrier_all();

    if(me == 0){
      for (i = 0; i < N; i += 1) {
        if(dest1[i] != -9){
          success1=1;
        }
        if(dest2[i] != -9){
          success2=1;
        }
        if(dest3[i] != -9){
          success3=1;
        }
        if(dest4[i] != -9){
          success4=1;
        }
        if(dest5[i] != -9){
          success5=1;
        }
        if(dest6[i] != -9){
          success6=1;
        }
        if(dest7[i] != -9){
          success7=1;
        }
        if(dest8[i] != -9){
          success8=1;
        }
      }

      if(success1==0)
        printf("Test shmem_short_put of zero length: Passed\n");  
      else
        printf("Test shmem_short_put of zero length: Failed\n");
      if(success2==0)
        printf("Test shmem_int_put of zero length: Passed\n");  
      else
        printf("Test shmem_int_put of zero length: Failed\n");
      if(success3==0)
        printf("Test shmem_long_put of zero length: Passed\n");  
      else
        printf("Test shmem_long_put of zero length: Failed\n");
      if(success4==0)
        printf("Test shmem_longdouble_put of zero length: Passed\n");  
      else
        printf("Test shmem_longdouble_put of zero length: Failed\n");
      if(success5==0)
        printf("Test shmem_longlong_put of zero length: Passed\n");  
      else
        printf("Test shmem_longlong_put of zero length: Failed\n");
      if(success6==0)
        printf("Test shmem_double_put of zero length: Passed\n");  
      else
        printf("Test shmem_double_put of zero length: Failed\n");
      if(success7==0)
        printf("Test shmem_float_put of zero length: Passed\n");  
      else
        printf("Test shmem_float_put of zero length: Failed\n");
      if(success8==0)
        printf("Test shmem_putmem of zero length: Passed\n");  
      else
        printf("Test shmem_putmem of zero length: Failed\n");

    }
    shmem_barrier_all();

    /*Testing shmem_put32, shmem_put64, shmem_put128 */
    if(sizeof(int)==4){
      for (i = 0; i < N; i += 1) {
        dest2[i] = -9;
        dest3[i] = -9;
        dest4[i] = -9;
      }
      success2 = 0;
      success3 = 0;
      success4 = 0;

      shmem_barrier_all();

      shmem_put32(dest2, src2, 0, nextpe);
      shmem_put64(dest3, src3, 0, nextpe);
      shmem_put128(dest4, src4, 0, nextpe);

      shmem_barrier_all();

      if(me == 0){
        for (i = 0; i < N; i += 1) {
          if(dest2[i] != -9){
            success2=1;
          }
          if(dest3[i] != -9){
            success3=1;
          }
          if(dest4[i] != -9){
            success4=1;
          }
        }
        if(success2==0)
          printf("Test shmem_put32 of zero length: Passed\n");  
        else
          printf("Test shmem_put32 of zero length: Failed\n");

        if(success3==0)
          printf("Test shmem_put64 of zero length: Passed\n");  
        else
          printf("Test shmem_put64 of zero length: Failed\n");

        if(success4==0)
          printf("Test shmem_put128 of zero length: Passed\n");  
        else
          printf("Test shmem_put128 of zero length: Failed\n");
      }
    }
    else if(sizeof(int)==8){
      for (i = 0; i < N; i += 1) {
        dest1[i] = -9;
        dest2[i] = -9;
        dest3[i] = -9;
      }
      success1 = 0;
      success2 = 0;
      success3 = 0;

      shmem_barrier_all();

      shmem_put32(dest1, src1, 0, nextpe);
      shmem_put64(dest2, src2, 0, nextpe);
      shmem_put128(dest3, src3, 0, nextpe);

      shmem_barrier_all();

      if(me == 0){
        for (i = 0; i < N; i += 1) {
          if(dest1[i] != -9){
            success1=1;
          }
          if(dest2[i] != -9){
            success2=1;
          }
          if(dest3[i] != -9){
            success3=1;
          }

        }
        if(success1==0)
          printf("Test shmem_put32 of zero length: Passed\n");  
        else
          printf("Test shmem_put32 of zero length: Failed\n");
        if(success2==0)
          printf("Test shmem_put64 of zero length: Passed\n");  
        else
          printf("Test shmem_put64 of zero length: Failed\n");

        if(success3==0)
          printf("Test shmem_put128 of zero length: Passed\n");  
        else
          printf("Test shmem_put128 of zero length: Failed\n");	
      }
    }	


    shmem_barrier_all();

    shfree(dest1);
    shfree(dest2);
    shfree(dest3);
    shfree(dest4);
    shfree(dest5);
    shfree(dest6);
    shfree(dest7);
    shfree(dest8);
    shfree(dest9);
    shfree(dest10);
    shfree(dest11);
    shfree(dest12);
    shfree(dest13);

  }
  else{
    printf("Number of PEs must be > 1 to test shmem put of zero length, test skipped\n");
  }
  return 0;
}
