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
 * shmem_short_get, shmem_int_get, shmem_long_get, shmem_longdouble_get,
 * shmem_longlong_get, shmem_double_get, shmem_float_get,
 * shmem_getmem, shmem_get32, shmem_get64, shmem_get128
 * shmem_iput8, shmem_iput32, shmem_iput64, shmem_iput128
 * shmem_double_iput, shmem_float_iput, shmem_int_iput, shmem_long_iput, shmem_short_iput
 * TODO:shmem_complexf_get, shmem_complexd_get
 *
 *
 * All PEs get an array from right neighbor
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

  short dest1[N];
  int dest2[N];
  long dest3[N];
  long double dest4[N];
  long long dest5[N];
  double dest6[N];
  float dest7[N];
  char *dest8;
  short dest9;
  int dest10;
  long dest11;
  double dest12;
  float dest13;

  short *src1;
  int *src2;
  long *src3;
  long double *src4;
  long long *src5;
  double *src6;
  float *src7;
  char *src8;
  short *src9;
  int *src10;
  long *src11;
  double *src12;
  float *src13;


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
    dest8 = (char *)malloc(N*sizeof(char));

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
    dest9 = -9;
    dest10 = -9;
    dest11 = -9;
    dest12 = -9;
    dest13 = -9;


    src1 = (short *)shmalloc( N * sizeof(*src1) );
    src2 = (int *)shmalloc( N * sizeof(*src2) );
    src3 = (long *)shmalloc( N * sizeof(*src3) );
    src4 = (long double *)shmalloc( N * sizeof(*src4) );
    src5 = (long long*)shmalloc( N * sizeof(*src5) );
    src6 = (double *)shmalloc( N * sizeof(*src6) );
    src7 = (float *)shmalloc( N * sizeof(*src7) );
    src8 = (char *)shmalloc( 4 * sizeof(*src8) );
    src9 = (short *)shmalloc( sizeof(*src9) );
    src10 = (int *)shmalloc( sizeof(*src10) );
    src11 = (long *)shmalloc( sizeof(*src11) );
    src12 = (double *)shmalloc( sizeof(*src12) );
    src13 = (float *)shmalloc( sizeof(*src13) );

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
    *src9 = (short)me;
    *src10 = me;
    *src11 = (long)me;
    *src12 = (double)me;
    *src13 = (float)me;



    nextpe = (me + 1) % npes;

    /*Testing shmem_short_get, shmem_short_get, shmem_int_get, shmem_long_get, shmem_longdouble_get, shmem_longlong_get, shmem_double_get, shmem_float_get, shmem_getmem*/
    shmem_barrier_all();

    shmem_short_get(dest1, src1, N, nextpe);
    shmem_int_get(dest2, src2, N, nextpe);
    shmem_long_get(dest3, src3, N, nextpe);
    shmem_longdouble_get(dest4, src4, N, nextpe);
    shmem_longlong_get(dest5, src5, N, nextpe);
    shmem_double_get(dest6, src6, N, nextpe);
    shmem_float_get(dest7, src7, N, nextpe);
    shmem_getmem(dest8, src8, N*sizeof(char), nextpe);

    shmem_barrier_all();

    if(me == 0){
      for (i = 0; i < N; i += 1) {
        if(dest1[i] != ( 1)){
          success1=1;
        }
        if(dest2[i] != ( 1)){
          success2=1;
        }
        if(dest3[i] != ( 1)){
          success3=1;
        }
        if(dest4[i] != ( 1)){
          success4=1;
        }
        if(dest5[i] != ( 1)){
          success5=1;
        }
        if(dest6[i] != ( 1)){
          success6=1;
        }
        if(dest7[i] != ( 1)){
          success7=1;
        }
        if(dest8[i] != ( 1)){
          success8=1;
        }
      }

      if(success1==0)
        printf("Test shmem_short_get: Passed\n");  
      else
        printf("Test shmem_short_get: Failed\n");
      if(success2==0)
        printf("Test shmem_int_get: Passed\n");  
      else
        printf("Test shmem_int_get: Failed\n");
      if(success3==0)
        printf("Test shmem_long_get: Passed\n");  
      else
        printf("Test shmem_long_get: Failed\n");
      if(success4==0)
        printf("Test shmem_longdouble_get: Passed\n");  
      else
        printf("Test shmem_longdouble_get: Failed\n");
      if(success5==0)
        printf("Test shmem_longlong_get: Passed\n");  
      else
        printf("Test shmem_longlong_get: Failed\n");
      if(success6==0)
        printf("Test shmem_double_get: Passed\n");  
      else
        printf("Test shmem_double_get: Failed\n");
      if(success7==0)
        printf("Test shmem_float_get: Passed\n");  
      else
        printf("Test shmem_float_get: Failed\n");
      if(success8==0)
        printf("Test shmem_getmem: Passed\n");  
      else
        printf("Test shmem_getmem: Failed\n");

    }
    shmem_barrier_all();

    /*Testing shmem_get32, shmem_get64, shmem_get128 */
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

      shmem_get32(dest2, src2, N, nextpe);
      shmem_get64(dest3, src3, N, nextpe);
      shmem_get128(dest4, src4, N, nextpe);

      shmem_barrier_all();

      if(me == 0){
        for (i = 0; i < N; i += 1) {
          if(dest2[i] != ( 1)){
            success2=1;
          }
          if(dest3[i] != ( 1)){
            success3=1;
          }
          if(dest4[i] != ( 1)){
            success4=1;
          }
        }
        if(success2==0)
          printf("Test shmem_get32: Passed\n");  
        else
          printf("Test shmem_get32: Failed\n");

        if(success3==0)
          printf("Test shmem_get64: Passed\n");  
        else
          printf("Test shmem_get64: Failed\n");

        if(success4==0)
          printf("Test shmem_get128: Passed\n");  
        else
          printf("Test shmem_get128: Failed\n");
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

      shmem_get32(dest1, src1, N, nextpe);
      shmem_get64(dest2, src2, N, nextpe);
      shmem_get128(dest3, src3, N, nextpe);

      shmem_barrier_all();

      if(me == 0){
        for (i = 0; i < N; i += 1) {
          if(dest1[i] != ( 1)){
            success1=1;
          }
          if(dest2[i] != ( 1)){
            success2=1;
          }
          if(dest3[i] != ( 1)){
            success3=1;
          }

        }
        if(success1==0)
          printf("Test shmem_get32: Passed\n");  
        else
          printf("Test shmem_get32: Failed\n");
        if(success2==0)
          printf("Test shmem_get64: Passed\n");  
        else
          printf("Test shmem_get64: Failed\n");

        if(success3==0)
          printf("Test shmem_get128: Passed\n");  
        else
          printf("Test shmem_get128: Failed\n");	
      }
    }	
	
	/* Testing shmem_iget32, shmem_iget64, shmem_iget128 */
	shmem_barrier_all();
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

      shmem_iget32(dest2, src2, 1, 2, N/2, npes-1);
      shmem_iget64(dest3, src3, 1, 2, N/2, npes-1);
      shmem_iget128(dest4, src4, 1, 2, N/2, npes-1);

      shmem_barrier_all();

      if(me == 0){
        for (i = 0; i < N/2; i += 1) {
          if(dest2[i] != (npes-1)){
            success2=1;
          }
          if(dest3[i] != (npes-1)){
            success3=1;
          }
          if(dest4[i] != (npes-1)){
            success4=1;
          }
        }
        if(success2==0)
          printf("Test shmem_iget32: Passed\n");  
        else
          printf("Test shmem_iget32: Failed\n");

        if(success3==0)
          printf("Test shmem_iget64: Passed\n");  
        else
          printf("Test shmem_iget64: Failed\n");

        if(success4==0)
          printf("Test shmem_iget128: Passed\n");  
        else
          printf("Test shmem_iget128: Failed\n");
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

      shmem_iget32(dest1, src1, 1, 2, N/2, npes-1);
      shmem_iget64(dest2, src2, 1, 2, N/2, npes-1);
      shmem_iget128(dest3, src3, 1, 2, N/2, npes-1);

      shmem_barrier_all();

      if(me == 0){
        for (i = 0; i < N/2; i += 1) {
          if(dest1[i] != (npes-1)){
            success1=1;
          }
          if(dest2[i] != (npes-1)){
            success2=1;
          }
          if(dest3[i] != (npes-1)){
            success3=1;
          }

        }
        if(success1==0)
          printf("Test shmem_iget32: Passed\n");  
        else
          printf("Test shmem_iget32: Failed\n");
        if(success2==0)
          printf("Test shmem_iget64: Passed\n");  
        else
          printf("Test shmem_iget64: Failed\n");

        if(success3==0)
          printf("Test shmem_iget128: Passed\n");  
        else
          printf("Test shmem_iget128: Failed\n");	
      }
    }	
	
	/*Testing shmem_short_iget, shmem_int_iget, shmem_long_iget, shmem_double_iget, shmem_float_iget */
	for (i = 0; i < N; i += 1) {
	    dest1[i] = -9;
        dest2[i] = -9;
        dest3[i] = -9;
        dest6[i] = -9;
		dest7[i] = -9;
      }
      success1 = 0;
      success2 = 0;
      success3 = 0;
	  success6 = 0;
      success7 = 0;
      
    shmem_barrier_all();

    shmem_short_iget(dest1, src1, 1, 2, N/2, npes-1);
    shmem_int_iget(dest2, src2, 1, 2, N/2, npes-1);
    shmem_long_iget(dest3, src3, 1, 2, N/2, npes-1);
    shmem_double_iget(dest6, src6, 1, 2, N/2, npes-1);
    shmem_float_iget(dest7, src7, 1, 2, N/2, npes-1);
    
    shmem_barrier_all();

    if(me == 0){
      for (i = 0; i < N/2; i += 1) {
        if(dest1[i] != (npes-1)){
          success1=1;
        }
        if(dest2[i] != (npes-1)){
          success2=1;
        }
        if(dest3[i] != (npes-1)){
          success3=1;
        }
        if(dest6[i] != (npes-1)){
          success6=1;
        }
        if(dest7[i] != (npes-1)){
          success7=1;
        }
      }

      if(success1==0)
        printf("Test shmem_short_iget: Passed\n");  
      else
        printf("Test shmem_short_iget: Failed\n");
      if(success2==0)
        printf("Test shmem_int_iget: Passed\n");  
      else
        printf("Test shmem_int_iget: Failed\n");
      if(success3==0)
        printf("Test shmem_long_iget: Passed\n");  
      else
        printf("Test shmem_long_iget: Failed\n");
      if(success6==0)
        printf("Test shmem_double_iget: Passed\n");  
      else
        printf("Test shmem_double_iget: Failed\n");
      if(success7==0)
        printf("Test shmem_float_iget: Passed\n");  
      else
        printf("Test shmem_float_iget: Failed\n");
      
    }
   


    /* Testing shmem_double_g, shmem_float_g, shmem_int_g, shmem_long_g, shmem_short_g */
    shmem_barrier_all();

    dest9 = shmem_short_g(src9, nextpe);
    dest10 = shmem_int_g(src10, nextpe);
    dest11 = shmem_long_g(src11, nextpe);
    dest12 = shmem_double_g(src12, nextpe);
    dest13 = shmem_float_g(src13, nextpe);

    shmem_barrier_all();

    if(me == 0){
      if(dest9 == 1)
        printf("Test shmem_short_g: Passed\n");  
      else
        printf("Test shmem_short_g: Failed\n");
      if(dest10 == 1)
        printf("Test shmem_int_g: Passed\n");  
      else
        printf("Test shmem_int_g: Failed\n");
      if(dest11 == 1)
        printf("Test shmem_long_g: Passed\n");  
      else
        printf("Test shmem_long_g: Failed\n");
      if(dest12 == 1)
        printf("Test shmem_double_g: Passed\n");  
      else
        printf("Test shmem_double_g: Failed\n");
      if(dest13 == 1)
        printf("Test shmem_float_g: Passed\n");  
      else
        printf("Test shmem_float_g: Failed\n");


    }

    shmem_barrier_all();


    shfree(src1);
    shfree(src2);
    shfree(src3);
    shfree(src4);
    shfree(src5);
    shfree(src6);
    shfree(src7);
    shfree(src8);
  }
  else{
    printf("Number of PEs must be > 1 to test shmem get, test skipped\n");
  }
  return 0;
}
