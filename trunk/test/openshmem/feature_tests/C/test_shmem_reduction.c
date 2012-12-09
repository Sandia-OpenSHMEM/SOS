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
 * Tests 
 * shmem_int_and_to_all, shmem_long_and_to_all, shmem_longlong_and_to_all, shmem_short_and_to_all,
 * shmem_double_max_to_all, shmem_float_max_to_all, shmem_int_max_to_all, shmem_long_max_to_all, shmem_longdouble_max_to_all, shmem_longlong_max_to_all, shmem_short_max_to_all, 
 * shmem_double_min_to_all, shmem_float_min_to_all, shmem_int_min_to_all, shmem_long_min_to_all, shmem_longdouble_min_to_all, shmem_longlong_min_to_all, shmem_short_min_to_all,
 * shmem_double_sum_to_all, shmem_float_sum_to_all, shmem_int_sum_to_all, shmem_long_sum_to_all, shmem_longdouble_sum_to_all, shmem_longlong_sum_to_all, shmem_short_sum_to_all,
 * shmem_double_prod_to_all, shmem_float_prod_to_all, shmem_int_prod_to_all, shmem_long_prod_to_all, shmem_longdouble_prod_to_all, shmem_longlong_prod_to_all, shmem_short_prod_to_all,
 * shmem_int_or_to_all, shmem_long_or_to_all, shmem_longlong_or_to_all, shmem_short_or_to_all,
 * shmem_int_xor_to_all, shmem_long_xor_to_all, shmem_longlong_xor_to_all, shmem_short_xor_to_all
 */

#include <stdio.h>
#include <string.h>

#include <shmem.h>

long pSync[_SHMEM_REDUCE_SYNC_SIZE];
long pSync1[_SHMEM_REDUCE_SYNC_SIZE];

#define N 3

short src0[N], dst0[N];
int src1[N], dst1[N];
long src2[N], dst2[N];
float src3[N], dst3[N];
double src4[N], dst4[N];
long double src5[N], dst5[N];
long long src6[N], dst6[N];

short expected_result0;
int expected_result1;
long expected_result2;
float expected_result3;
double expected_result4;
long double expected_result5;
long long expected_result6;


short pWrk0[_SHMEM_REDUCE_MIN_WRKDATA_SIZE];
int pWrk1[_SHMEM_REDUCE_MIN_WRKDATA_SIZE];
long pWrk2[_SHMEM_REDUCE_MIN_WRKDATA_SIZE];
float pWrk3[_SHMEM_REDUCE_MIN_WRKDATA_SIZE];
double pWrk4[_SHMEM_REDUCE_MIN_WRKDATA_SIZE];
long double pWrk5[_SHMEM_REDUCE_MIN_WRKDATA_SIZE];
long long pWrk6[_SHMEM_REDUCE_MIN_WRKDATA_SIZE];

int
main()
{
  int i,j;
  int me, npes;
  int success0, success1, success2, success3, success4, success5, success6;
  success0 = success1 = success2 = success3 = success4 = success5 = success6 = 0;

  int n_active = 0;

  start_pes(0);
  me = _my_pe();
  npes = _num_pes();

  for (i = 0; i < _SHMEM_REDUCE_SYNC_SIZE; i += 1) {
    pSync[i] = _SHMEM_SYNC_VALUE;
    pSync1[i] = _SHMEM_SYNC_VALUE;
  }

  for (i = 0; i < N; i += 1) {
    src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] = src6[i] = me + i;
  }

  shmem_barrier_all();

  if(npes>1){


    /*Test MAX: shmem_double_max_to_all, shmem_float_max_to_all, shmem_int_max_to_all, shmem_long_max_to_all, shmem_longdouble_max_to_all, shmem_longlong_max_to_all, shmem_short_max_to_all */

    shmem_short_max_to_all(dst0, src0, N, 0, 0, npes, pWrk0, pSync);
    shmem_int_max_to_all(dst1, src1, N, 0, 0, npes, pWrk1, pSync1);
    shmem_long_max_to_all(dst2, src2, N, 0, 0, npes, pWrk2, pSync);
    shmem_float_max_to_all(dst3, src3, N, 0, 0, npes, pWrk3, pSync1);
    shmem_double_max_to_all(dst4, src4, N, 0, 0, npes, pWrk4, pSync);
    shmem_longdouble_max_to_all(dst5, src5, N, 0, 0, npes, pWrk5, pSync1);
    shmem_longlong_max_to_all(dst6, src6, N, 0, 0, npes, pWrk6, pSync);


    if(me == 0){
      for (i = 0,j=-1; i < N; i++,j++) {
        if(dst0[i] != npes+j)
          success0 =1;
        if(dst1[i] != npes+j)
          success1 =1;
        if(dst2[i] != npes+j)
          success2 =1;
        if(dst3[i] != npes+j)
          success3 =1;
        if(dst4[i] != npes+j)
          success4 =1;
        if(dst5[i] != npes+j)
          success5 =1;
        if(dst6[i] != npes+j)
          success6 =1;
      }
      if(success0==1){
        printf("Reduction operation shmem_short_max_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_short_max_to_all: Passed\n");
      }
      if(success1==1){
        printf("Reduction operation shmem_int_max_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_int_max_to_all: Passed\n");
      }
      if(success2==1){
        printf("Reduction operation shmem_long_max_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_long_max_to_all: Passed\n");
      }
      if(success3==1){
        printf("Reduction operation shmem_float_max_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_float_max_to_all: Passed\n");
      }
      if(success4==1){
        printf("Reduction operation shmem_double_max_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_double_max_to_all: Passed\n");
      }
      if(success5==1){
        printf("Reduction operation shmem_longdouble_max_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_longdouble_max_to_all: Passed\n");
      }
      if(success6==1){
        printf("Reduction operation shmem_longlong_max_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_longlong_max_to_all: Passed\n");
      }

    }

    /*Test MIN: shmem_double_min_to_all, shmem_float_min_to_all, shmem_int_min_to_all, shmem_long_min_to_all, shmem_longdouble_min_to_all, shmem_longlong_min_to_all, shmem_short_min_to_all*/
    success0 = success1 = success2 = success3 = success4 = success5 = success6 = 0;


    for (i = 0; i < N; i += 1) {
      dst0[i] = -9;
      dst1[i] = -9;
      dst2[i] = -9;
      dst3[i] = -9;
      dst4[i] = -9;
      dst5[i] = -9;
      dst6[i] = -9;
    }

    shmem_barrier_all();

    shmem_short_min_to_all(dst0, src0, N, 0, 0, npes, pWrk0, pSync);
    shmem_int_min_to_all(dst1, src1, N, 0, 0, npes, pWrk1, pSync1);
    shmem_long_min_to_all(dst2, src2, N, 0, 0, npes, pWrk2, pSync);
    shmem_float_min_to_all(dst3, src3, N, 0, 0, npes, pWrk3, pSync1);
    shmem_double_min_to_all(dst4, src4, N, 0, 0, npes, pWrk4, pSync);
    shmem_longdouble_min_to_all(dst5, src5, N, 0, 0, npes, pWrk5, pSync1);
    shmem_longlong_min_to_all(dst6, src6, N, 0, 0, npes, pWrk6, pSync);


    if(me == 0){
      for (i = 0; i < N; i++) {
        if(dst0[i] != i)
          success0 =1;
        if(dst1[i] != i)
          success1 =1;
        if(dst2[i] != i)
          success2 =1;
        if(dst3[i] != i)
          success3 =1;
        if(dst4[i] != i)
          success4 =1;
        if(dst5[i] != i)
          success5 =1;
        if(dst6[i] != i)
          success6 =1;
      }
      if(success0==1){
        printf("Reduction operation shmem_short_min_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_short_min_to_all: Passed\n");
      }
      if(success1==1){
        printf("Reduction operation shmem_int_min_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_int_min_to_all: Passed\n");
      }
      if(success2==1){
        printf("Reduction operation shmem_long_min_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_long_min_to_all: Passed\n");
      }
      if(success3==1){
        printf("Reduction operation shmem_float_min_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_float_min_to_all: Passed\n");
      }
      if(success4==1){
        printf("Reduction operation shmem_double_min_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_double_min_to_all: Passed\n");
      }
      if(success5==1){
        printf("Reduction operation shmem_longdouble_min_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_longdouble_min_to_all: Passed\n");
      }
      if(success6==1){
        printf("Reduction operation shmem_longlong_min_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_longlong_min_to_all: Passed\n");
      }

    }

    /*Test SUM: shmem_double_sum_to_all, shmem_float_sum_to_all, shmem_int_sum_to_all, shmem_long_sum_to_all, shmem_longdouble_sum_to_all, shmem_longlong_sum_to_all, shmem_short_sum_to_all*/
    success0 = success1 = success2 = success3 = success4 = success5 = success6 = 0;
    for (i = 0; i < N; i += 1) {
      src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] = src6[i] = me;
    }
    for (i = 0; i < N; i += 1) {
      dst0[i] = -9;
      dst1[i] = -9;
      dst2[i] = -9;
      dst3[i] = -9;
      dst4[i] = -9;
      dst5[i] = -9;
      dst6[i] = -9;
    }
    shmem_barrier_all();

    shmem_short_sum_to_all(dst0, src0, N, 0, 0, npes, pWrk0, pSync);
    shmem_int_sum_to_all(dst1, src1, N, 0, 0, npes, pWrk1, pSync1);
    shmem_long_sum_to_all(dst2, src2, N, 0, 0, npes, pWrk2, pSync);
    shmem_float_sum_to_all(dst3, src3, N, 0, 0, npes, pWrk3, pSync1);
    shmem_double_sum_to_all(dst4, src4, N, 0, 0, npes, pWrk4, pSync);
    shmem_longdouble_sum_to_all(dst5, src5, N, 0, 0, npes, pWrk5, pSync1);
    shmem_longlong_sum_to_all(dst6, src6, N, 0, 0, npes, pWrk6, pSync);


    if(me == 0){
      for (i = 0; i < N; i++) {
        if(dst0[i] != (npes * (npes-1)/2))
          success0 =1;
        if(dst1[i] != (npes * (npes-1)/2))
          success1 =1;
        if(dst2[i] != (npes * (npes-1)/2))
          success2 =1;
        if(dst3[i] != (npes * (npes-1)/2))
          success3 =1;
        if(dst4[i] != (npes * (npes-1)/2))
          success4 =1;
        if(dst5[i] != (npes * (npes-1)/2))
          success5 =1;
        if(dst6[i] != (npes * (npes-1)/2))
          success6 =1;
      }
      if(success0==1){
        printf("Reduction operation shmem_short_sum_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_short_sum_to_all: Passed\n");
      }
      if(success1==1){
        printf("Reduction operation shmem_int_sum_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_int_sum_to_all: Passed\n");
      }
      if(success2==1){
        printf("Reduction operation shmem_long_sum_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_long_sum_to_all: Passed\n");
      }
      if(success3==1){
        printf("Reduction operation shmem_float_sum_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_float_sum_to_all: Passed\n");
      }
      if(success4==1){
        printf("Reduction operation shmem_double_sum_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_double_sum_to_all: Passed\n");
      }
      if(success5==1){
        printf("Reduction operation shmem_longdouble_sum_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_longdouble_sum_to_all: Passed\n");
      }
      if(success6==1){
        printf("Reduction operation shmem_longlong_sum_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_longlong_sum_to_all: Passed\n");
      }

    }

    /*Test AND: shmem_int_and_to_all, shmem_long_and_to_all, shmem_longlong_and_to_all, shmem_short_and_to_all,*/
    success0 = success1 = success2 = success6 = 0;
    for (i = 0; i < N; i += 1) {
      src0[i] = src1[i] = src2[i] = src6[i] = me;
    }
    for (i = 0; i < N; i += 1) {
      dst0[i] = -9;
      dst1[i] = -9;
      dst2[i] = -9;
      dst6[i] = -9;
    }

    shmem_barrier_all();

    shmem_short_and_to_all(dst0, src0, N, 0, 0, npes, pWrk0, pSync);
    shmem_int_and_to_all(dst1, src1, N, 0, 0, npes, pWrk1, pSync1);
    shmem_long_and_to_all(dst2, src2, N, 0, 0, npes, pWrk2, pSync);
    shmem_longlong_and_to_all(dst6, src6, N, 0, 0, npes, pWrk6, pSync1);


    if(me==0){
      for (i = 0; i < N; i++) {
        if(dst0[i] != 0)
          success0 =1;
        if(dst1[i] != 0)
          success1 =1;
        if(dst2[i] != 0)
          success2 =1;
        if(dst6[i] != 0)
          success6 =1;
      }
      if(success0==1){
        printf("Reduction operation shmem_short_and_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_short_and_to_all: Passed\n");
      }
      if(success1==1){
        printf("Reduction operation shmem_int_and_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_int_and_to_all: Passed\n");
      }
      if(success2==1){
        printf("Reduction operation shmem_long_and_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_long_and_to_all: Passed\n");
      }
      if(success6==1){
        printf("Reduction operation shmem_longlong_and_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_longlong_and_to_all: Passed\n");
      }

    }

    /*Test PROD: shmem_double_prod_to_all, shmem_float_prod_to_all, shmem_int_prod_to_all, shmem_long_prod_to_all, shmem_longdouble_prod_to_all, shmem_longlong_prod_to_all, shmem_short_prod_to_all, */

    success0 = success1 = success2 = success3 = success4 = success5 = success6 = 0;
    for (i = 0; i < N; i += 1) {
      src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] = src6[i] = me + 1;
    }
    for (i = 0; i < N; i += 1) {
      dst0[i] = -9;
      dst1[i] = -9;
      dst2[i] = -9;
      dst3[i] = -9;
      dst4[i] = -9;
      dst5[i] = -9;
      dst6[i] = -9;
    }

    expected_result0 = expected_result1 = expected_result2 = expected_result3 = expected_result4 = expected_result5 = expected_result6 =1;
    for(i=1;i<=npes;i++){
      expected_result0 = expected_result0 * i;
      expected_result1 = expected_result1 * i;
      expected_result2 = expected_result2 * i;
      expected_result3 = expected_result3 * i;
      expected_result4 = expected_result4 * i;
      expected_result5 = expected_result5 * i;
      expected_result6 = expected_result6 * i;
    }

    shmem_barrier_all();

    shmem_short_prod_to_all(dst0, src0, N, 0, 0, npes, pWrk0, pSync);
    shmem_int_prod_to_all(dst1, src1, N, 0, 0, npes, pWrk1, pSync1);
    shmem_long_prod_to_all(dst2, src2, N, 0, 0, npes, pWrk2, pSync);
    shmem_float_prod_to_all(dst3, src3, N, 0, 0, npes, pWrk3, pSync1);
    shmem_double_prod_to_all(dst4, src4, N, 0, 0, npes, pWrk4, pSync);
    shmem_longdouble_prod_to_all(dst5, src5, N, 0, 0, npes, pWrk5, pSync1);
    shmem_longlong_prod_to_all(dst6, src6, N, 0, 0, npes, pWrk6, pSync);


    if(me == 0){
      for (i = 0; i < N; i++) {
        /*printf("dst2[%d]: %ld, expected val: %ld\n",i, dst2[i], (long)expected_result2);*/
        if(dst0[i] != expected_result0)
          success0 =1;
        if(dst1[i] != expected_result1)
          success1 =1;
        if(dst2[i] != expected_result2)
          success2 =1;
        if(dst3[i] != expected_result3)
          success3 =1;
        if(dst4[i] != expected_result4)
          success4 =1;
        if(dst5[i] != expected_result5)
          success5 =1;
        if(dst6[i] != expected_result6)
          success6 =1;
      }
      if(success0==1){
        printf("Reduction operation shmem_short_prod_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_short_prod_to_all: Passed\n");
      }
      if(success1==1){
        printf("Reduction operation shmem_int_prod_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_int_prod_to_all: Passed\n");
      }
      if(success2==1){
        printf("Reduction operation shmem_long_prod_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_long_prod_to_all: Passed\n");
      }
      if(success3==1){
        printf("Reduction operation shmem_float_prod_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_float_prod_to_all: Passed\n");
      }
      if(success4==1){
        printf("Reduction operation shmem_double_prod_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_double_prod_to_all: Passed\n");
      }
      if(success5==1){
        printf("Reduction operation shmem_longdouble_prod_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_longdouble_prod_to_all: Passed\n");
      }
      if(success6==1){
        printf("Reduction operation shmem_longlong_prod_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_longlong_prod_to_all: Passed\n");
      }

    }

    /*Test OR: shmem_int_or_to_all, shmem_long_or_to_all, shmem_longlong_or_to_all, shmem_short_or_to_all,*/

    success0 = success1 = success2 = success6 = 0;
    for (i = 0; i < N; i += 1) {
      src0[i] = src1[i] = src2[i] = src6[i] = (me + 1)%4;
    }
    for (i = 0; i < N; i += 1) {
      dst0[i] = -9;
      dst1[i] = -9;
      dst2[i] = -9;
      dst6[i] = -9;
    }

    shmem_barrier_all();

    shmem_short_or_to_all(dst0, src0, N, 0, 0, npes, pWrk0, pSync);
    shmem_int_or_to_all(dst1, src1, N, 0, 0, npes, pWrk1, pSync1);
    shmem_long_or_to_all(dst2, src2, N, 0, 0, npes, pWrk2, pSync);
    shmem_longlong_or_to_all(dst6, src6, N, 0, 0, npes, pWrk6, pSync1);


    if(me==0){
      for (i = 0; i < N; i++) {
        if(dst0[i] != 3)
          success0 =1;
        if(dst1[i] != 3)
          success1 =1;
        if(dst2[i] != 3)
          success2 =1;
        if(dst6[i] != 3)
          success6 =1;
      }
      if(success0==1){
        printf("Reduction operation shmem_short_or_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_short_or_to_all: Passed\n");
      }
      if(success1==1){
        printf("Reduction operation shmem_int_or_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_int_or_to_all: Passed\n");
      }
      if(success2==1){
        printf("Reduction operation shmem_long_or_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_long_or_to_all: Passed\n");
      }
      if(success6==1){
        printf("Reduction operation shmem_longlong_or_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_longlong_or_to_all: Passed\n");
      }

    }

    /*Test XOR: shmem_int_xor_to_all, shmem_long_xor_to_all, shmem_longlong_xor_to_all, shmem_short_xor_to_all*/

    success0 = success1 = success2 = success6 = 0;
    for (i = 0; i < N; i += 1) {
      src0[i] = src1[i] = src2[i] = src6[i] = me%2;
    }
    for (i = 0; i < N; i += 1) {
      dst0[i] = -9;
      dst1[i] = -9;
      dst2[i] = -9;
      dst6[i] = -9;
    }
    int expected_result = ((int)(npes/2) % 2);


    shmem_barrier_all();

    shmem_short_xor_to_all(dst0, src0, N, 0, 0, npes, pWrk0, pSync);
    shmem_int_xor_to_all(dst1, src1, N, 0, 0, npes, pWrk1, pSync1);
    shmem_long_xor_to_all(dst2, src2, N, 0, 0, npes, pWrk2, pSync);
    shmem_longlong_xor_to_all(dst6, src6, N, 0, 0, npes, pWrk6, pSync1);

    if(me==0){
      for (i = 0; i < N; i++) {
        if(dst0[i] != expected_result)
          success0 =1;
        if(dst1[i] != expected_result)
          success1 =1;
        if(dst2[i] != expected_result)
          success2 =1;
        if(dst6[i] != expected_result)
          success6 =1;
      }
      if(success0==1){
        printf("Reduction operation shmem_short_xor_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_short_xor_to_all: Passed\n");
      }
      if(success1==1){
        printf("Reduction operation shmem_int_xor_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_int_xor_to_all: Passed\n");
      }
      if(success2==1){
        printf("Reduction operation shmem_long_xor_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_long_xor_to_all: Passed\n");
      }
      if(success6==1){
        printf("Reduction operation shmem_longlong_xor_to_all: Failed\n");
      }  
      else{
        printf("Reduction operation shmem_longlong_xor_to_all: Passed\n");
      }

    }
  }
  else
    printf("Number of PEs must be > 1 to test reduction operations: Tests skipped\n");

  if(npes > 2){

    int max_result;

    for (i = 0; i < N; i += 1) {
      src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] = src6[i] = me;
    }

    if(npes%2==0){
      n_active=npes/2;
      max_result = npes-2;
    }
    else{
      n_active= (npes+1)/2;
      max_result = npes-1;
    }

    /*Test strided MAX: shmem_double_max_to_all, shmem_float_max_to_all, shmem_int_max_to_all, shmem_long_max_to_all, shmem_longdouble_max_to_all, shmem_longlong_max_to_all, shmem_short_max_to_all */
    shmem_barrier_all();

    if(me%2 == 0){
      shmem_short_max_to_all(dst0, src0, N, 0, 1, n_active, pWrk0, pSync);
      shmem_int_max_to_all(dst1, src1, N, 0, 1, n_active, pWrk1, pSync1);
      shmem_long_max_to_all(dst2, src2, N, 0, 1, n_active, pWrk2, pSync);
      shmem_float_max_to_all(dst3, src3, N, 0, 1, n_active, pWrk3, pSync1);
      shmem_double_max_to_all(dst4, src4, N, 0, 1, n_active, pWrk4, pSync);
      shmem_longdouble_max_to_all(dst5, src5, N, 0, 1, n_active, pWrk5, pSync1);
      shmem_longlong_max_to_all(dst6, src6, N, 0, 1, n_active, pWrk6, pSync);
    }

    if(me == 0){
      for (i = 0,j=-1; i < N; i++,j++) {
        if(dst0[i] != max_result)
          success0 =1;
        if(dst1[i] != max_result)
          success1 =1;
        if(dst2[i] != max_result)
          success2 =1;
        if(dst3[i] != max_result)
          success3 =1;
        if(dst4[i] != max_result)
          success4 =1;
        if(dst5[i] != max_result)
          success5 =1;
        if(dst6[i] != max_result)
          success6 =1;
      }
      if(success0==1){
        printf("Strided reduction operation shmem_short_max_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_short_max_to_all: Passed\n");
      }
      if(success1==1){
        printf("Strided reduction operation shmem_int_max_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_int_max_to_all: Passed\n");
      }
      if(success2==1){
        printf("Strided reduction operation shmem_long_max_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_long_max_to_all: Passed\n");
      }
      if(success3==1){
        printf("Strided reduction operation shmem_float_max_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_float_max_to_all: Passed\n");
      }
      if(success4==1){
        printf("Strided reduction operation shmem_double_max_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_double_max_to_all: Passed\n");
      }
      if(success5==1){
        printf("Strided reduction operation shmem_longdouble_max_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_longdouble_max_to_all: Passed\n");
      }
      if(success6==1){
        printf("Strided reduction operation shmem_longlong_max_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_longlong_max_to_all: Passed\n");
      }

    }

    /*Test MIN: shmem_double_min_to_all, shmem_float_min_to_all, shmem_int_min_to_all, shmem_long_min_to_all, shmem_longdouble_min_to_all, shmem_longlong_min_to_all, shmem_short_min_to_all*/
    success0 = success1 = success2 = success3 = success4 = success5 = success6 = 0;

    for (i = 0; i < N; i += 1) {
      src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] = src6[i] = me ;
    }

    for (i = 0; i < N; i += 1) {
      dst0[i] = -9;
      dst1[i] = -9;
      dst2[i] = -9;
      dst3[i] = -9;
      dst4[i] = -9;
      dst5[i] = -9;
      dst6[i] = -9;
    }

    shmem_barrier_all();

    if(me%2==0){
    shmem_short_min_to_all(dst0, src0, N, 0, 1, n_active, pWrk0, pSync);
    shmem_int_min_to_all(dst1, src1, N, 0, 1, n_active, pWrk1, pSync1);
    shmem_long_min_to_all(dst2, src2, N, 0, 1, n_active, pWrk2, pSync);
    shmem_float_min_to_all(dst3, src3, N, 0, 1, n_active, pWrk3, pSync1);
    shmem_double_min_to_all(dst4, src4, N, 0, 1, n_active, pWrk4, pSync);
    shmem_longdouble_min_to_all(dst5, src5, N, 0, 1, n_active, pWrk5, pSync1);
    shmem_longlong_min_to_all(dst6, src6, N, 0, 1, n_active, pWrk6, pSync);
    }

    if(me == 0){
      for (i = 0; i < N; i++) {
        if(dst0[i] != 0)
          success0 =1;
        if(dst1[i] != 0)
          success1 =1;
        if(dst2[i] != 0)
          success2 =1;
        if(dst3[i] != 0)
          success3 =1;
        if(dst4[i] != 0)
          success4 =1;
        if(dst5[i] != 0)
          success5 =1;
        if(dst6[i] != 0)
          success6 =1;
      }
      if(success0==1){
        printf("Stridrd reduction operation shmem_short_min_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_short_min_to_all: Passed\n");
      }
      if(success1==1){
        printf("Strided reduction operation shmem_int_min_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_int_min_to_all: Passed\n");
      }
      if(success2==1){
        printf("Strided reduction operation shmem_long_min_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_long_min_to_all: Passed\n");
      }
      if(success3==1){
        printf("Strided reduction operation shmem_float_min_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_float_min_to_all: Passed\n");
      }
      if(success4==1){
        printf("Strided reduction operation shmem_double_min_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_double_min_to_all: Passed\n");
      }
      if(success5==1){
        printf("Strided reduction operation shmem_longdouble_min_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_longdouble_min_to_all: Passed\n");
      }
      if(success6==1){
        printf("Strided reduction operation shmem_longlong_min_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_longlong_min_to_all: Passed\n");
      }

    }

    /*Test SUM: shmem_double_sum_to_all, shmem_float_sum_to_all, shmem_int_sum_to_all, shmem_long_sum_to_all, shmem_longdouble_sum_to_all, shmem_longlong_sum_to_all, shmem_short_sum_to_all*/
    success0 = success1 = success2 = success3 = success4 = success5 = success6 = 0;
    for (i = 0; i < N; i += 1) {
      src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] = src6[i] = i;
    }
    for (i = 0; i < N; i += 1) {
      dst0[i] = -9;
      dst1[i] = -9;
      dst2[i] = -9;
      dst3[i] = -9;
      dst4[i] = -9;
      dst5[i] = -9;
      dst6[i] = -9;
    }
    shmem_barrier_all();

    if(me%2==0){
      shmem_short_sum_to_all(dst0, src0, N, 0, 1, n_active, pWrk0, pSync);
      shmem_int_sum_to_all(dst1, src1, N, 0, 1, n_active, pWrk1, pSync1);
      shmem_long_sum_to_all(dst2, src2, N, 0, 1, n_active, pWrk2, pSync);
      shmem_float_sum_to_all(dst3, src3, N, 0, 1, n_active, pWrk3, pSync1);
      shmem_double_sum_to_all(dst4, src4, N, 0, 1, n_active, pWrk4, pSync);
      shmem_longdouble_sum_to_all(dst5, src5, N, 0, 1, n_active, pWrk5, pSync1);
      shmem_longlong_sum_to_all(dst6, src6, N, 0, 1, n_active, pWrk6, pSync);
    }


    if(me == 0){
      for (i = 0; i < N; i++) {
        if(dst0[i] != (n_active * i))
          success0 =1;
        if(dst1[i] != (n_active * i))
          success1 =1;
        if(dst2[i] != (n_active * i))
          success2 =1;
        if(dst3[i] != (n_active * i))
          success3 =1;
        if(dst4[i] != (n_active * i))
          success4 =1;
        if(dst5[i] != (n_active * i))
          success5 =1;
        if(dst6[i] != (n_active * i))
          success6 =1;
      }
      if(success0==1){
        printf("Strided reduction operation shmem_short_sum_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_short_sum_to_all: Passed\n");
      }
      if(success1==1){
        printf("Strided reduction operation shmem_int_sum_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_int_sum_to_all: Passed\n");
      }
      if(success2==1){
        printf("Strided reduction operation shmem_long_sum_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_long_sum_to_all: Passed\n");
      }
      if(success3==1){
        printf("Strided reduction operation shmem_float_sum_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_float_sum_to_all: Passed\n");
      }
      if(success4==1){
        printf("Strided reduction operation shmem_double_sum_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_double_sum_to_all: Passed\n");
      }
      if(success5==1){
        printf("Strided reduction operation shmem_longdouble_sum_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_longdouble_sum_to_all: Passed\n");
      }
      if(success6==1){
        printf("Strided reduction operation shmem_longlong_sum_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_longlong_sum_to_all: Passed\n");
      }

    }

    /*Test AND: shmem_int_and_to_all, shmem_long_and_to_all, shmem_longlong_and_to_all, shmem_short_and_to_all,*/
    success0 = success1 = success2 = success6 = 0;
    for (i = 0; i < N; i += 1) {
      src0[i] = src1[i] = src2[i] = src6[i] = me%2;
    }
    for (i = 0; i < N; i += 1) {
      dst0[i] = -9;
      dst1[i] = -9;
      dst2[i] = -9;
      dst6[i] = -9;
    }

    shmem_barrier_all();

    if(me%2==0){
      shmem_short_and_to_all(dst0, src0, N, 0, 1, n_active, pWrk0, pSync);
      shmem_int_and_to_all(dst1, src1, N, 0, 1, n_active, pWrk1, pSync1);
      shmem_long_and_to_all(dst2, src2, N, 0, 1, n_active, pWrk2, pSync);
      shmem_longlong_and_to_all(dst6, src6, N, 0, 1, n_active, pWrk6, pSync1);
    }

    if(me==0){
      for (i = 0; i < N; i++) {
        if(dst0[i] != 0)
          success0 =1;
        if(dst1[i] != 0)
          success1 =1;
        if(dst2[i] != 0)
          success2 =1;
        if(dst6[i] != 0)
          success6 =1;
      }
      if(success0==1){
        printf("Strided reduction operation shmem_short_and_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_short_and_to_all: Passed\n");
      }
      if(success1==1){
        printf("Strided reduction operation shmem_int_and_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_int_and_to_all: Passed\n");
      }
      if(success2==1){
        printf("Strided reduction operation shmem_long_and_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_long_and_to_all: Passed\n");
      }
      if(success6==1){
        printf("Strided reduction operation shmem_longlong_and_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_longlong_and_to_all: Passed\n");
      }

    }

    /*Test PROD: shmem_double_prod_to_all, shmem_float_prod_to_all, shmem_int_prod_to_all, shmem_long_prod_to_all, shmem_longdouble_prod_to_all, shmem_longlong_prod_to_all, shmem_short_prod_to_all, */

    success0 = success1 = success2 = success3 = success4 = success5 = success6 = 0;
    for (i = 0; i < N; i += 1) {
      src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] = src6[i] = 1;
    }
    for (i = 0; i < N; i += 1) {
      dst0[i] = -9;
      dst1[i] = -9;
      dst2[i] = -9;
      dst3[i] = -9;
      dst4[i] = -9;
      dst5[i] = -9;
      dst6[i] = -9;
    }

    shmem_barrier_all();

    if(me%2==0){
      shmem_short_prod_to_all(dst0, src0, N, 0, 1, n_active, pWrk0, pSync);
      shmem_int_prod_to_all(dst1, src1, N, 0, 1, n_active, pWrk1, pSync1);
      shmem_long_prod_to_all(dst2, src2, N, 0, 1, n_active, pWrk2, pSync);
      shmem_float_prod_to_all(dst3, src3, N, 0, 1, n_active, pWrk3, pSync1);
      shmem_double_prod_to_all(dst4, src4, N, 0, 1, n_active, pWrk4, pSync);
      shmem_longdouble_prod_to_all(dst5, src5, N, 0, 1, n_active, pWrk5, pSync1);
      shmem_longlong_prod_to_all(dst6, src6, N, 0, 1, n_active, pWrk6, pSync);
    }

    if(me == 0){
      for (i = 0; i < N; i++) {
       // printf("dst1[%d]:%d",i,dst1[i]);
        if(dst0[i] != 1)
          success0 =1;
        if(dst1[i] != 1)
          success1 =1;
        if(dst2[i] != 1)
          success2 =1;
        if(dst3[i] != 1)
          success3 =1;
        if(dst4[i] != 1)
          success4 =1;
        if(dst5[i] != 1)
          success5 =1;
        if(dst6[i] != 1)
          success6 =1;
      }
      if(success0==1){
        printf("Strided reduction operation shmem_short_prod_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_short_prod_to_all: Passed\n");
      }
      if(success1==1){
        printf("Strided reduction operation shmem_int_prod_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_int_prod_to_all: Passed\n");
      }
      if(success2==1){
        printf("Strided reduction operation shmem_long_prod_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_long_prod_to_all: Passed\n");
      }
      if(success3==1){
        printf("Strided reduction operation shmem_float_prod_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_float_prod_to_all: Passed\n");
      }
      if(success4==1){
        printf("Strided reduction operation shmem_double_prod_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_double_prod_to_all: Passed\n");
      }
      if(success5==1){
        printf("Strided reduction operation shmem_longdouble_prod_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_longdouble_prod_to_all: Passed\n");
      }
      if(success6==1){
        printf("Strided reduction operation shmem_longlong_prod_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_longlong_prod_to_all: Passed\n");
      }

    }

    /*Test OR: shmem_int_or_to_all, shmem_long_or_to_all, shmem_longlong_or_to_all, shmem_short_or_to_all,*/

    success0 = success1 = success2 = success6 = 0;
    for (i = 0; i < N; i += 1) {
      src0[i] = src1[i] = src2[i] = src6[i] = me%2;
    }
    for (i = 0; i < N; i += 1) {
      dst0[i] = -9;
      dst1[i] = -9;
      dst2[i] = -9;
      dst6[i] = -9;
    }


    shmem_barrier_all();

    if(me%2==0){
      shmem_short_or_to_all(dst0, src0, N, 0, 1, n_active, pWrk0, pSync);
      shmem_int_or_to_all(dst1, src1, N, 0, 1, n_active, pWrk1, pSync1);
      shmem_long_or_to_all(dst2, src2, N, 0, 1, n_active, pWrk2, pSync);
      shmem_longlong_or_to_all(dst6, src6, N, 0, 1, n_active, pWrk6, pSync1);
    }

    if(me==0){
      for (i = 0; i < N; i++) {
        if(dst0[i] != 0)
          success0 =1;
        if(dst1[i] != 0)
          success1 =1;
        if(dst2[i] != 0)
          success2 =1;
        if(dst6[i] != 0)
          success6 =1;
      }
      if(success0==1){
        printf("Strided reduction operation shmem_short_or_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_short_or_to_all: Passed\n");
      }
      if(success1==1){
        printf("Strided reduction operation shmem_int_or_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_int_or_to_all: Passed\n");
      }
      if(success2==1){
        printf("Strided reduction operation shmem_long_or_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_long_or_to_all: Passed\n");
      }
      if(success6==1){
        printf("Strided reduction operation shmem_longlong_or_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_longlong_or_to_all: Passed\n");
      }

    }

    /*Test XOR: shmem_int_xor_to_all, shmem_long_xor_to_all, shmem_longlong_xor_to_all, shmem_short_xor_to_all*/

    success0 = success1 = success2 = success6 = 0;
    for (i = 0; i < N; i += 1) {
      src0[i] = src1[i] = src2[i] = src6[i] = me%2;
    }
    for (i = 0; i < N; i += 1) {
      dst0[i] = -9;
      dst1[i] = -9;
      dst2[i] = -9;
      dst6[i] = -9;
    }
    int expected_result = 0;


    shmem_barrier_all();

    if(me%2==0){
      shmem_short_xor_to_all(dst0, src0, N, 0, 1,n_active, pWrk0, pSync);
      shmem_int_xor_to_all(dst1, src1, N, 0,1,n_active, pWrk1, pSync1);
      shmem_long_xor_to_all(dst2, src2, N, 0,1,n_active, pWrk2, pSync);
      shmem_longlong_xor_to_all(dst6, src6, N, 0,1,n_active, pWrk6, pSync1);
    }

    if(me==0){
    //  for(i=0;i<N;i++)
    //    printf("dst[%d]:%d\n",i,dst0[i]);
      for (i = 0; i < N; i++) {
        if(dst0[i] != expected_result)
          success0 =1;
        if(dst1[i] != expected_result)
          success1 =1;
        if(dst2[i] != expected_result)
          success2 =1;
        if(dst6[i] != expected_result)
          success6 =1;
      }
      if(success0==1){
        printf("Strided reduction operation shmem_short_xor_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_short_xor_to_all: Passed\n");
      }
      if(success1==1){
        printf("Strided reduction operation shmem_int_xor_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_int_xor_to_all: Passed\n");
      }
      if(success2==1){
        printf("Strided reduction operation shmem_long_xor_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_long_xor_to_all: Passed\n");
      }
      if(success6==1){
        printf("Strided reduction operation shmem_longlong_xor_to_all: Failed\n");
      }  
      else{
        printf("Strided reduction operation shmem_longlong_xor_to_all: Passed\n");
      }

    }
  }
  else
    printf("Number of PEs must be > 2 to test strided reduction operations: Tests skipped\n");

  return 0;
}
