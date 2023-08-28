/*
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 *  Copyright (c) 2017 Intel Corporation. All rights reserved.
 *  This software is available to you under the BSD license below:
 *
 *      Redistribution and use in source and binary forms, with or
 *      without modification, are permitted provided that the following
 *      conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
* to_all - exercise SHMEM max,min,or,prod,sum,or,xor_to_all() reduction calls.
*       Each reduction is invoked for all data types:
*           short, int, long, float, double, long double, long long.
*       Point being numerous SHMEM atomics and synchronizations in flight.
*       From OpenSHMEM_specification_v1.0-final doc:
*
* frank @ SystemFabric Works identified an interesting overflow issue in the
* prod_to_all test. In the presence of slightly larger PE counts (>=14),
* overflow is encountered in short, int and float, double and long double.
* The short and int both wrap correctly and are both uniformly wrong...uniformly
* being the salient point. float, double and long double all suffer from
* floating point rounding errors, hence the FP test results are ignored
* (assumed to pass)when FP rounding is encountered.
*
* usage: to_all {-amopsSv|h}
*   where:
*       -a  do not run and_to_all
*       -m  do not run min_to_all, max_to_all() always run.
*       -o  do not run or_to_all
*       -p  do not run prod_to_all
*       -s  do not run sum_to_all
*       -x  do not run xor_to_all
*       -S  Serialize *_to_all() calls with barriers.
*       -v  verbose(additional -v, more verbose)
*       -h  this text.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <complex.h>

#include <shmem.h>

#define Rprintf if (shmem_my_pe() == 0) printf
#define Rfprintf if (shmem_my_pe() == 0) fprintf
#define Vprintf if (Verbose > 1) printf

int sum_reduce(int me, int npes);
int min_reduce(int me, int npes);
int max_reduce(int me, int npes);
int prod_reduce(int me, int npes);
int and_reduce(int me, int npes);
int or_reduce(int me, int npes);
int xor_reduce(int me, int npes);

int Verbose;
int Serialize;
int Min, And, Sum, Prod, Or, Xor;
int Passed;

#define N 128

#define MAX(a, b) ((a) > (b)) ? (a) : (b)
#define WRK_SIZE MAX(N/2+1, SHMEM_REDUCE_MIN_WRKDATA_SIZE)

short src0[N], dst0[N];
int src1[N], dst1[N];
long src2[N], dst2[N];
float src3[N], dst3[N];
double src4[N], dst4[N];
long double src5[N], dst5[N];
long long src6[N], dst6[N];

/* bitwise reduction types */
unsigned short src7[N], dst7[N];
unsigned int src8[N], dst8[N];
unsigned long src9[N], dst9[N];
unsigned long long src10[N], dst10[N];

short expected_result0;
int expected_result1;
long expected_result2;
float expected_result3;
double expected_result4;
long double expected_result5;
long long expected_result6;

int ok[7];

int
max_reduce(int me, int npes)
{
    int i, j, pass=0;

    memset(ok,0,sizeof(ok));

    for (i = 0; i < N; i++) {
        src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] = src6[i]
            = me + i;
    }
    shmem_barrier_all();

    shmem_short_max_reduce(     SHMEM_TEAM_WORLD, dst0, src0, N);
    shmem_int_max_reduce(       SHMEM_TEAM_WORLD, dst1, src1, N);
    shmem_long_max_reduce(      SHMEM_TEAM_WORLD, dst2, src2, N);
    shmem_float_max_reduce(     SHMEM_TEAM_WORLD, dst3, src3, N);
    shmem_double_max_reduce(    SHMEM_TEAM_WORLD, dst4, src4, N);
    shmem_longdouble_max_reduce(SHMEM_TEAM_WORLD, dst5, src5, N);
    shmem_longlong_max_reduce(  SHMEM_TEAM_WORLD, dst6, src6, N);

    if (me == 0) {
        for (i = 0,j=-1; i < N; i++,j++) {
          if(dst0[i] != npes+j) ok[0] = 1;
          if(dst1[i] != npes+j) ok[1] = 1;
          if(dst2[i] != npes+j) ok[2] = 1;
          if(dst3[i] != npes+j) ok[3] = 1;
          if(dst4[i] != npes+j) ok[4] = 1;
          if(dst5[i] != npes+j) ok[5] = 1;
          if(dst6[i] != npes+j) ok[6] = 1;
        }

        if(ok[0]==1){
            printf("Reduction operation shmem_short_max_reduce: Failed\n");
        }
        else{
            Vprintf("Reduction operation shmem_short_max_reduce: Passed\n");
            pass++;
        }
        if(ok[1]==1){
            printf("Reduction operation shmem_int_max_reduce: Failed\n");
        }
        else{
            Vprintf("Reduction operation shmem_int_max_reduce: Passed\n");
            pass++;
        }
        if(ok[2]==1){
            printf("Reduction operation shmem_long_max_reduce: Failed\n");
        }
        else{
            Vprintf("Reduction operation shmem_long_max_reduce: Passed\n");
            pass++;
        }
        if(ok[3]==1){
            printf("Reduction operation shmem_float_max_reduce: Failed\n");
        }
        else{
            Vprintf("Reduction operation shmem_float_max_reduce: Passed\n");
            pass++;
        }
        if(ok[4]==1){
            printf("Reduction operation shmem_double_max_reduce: Failed\n");
        }
        else{
            Vprintf("Reduction operation shmem_double_max_reduce: Passed\n");
            pass++;
        }
        if(ok[5]==1){
          printf("Reduction operation shmem_longdouble_max_reduce: Failed\n");
        }
        else{
           Vprintf("Reduction operation shmem_longdouble_max_reduce: Passed\n");
           pass++;
        }
        if(ok[6]==1){
            printf("Reduction operation shmem_longlong_max_reduce: Failed\n");
        }
        else{
            Vprintf("Reduction operation shmem_longlong_max_reduce: Passed\n");
            pass++;
        }
        Vprintf("\n");
    }
    if (Serialize) shmem_barrier_all();

    return (pass == 7 ? 1 : 0);
}

int
min_reduce(int me, int npes)
{
    int i, pass=0;

    memset(ok,0,sizeof(ok));

    for (i = 0; i < N; i++) {
      src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] = src6[i]
        = me + i;
      dst0[i] = -9;
      dst1[i] = -9;
      dst2[i] = -9;
      dst3[i] = -9;
      dst4[i] = -9;
      dst5[i] = -9;
      dst6[i] = -9;
    }

    shmem_barrier_all();

    shmem_short_min_reduce(     SHMEM_TEAM_WORLD, dst0, src0, N);
    shmem_int_min_reduce(       SHMEM_TEAM_WORLD, dst1, src1, N);
    shmem_long_min_reduce(      SHMEM_TEAM_WORLD, dst2, src2, N);
    shmem_float_min_reduce(     SHMEM_TEAM_WORLD, dst3, src3, N);
    shmem_double_min_reduce(    SHMEM_TEAM_WORLD, dst4, src4, N);
    shmem_longdouble_min_reduce(SHMEM_TEAM_WORLD, dst5, src5, N);
    shmem_longlong_min_reduce(  SHMEM_TEAM_WORLD, dst6, src6, N);

    if(me == 0) {
      for (i = 0; i < N; i++) {
         if(dst0[i] != i) ok[0] = 1;
           if(dst1[i] != i) ok[1] = 1;
           if(dst2[i] != i) ok[2] = 1;
           if(dst3[i] != i) ok[3] = 1;
           if(dst4[i] != i) ok[4] = 1;
           if(dst5[i] != i) ok[5] = 1;
           if(dst6[i] != i) ok[6] = 1;
      }
      if(ok[0]==1){
        printf("Reduction operation shmem_short_min_reduce: Failed\n");
      }
      else{
        Vprintf("Reduction operation shmem_short_min_reduce: Passed\n");
        pass++;
        }
        if(ok[1]==1){
        printf("Reduction operation shmem_int_min_reduce: Failed\n");
        }
      else{
        Vprintf("Reduction operation shmem_int_min_reduce: Passed\n");
        pass++;
        }
        if(ok[2]==1){
        printf("Reduction operation shmem_long_min_reduce: Failed\n");
        }
      else{
        Vprintf("Reduction operation shmem_long_min_reduce: Passed\n");
        pass++;
        }
        if(ok[3]==1){
        printf("Reduction operation shmem_float_min_reduce: Failed\n");
        }
      else{
        Vprintf("Reduction operation shmem_float_min_reduce: Passed\n");
        pass++;
        }
        if(ok[4]==1){
        printf("Reduction operation shmem_double_min_reduce: Failed\n");
        }
      else{
        Vprintf("Reduction operation shmem_double_min_reduce: Passed\n");
        pass++;
        }
        if(ok[5]==1){
        printf("Reduction operation shmem_longdouble_min_reduce: Failed\n");
        }
      else{
        Vprintf("Reduction operation shmem_longdouble_min_reduce: Passed\n");
        pass++;
        }
        if(ok[6]==1){
        printf("Reduction operation shmem_longlong_min_reduce: Failed\n");
        }
      else{
        Vprintf("Reduction operation shmem_longlong_min_reduce: Passed\n");
        pass++;
        }
        Vprintf("\n");
    }
    if (Serialize) shmem_barrier_all();

    return (pass == 7 ? 1 : 0);
}


int
sum_reduce(int me, int npes)
{
  int i, pass=0;

  memset(ok,0,sizeof(ok));

  for (i = 0; i < N; i++) {
    src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] = src6[i] = me;
    dst0[i] = -9;
    dst1[i] = -9;
    dst2[i] = -9;
    dst3[i] = -9;
    dst4[i] = -9;
    dst5[i] = -9;
    dst6[i] = -9;
  }

  shmem_barrier_all();

  shmem_short_sum_reduce(     SHMEM_TEAM_WORLD, dst0, src0, N);
  shmem_int_sum_reduce(       SHMEM_TEAM_WORLD, dst1, src1, N);
  shmem_long_sum_reduce(      SHMEM_TEAM_WORLD, dst2, src2, N);
  shmem_float_sum_reduce(     SHMEM_TEAM_WORLD, dst3, src3, N);
  shmem_double_sum_reduce(    SHMEM_TEAM_WORLD, dst4, src4, N);
  shmem_longdouble_sum_reduce(SHMEM_TEAM_WORLD, dst5, src5, N);
  shmem_longlong_sum_reduce(  SHMEM_TEAM_WORLD, dst6, src6, N);

  if(me == 0) {
    for (i = 0; i < N; i++) {
      if(dst0[i] != (short) (npes * (npes-1)/2)) ok[0] = 1;
      if(dst1[i] != (int) (npes * (npes-1)/2)) ok[1] = 1;
      if(dst2[i] != (long) (npes * (npes-1)/2)) ok[2] = 1;
      if(dst3[i] != (float) (npes * (npes-1)/2)) ok[3] = 1;
      if(dst4[i] != (double) (npes * (npes-1)/2)) ok[4] = 1;
      if(dst5[i] != (long double) (npes * (npes-1)/2)) ok[5] = 1;
      if(dst6[i] != (long long) (npes * (npes-1)/2)) ok[6] = 1;
    }
    if(ok[0]==1){
      printf("Reduction operation shmem_short_sum_reduce: Failed\n");
    }
    else{
      Vprintf("Reduction operation shmem_short_sum_reduce: Passed\n");
      pass++;
    }
    if(ok[1]==1){
      printf("Reduction operation shmem_int_sum_reduce: Failed\n");
    }
    else{
      Vprintf("Reduction operation shmem_int_sum_reduce: Passed\n");
      pass++;
    }
    if(ok[2]==1){
      printf("Reduction operation shmem_long_sum_reduce: Failed\n");
    }
    else{
      Vprintf("Reduction operation shmem_long_sum_reduce: Passed\n");
      pass++;
    }
    if(ok[3]==1){
      printf("Reduction operation shmem_float_sum_reduce: Failed\n");
    }
    else{
      Vprintf("Reduction operation shmem_float_sum_reduce: Passed\n");
      pass++;
    }
    if(ok[4]==1){
      printf("Reduction operation shmem_double_sum_reduce: Failed\n");
    }
    else{
      Vprintf("Reduction operation shmem_double_sum_reduce: Passed\n");
      pass++;
    }
    if(ok[5]==1){
      printf("Reduction operation shmem_longdouble_sum_reduce: Failed\n");
    }
    else{
      Vprintf("Reduction operation shmem_longdouble_sum_reduce: Passed\n");
      pass++;
    }
    if(ok[6]==1){
      printf("Reduction operation shmem_longlong_sum_reduce: Failed\n");
    }
    else{
      Vprintf("Reduction operation shmem_longlong_sum_reduce: Passed\n");
      pass++;
    }
    Vprintf("\n"); fflush(stdout);
  }
    if (Serialize) shmem_barrier_all();

    return (pass == 7 ? 1 : 0);
}


int
prod_reduce(int me, int npes)
{
    int i, pass=0;
    int float_rounding_err=0;
    int double_rounding_err=0;
    int ldouble_rounding_err=0;

    memset(ok,0,sizeof(ok));

    for (i = 0; i < N; i++) {
        src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] = src6[i]
            = me + 1;
        dst0[i] = -9;
        dst1[i] = -9;
        dst2[i] = -9;
        dst3[i] = -9;
        dst4[i] = -9;
        dst5[i] = -9;
        dst6[i] = -9;
    }

    expected_result0 = expected_result1 = expected_result2 =
    expected_result6 = 1;
    expected_result3 = expected_result4 = expected_result5 = 1.0;


    for(i=1; i <= npes; i++) {
        expected_result0 *= i;
        expected_result1 *= i;
        expected_result2 *= i;
        expected_result3 *= (float)i;
        expected_result4 *= (double)i;
        if ((double)expected_result3 != expected_result4) {
            if (!float_rounding_err && Verbose > 2 && me == 0)
                printf("float_err @ npes %d\n",i);
            float_rounding_err = 1;
        }
        expected_result5 *= (long double)i;
        if ((long double)expected_result4 != expected_result5) {
            if (!double_rounding_err && Verbose > 2 && me == 0)
                printf("double_err @ npes %d\n",i);
            ldouble_rounding_err = double_rounding_err = 1;
        }
        expected_result6 *= i;
    }

    shmem_barrier_all();

    shmem_short_prod_reduce(     SHMEM_TEAM_WORLD, dst0, src0, N);
    shmem_int_prod_reduce(       SHMEM_TEAM_WORLD, dst1, src1, N);
    shmem_long_prod_reduce(      SHMEM_TEAM_WORLD, dst2, src2, N);
    shmem_float_prod_reduce(     SHMEM_TEAM_WORLD, dst3, src3, N);
    shmem_double_prod_reduce(    SHMEM_TEAM_WORLD, dst4, src4, N);
    shmem_longdouble_prod_reduce(SHMEM_TEAM_WORLD, dst5, src5, N);
    shmem_longlong_prod_reduce(  SHMEM_TEAM_WORLD, dst6, src6, N);

    if(me == 0) {
      for (i = 0; i < N; i++) {
        if(dst0[i] != expected_result0) ok[0] = 1;
        if(dst1[i] != expected_result1) ok[1] = 1;
        if(dst2[i] != expected_result2) ok[2] = 1;

        /* check for overflow */
        if(!float_rounding_err && dst3[i] != expected_result3) { ok[3] = 1;
            printf("dst3[%d]: %f, expected val: %f\n",i, dst3[i], expected_result3);
        }
        if(!double_rounding_err && dst4[i] != expected_result4) {ok[4] = 1;
            printf("dst4[%d]: %f, expected val: %f\n",i, dst4[i], expected_result4);
        }
        if(!ldouble_rounding_err && dst5[i] != expected_result5) {ok[5] = 1;
            printf("dst5[%d]: %Lf, expected val: %Lf T4 %f\n",i, dst5[i], expected_result5,dst4[i]);
        }
        if(dst6[i] != expected_result6) ok[6] = 1;
      }

      if(ok[0]==1)
        printf("Reduction operation shmem_short_prod_reduce: Failed\n");
      else {
        Vprintf("Reduction operation shmem_short_prod_reduce: Passed\n");
        pass++;
      }

      if(ok[1]==1)
        printf("Reduction operation shmem_int_prod_reduce: Failed\n");
      else {
        Vprintf("Reduction operation shmem_int_prod_reduce: Passed\n");
        pass++;
      }

      if(ok[2]==1)
        printf("Reduction operation shmem_long_prod_reduce: Failed\n");
      else {
        Vprintf("Reduction operation shmem_long_prod_reduce: Passed\n");
        pass++;
      }

      if(ok[3]==1)
        printf("Reduction operation shmem_float_prod_reduce: Failed\n");
      else {
        if (float_rounding_err) {
            Vprintf("Reduction operation shmem_float_prod_reduce: skipped due to float rounding error\n");
        }
        else {
            Vprintf("Reduction operation shmem_float_prod_reduce: Passed\n");
        }
        pass++;
      }

      if(ok[4]==1)
        printf("Reduction operation shmem_double_prod_reduce: Failed\n");
      else {
        if (double_rounding_err) {
            Vprintf("Reduction operation shmem_double_prod_reduce: skipped due to double rounding error\n");
        }
        else {
            Vprintf("Reduction operation shmem_double_prod_reduce: Passed\n");
        }
        pass++;
      }

      if(ok[5]==1)
        printf("Reduction operation shmem_longdouble_prod_reduce: Failed\n");
      else {
        if (double_rounding_err) {
            Vprintf("Reduction operation shmem_longdouble_prod_reduce: skipped due to long double rounding error\n");
        }
        else {
            Vprintf("Reduction operation shmem_longdouble_prod_reduce: Passed\n");
        }
        pass++;
      }

      if(ok[6]==1)
        printf("Reduction operation shmem_longlong_prod_reduce: Failed\n");
      else {
        Vprintf("Reduction operation shmem_longlong_prod_reduce: Passed\n");
        pass++;
      }
      Vprintf("\n");
    }
    if (Serialize) shmem_barrier_all();

    return (pass == 7 ? 1 : 0);
}


int
and_reduce(int me, int num_pes)
{
    int i, pass=0;

    memset(ok,0,sizeof(ok));

    for (i = 0; i < N; i++) {
        src0[i] = src1[i] = src2[i] = src6[i] = me;
        dst0[i] = dst1[i] = dst2[i] = dst6[i] = -9;
    }

    shmem_barrier_all();

    shmem_ushort_and_reduce(   SHMEM_TEAM_WORLD, dst7,  src7, N);
    shmem_uint_and_reduce(     SHMEM_TEAM_WORLD, dst8,  src8, N);
    shmem_ulong_and_reduce(    SHMEM_TEAM_WORLD, dst9,  src9, N);
    shmem_ulonglong_and_reduce(SHMEM_TEAM_WORLD, dst10, src10, N);

    if (me==0) {
      for (i = 0; i < N; i++) {
          if(dst7[i]  != 0) ok[0] = 1;
          if(dst8[i]  != 0) ok[1] = 1;
          if(dst9[i]  != 0) ok[2] = 1;
          if(dst10[i] != 0) ok[3] = 1;
      }

      if(ok[0]==1){
        printf("Reduction operation shmem_ushort_and_reduce: Failed\n");
      }
      else{
        Vprintf("Reduction operation shmem_ushort_and_reduce: Passed\n");
        pass++;
        }
        if(ok[1]==1){
        printf("Reduction operation shmem_uint_and_reduce: Failed\n");
        }
      else{
        Vprintf("Reduction operation shmem_uint_and_reduce: Passed\n");
        pass++;
        }
        if(ok[2]==1){
        printf("Reduction operation shmem_ulong_and_reduce: Failed\n");
        }
      else{
        Vprintf("Reduction operation shmem_ulong_and_reduce: Passed\n");
        pass++;
        }
        if(ok[3]==1){
        printf("Reduction operation shmem_ulonglong_and_reduce: Failed\n");
        }
      else{
        Vprintf("Reduction operation shmem_ulonglong_and_reduce: Passed\n");
        pass++;
        }
      Vprintf("\n"); fflush(stdout);
    }
    if (Serialize) shmem_barrier_all();

    return (pass == 4 ? 1 : 0);
}


int
or_reduce(int me, int npes)
{
    int i, pass=0;

    memset(ok,0,sizeof(ok));

    for (i = 0; i < N; i++) {
      src7[i]  = src8[i] = src9[i] = src10[i] = (me + 1)%4;
      dst7[i]  = -9;
      dst8[i]  = -9;
      dst9[i]  = -9;
      dst10[i] = -9;
    }

    shmem_barrier_all();

    shmem_ushort_or_reduce(   SHMEM_TEAM_WORLD, dst7,  src7, N);
    shmem_uint_or_reduce(     SHMEM_TEAM_WORLD, dst8,  src8, N);
    shmem_ulong_or_reduce(    SHMEM_TEAM_WORLD, dst9,  src9, N);
    shmem_ulonglong_or_reduce(SHMEM_TEAM_WORLD, dst10, src10, N);

    if (me==0) {
        for (i = 0; i < N; i++) {
            int expected = (npes == 1) ? 1 : 3;

            if(dst7[i]  != expected) ok[0] = 1;
            if(dst8[i]  != expected) ok[1] = 1;
            if(dst9[i]  != expected) ok[2] = 1;
            if(dst10[i] != expected) ok[6] = 1;
        }

        if(ok[0]==1)
            printf("Reduction operation shmem_ushort_or_reduce: Failed\n");
        else {
            Vprintf("Reduction operation shmem_ushort_or_reduce: Passed\n");
            pass++;
        }

        if(ok[1]==1)
            printf("Reduction operation shmem_uint_or_reduce: Failed\n");
        else {
            Vprintf("Reduction operation shmem_uint_or_reduce: Passed\n");
            pass++;
        }

        if(ok[2]==1)
            printf("Reduction operation shmem_ulong_or_reduce: Failed\n");
        else {
            Vprintf("Reduction operation shmem_ulong_or_reduce: Passed\n");
            pass++;
        }

        if(ok[6]==1)
            printf("Reduction operation shmem_ulonglong_or_reduce: Failed\n");
        else {
            Vprintf("Reduction operation shmem_ulonglong_or_reduce: Passed\n");
            pass++;
        }
        Vprintf("\n");
    }
    if (Serialize) shmem_barrier_all();

    return (pass == 4 ? 1 : 0);
}


int
xor_reduce(int me, int npes)
{
    int i, pass=0;
    int expected_result = ((int)(npes/2) % 2);

    memset(ok,0,sizeof(ok));

    for (i = 0; i < N; i++) {
        src7[i]  = src8[i] = src9[i] = src10[i] = me%2;
        dst7[i]  = -9;
        dst8[i]  = -9;
        dst9[i]  = -9;
        dst10[i] = -9;
    }

    shmem_barrier_all();

    shmem_ushort_xor_reduce(   SHMEM_TEAM_WORLD, dst7,  src7, N);
    shmem_uint_xor_reduce(     SHMEM_TEAM_WORLD, dst8,  src8, N);
    shmem_ulong_xor_reduce(    SHMEM_TEAM_WORLD, dst9,  src9, N);
    shmem_ulonglong_xor_reduce(SHMEM_TEAM_WORLD, dst10, src10, N);

    if (me==0) {
      for (i = 0; i < N; i++) {
          if(dst7[i]  != expected_result) ok[0] = 1;
          if(dst8[i]  != expected_result) ok[1] = 1;
          if(dst9[i]  != expected_result) ok[2] = 1;
          if(dst10[i] != expected_result) ok[6] = 1;
      }

      if(ok[0]==1)
          printf("Reduction operation shmem_ushort_xor_reduce: Failed\n");
      else {
          Vprintf("Reduction operation shmem_ushort_xor_reduce: Passed\n");
          pass++;
      }

      if(ok[1]==1)
          printf("Reduction operation shmem_uint_xor_reduce: Failed\n");
       else {
          Vprintf("Reduction operation shmem_uint_xor_reduce: Passed\n");
          pass++;
      }

       if(ok[2]==1)
          printf("Reduction operation shmem_ulong_xor_reduce: Failed\n");
        else {
          Vprintf("Reduction operation shmem_ulong_xor_reduce: Passed\n");
          pass++;
      }

        if(ok[6]==1)
          printf("Reduction operation shmem_ulonglong_xor_reduce: Failed\n");
        else {
          Vprintf("Reduction operation shmem_ulonglong_xor_reduce: Passed\n");
          pass++;
      }

      Vprintf("\n");
    }
    if (Serialize) shmem_barrier_all();

    return (pass == 4 ? 1 : 0);
}


int
main(int argc, char* argv[])
{
    int c, mype, num_pes, tests, passed;
    char *pgm;

    shmem_init();
    mype = shmem_my_pe();
    num_pes = shmem_n_pes();

    if ((pgm=strrchr(argv[0],'/'))) {
        pgm++;
    } else {
        pgm = argv[0];
    }

    while((c=getopt(argc,argv,"ampsSoxhv")) != -1) {
        switch(c) {
          case 'a':
            And++;  // do not run and_reduce
            break;
          case 'm':
            Min++;  // do not run min_reduce
            break;
          case 'o':
            Or++;  // do not run or_reduce
            break;
          case 'p':
            Prod++;  // do not run prod_reduce
            break;
          case 's':
            Sum++;  // do not run sum_reduce
            break;
          case 'x':
            Xor++;  // do not run xor_reduce
            break;
          case 'S':
            Serialize++;
            break;
          case 'v':
            Verbose++;
            break;
          case 'h':
          default:
            Rfprintf(stderr,"usage: %s {-v(verbose)|h(help)}\n",pgm);
            shmem_finalize();
            return 1;
        }
    }

    tests = passed = 0;

    shmem_barrier_all();

    passed += max_reduce(mype, num_pes);
    tests++;

    if (!Min) {
        passed += min_reduce(mype, num_pes);
        tests++;
    }

    if (!Sum) {
        passed += sum_reduce(mype, num_pes);
        tests++;
    }

    if (!And) {
        passed += and_reduce(mype, num_pes);
        tests++;
    }

    if (!Prod) {
        passed += prod_reduce(mype, num_pes);
        tests++;
    }

    if (!Or) {
        passed += or_reduce(mype, num_pes);
        tests++;
    }

    if (!Xor) {
        passed += xor_reduce(mype, num_pes);
        tests++;
    }

    c = 0;
    if (mype == 0) {
        if ((Verbose || tests != passed))
            fprintf(stderr,"to_all[%d] %d of %d tests passed\n",
                    mype,passed,tests);
        c = (tests == passed ? 0 : 1);
    }

    shmem_finalize();

    return c;
}
