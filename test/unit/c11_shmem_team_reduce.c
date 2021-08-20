/*
 *  This test program is derived from a unit test created by Nick Park.
 *  The original unit test is a work of the U.S. Government and is not subject
 *  to copyright protection in the United States.  Foreign copyrights may
 *  apply.
 *
 *  Copyright (c) 2019 Intel Corporation. All rights reserved.
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

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <complex.h>
#include <math.h>
#include <stdbool.h>
#include <shmem.h>

#define MAX_NPES 32

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L

enum op { and = 0, or, xor, max, min, sum, prod };

const double FLOATING_POINT_TOLERANCE = 1e-6;

#define REDUCTION(OP) \
  do { ret = shmem_##OP##_reduce(SHMEM_TEAM_WORLD, dest, src, npes); } while (0)

#define is_floating_point(X) _Generic((X), \
              float: true, \
             double: true, \
        long double: true, \
     float _Complex: true, \
    double _Complex: true, \
            default: false \
)

#define INIT_SRC_BUFFER(TYPE)                                                  \
  do {                                                                         \
      for (int i = 0; i < MAX_NPES; i++) {                                     \
          src[i] = (TYPE)1ULL;                                                 \
      }                                                                        \
  } while (0)

#define CHECK_DEST_BUFFER(OP, TYPE, CORRECT_VAL)                               \
  do {                                                                         \
      for (int i = 0; i < npes; i++) {                                         \
          if (dest[i] != (TYPE)CORRECT_VAL) {                                  \
              printf("PE %i received incorrect value with "                    \
                     "TEST_SHMEM_REDUCE(%s, %s)\n", mype, #OP, #TYPE);         \
              rc = EXIT_FAILURE;                                               \
          }                                                                    \
      }                                                                        \
  } while (0)

#define CHECK_DEST_BUFFER_FP(OP, TYPE, CORRECT_VAL, TOLERANCE)                 \
  do {                                                                         \
      for (int i = 0; i < npes; i++) {                                         \
          if (fabsl(creal(dest[i]) - creal((TYPE)CORRECT_VAL)) > TOLERANCE) {  \
              printf("PE %i received incorrect real value with "               \
                     "TEST_SHMEM_REDUCE(%s, %s)\n", mype, #OP, #TYPE);         \
              rc = EXIT_FAILURE;                                               \
          }                                                                    \
          if (fabsl(cimag(dest[i]) - cimag((TYPE)CORRECT_VAL)) > TOLERANCE) {  \
                printf("PE %i received incorrect imaginary value with "        \
                       "TEST_SHMEM_REDUCE(%s, %s)\n", mype, #OP, #TYPE);       \
              rc = EXIT_FAILURE;                                               \
          }                                                                    \
      }                                                                        \
  } while (0)

#define TEST_SHMEM_REDUCE(OP, TYPE)                                            \
  do {                                                                         \
    static TYPE src[MAX_NPES];                                                 \
    static TYPE dest[MAX_NPES];                                                \
    int ret;                                                                   \
    const bool floating_point_val = is_floating_point((TYPE)0);                \
                                                                               \
    INIT_SRC_BUFFER(TYPE);                                                     \
                                                                               \
    REDUCTION(OP);                                                             \
                                                                               \
    if (ret != 0) {                                                            \
        printf("Reduction returned non-zero value (%i) on PE (%i) with "       \
               "TEST_SHMEM_REDUCE(%s, %s)\n", ret, mype, #OP, #TYPE);          \
        rc = EXIT_FAILURE;                                                     \
    }                                                                          \
                                                                               \
    shmem_barrier_all();                                                       \
                                                                               \
    switch (OP) {                                                              \
      case and:                                                                \
          CHECK_DEST_BUFFER(OP, TYPE, 1ULL);                                   \
          break;                                                               \
      case or:                                                                 \
          CHECK_DEST_BUFFER(OP, TYPE, 1ULL);                                   \
          break;                                                               \
      case xor:                                                                \
          CHECK_DEST_BUFFER(OP, TYPE, (TYPE)(npes % 2 ? 1ULL : 0ULL));         \
          break;                                                               \
      case max:                                                                \
          if (floating_point_val)                                              \
              CHECK_DEST_BUFFER_FP(OP, TYPE, 1ULL, FLOATING_POINT_TOLERANCE);  \
          else                                                                 \
              CHECK_DEST_BUFFER(OP, TYPE, 1ULL);                               \
          break;                                                               \
      case min:                                                                \
          if (floating_point_val)                                              \
              CHECK_DEST_BUFFER_FP(OP, TYPE, 1ULL, FLOATING_POINT_TOLERANCE);  \
          else                                                                 \
              CHECK_DEST_BUFFER(OP, TYPE, 1ULL);                               \
          break;                                                               \
      case sum:                                                                \
          if (floating_point_val)                                              \
              CHECK_DEST_BUFFER_FP(OP, TYPE, npes, FLOATING_POINT_TOLERANCE);  \
          else                                                                 \
              CHECK_DEST_BUFFER(OP, TYPE, npes);                               \
          break;                                                               \
      case prod:                                                               \
          if (floating_point_val)                                              \
              CHECK_DEST_BUFFER_FP(OP, TYPE, 1ULL, FLOATING_POINT_TOLERANCE);  \
          else                                                                 \
              CHECK_DEST_BUFFER(OP, TYPE, 1ULL);                               \
          break;                                                               \
      default:                                                                 \
          printf("Invalid operation (%d)\n", OP);                              \
          shmem_global_exit(1);                                                \
    }                                                                          \
  } while (0)

#else
#define TEST_SHMEM_REDUCE(OP, TYPE)
#endif


int main(void) {

    shmem_init();

    int rc = EXIT_SUCCESS;

    const int mype = shmem_my_pe();
    const int npes = shmem_n_pes();

    if (npes > MAX_NPES) {
        if (mype == 0)
            fprintf(stderr, "ERR - Requires less than %d PEs\n", MAX_NPES);
        shmem_global_exit(1);
    }

    TEST_SHMEM_REDUCE(and, unsigned char);
    TEST_SHMEM_REDUCE(and, short);
    TEST_SHMEM_REDUCE(and, unsigned short);
    TEST_SHMEM_REDUCE(and, int);
    TEST_SHMEM_REDUCE(and, unsigned int);
    TEST_SHMEM_REDUCE(and, long);
    TEST_SHMEM_REDUCE(and, unsigned long);
    TEST_SHMEM_REDUCE(and, long long);
    TEST_SHMEM_REDUCE(and, unsigned long long);

    TEST_SHMEM_REDUCE(or, unsigned char);
    TEST_SHMEM_REDUCE(or, short);
    TEST_SHMEM_REDUCE(or, unsigned short);
    TEST_SHMEM_REDUCE(or, int);
    TEST_SHMEM_REDUCE(or, unsigned int);
    TEST_SHMEM_REDUCE(or, long);
    TEST_SHMEM_REDUCE(or, unsigned long);
    TEST_SHMEM_REDUCE(or, long long);
    TEST_SHMEM_REDUCE(or, unsigned long long);

    TEST_SHMEM_REDUCE(xor, unsigned char);
    TEST_SHMEM_REDUCE(xor, short);
    TEST_SHMEM_REDUCE(xor, unsigned short);
    TEST_SHMEM_REDUCE(xor, int);
    TEST_SHMEM_REDUCE(xor, unsigned int);
    TEST_SHMEM_REDUCE(xor, long);
    TEST_SHMEM_REDUCE(xor, unsigned long);
    TEST_SHMEM_REDUCE(xor, long long);
    TEST_SHMEM_REDUCE(xor, unsigned long long);

    TEST_SHMEM_REDUCE(max, char);
    //TEST_SHMEM_REDUCE(max, signed char);
    //TEST_SHMEM_REDUCE(max, unsigned char);
    TEST_SHMEM_REDUCE(max, short);
    TEST_SHMEM_REDUCE(max, unsigned short);
    TEST_SHMEM_REDUCE(max, int);
    TEST_SHMEM_REDUCE(max, unsigned int);
    TEST_SHMEM_REDUCE(max, long);
    TEST_SHMEM_REDUCE(max, unsigned long);
    TEST_SHMEM_REDUCE(max, long long);
    TEST_SHMEM_REDUCE(max, unsigned long long);
    TEST_SHMEM_REDUCE(max, float);
    TEST_SHMEM_REDUCE(max, double);
    TEST_SHMEM_REDUCE(max, long double);

    TEST_SHMEM_REDUCE(min, char);
    //TEST_SHMEM_REDUCE(min, signed char);
    //TEST_SHMEM_REDUCE(min, unsigned char);
    TEST_SHMEM_REDUCE(min, short);
    TEST_SHMEM_REDUCE(min, unsigned short);
    TEST_SHMEM_REDUCE(min, int);
    TEST_SHMEM_REDUCE(min, unsigned int);
    TEST_SHMEM_REDUCE(min, long);
    TEST_SHMEM_REDUCE(min, unsigned long);
    TEST_SHMEM_REDUCE(min, long long);
    TEST_SHMEM_REDUCE(min, unsigned long long);
    TEST_SHMEM_REDUCE(min, float);
    TEST_SHMEM_REDUCE(min, double);
    TEST_SHMEM_REDUCE(min, long double);

    TEST_SHMEM_REDUCE(sum, char);
    //TEST_SHMEM_REDUCE(sum, signed char);
    //TEST_SHMEM_REDUCE(sum, unsigned char);
    TEST_SHMEM_REDUCE(sum, short);
    TEST_SHMEM_REDUCE(sum, unsigned short);
    TEST_SHMEM_REDUCE(sum, int);
    TEST_SHMEM_REDUCE(sum, unsigned int);
    TEST_SHMEM_REDUCE(sum, long);
    TEST_SHMEM_REDUCE(sum, unsigned long);
    TEST_SHMEM_REDUCE(sum, long long);
    TEST_SHMEM_REDUCE(sum, unsigned long long);
    TEST_SHMEM_REDUCE(sum, float);
    TEST_SHMEM_REDUCE(sum, double);
    TEST_SHMEM_REDUCE(sum, long double);
    TEST_SHMEM_REDUCE(sum, double _Complex);
    TEST_SHMEM_REDUCE(sum, float _Complex);

    TEST_SHMEM_REDUCE(prod, char);
    //TEST_SHMEM_REDUCE(prod, signed char);
    //TEST_SHMEM_REDUCE(prod, unsigned char);
    TEST_SHMEM_REDUCE(prod, short);
    TEST_SHMEM_REDUCE(prod, unsigned short);
    TEST_SHMEM_REDUCE(prod, int);
    TEST_SHMEM_REDUCE(prod, unsigned int);
    TEST_SHMEM_REDUCE(prod, long);
    TEST_SHMEM_REDUCE(prod, unsigned long);
    TEST_SHMEM_REDUCE(prod, long long);
    TEST_SHMEM_REDUCE(prod, unsigned long long);
    TEST_SHMEM_REDUCE(prod, float);
    TEST_SHMEM_REDUCE(prod, double);
    TEST_SHMEM_REDUCE(prod, long double);
    TEST_SHMEM_REDUCE(prod, double _Complex);
    TEST_SHMEM_REDUCE(prod, float _Complex);

    shmem_finalize();
    return rc;
}
