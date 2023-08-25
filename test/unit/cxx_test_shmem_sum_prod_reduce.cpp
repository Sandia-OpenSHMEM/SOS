/*
 *  This test program is derived from a unit test created by Nick Park.
 *  The original unit test is a work of the U.S. Government and is not subject
 *  to copyright protection in the United States.  Foreign copyrights may
 *  apply.
 *
 *  Copyright (c) 2021 Intel Corporation. All rights reserved.
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
#include <type_traits>

#define MAX_NPES 32

enum op { OP_SUM, OP_PROD };

const double FLOATING_POINT_TOLERANCE = 1e-6;

#define REDUCTION(OP, TYPE)                                                   \
    do {                                                                      \
        switch (OP) {                                                         \
            case OP_SUM:                                                      \
                ret = shmem_##TYPE##_sum_reduce(SHMEM_TEAM_WORLD, dest, src,  \
                                                npes);                        \
                break;                                                        \
            case OP_PROD:                                                     \
                ret = shmem_##TYPE##_prod_reduce(SHMEM_TEAM_WORLD, dest, src, \
                                                 npes);                       \
                break;                                                        \
            default:                                                          \
                printf("Invalid operation (%d)\n", OP);                       \
                shmem_global_exit(1);                                         \
        }                                                                     \
    } while (0)

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

#define TEST_SHMEM_REDUCE(OP, TYPENAME, TYPE)                                  \
  do {                                                                         \
    static TYPE src[MAX_NPES];                                                 \
    static TYPE dest[MAX_NPES];                                                \
    int ret;                                                                   \
    const bool floating_point_val = std::is_floating_point<TYPE>::value;       \
                                                                               \
    INIT_SRC_BUFFER(TYPE);                                                     \
    REDUCTION(OP, TYPENAME);                                                   \
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
      case OP_SUM:                                                             \
          if (floating_point_val)                                              \
              CHECK_DEST_BUFFER_FP(OP, TYPE, npes, FLOATING_POINT_TOLERANCE);  \
          else                                                                 \
              CHECK_DEST_BUFFER(OP, TYPE, npes);                               \
          break;                                                               \
      case OP_PROD:                                                            \
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

    TEST_SHMEM_REDUCE(OP_SUM, char, char);
    TEST_SHMEM_REDUCE(OP_SUM, schar, signed char);
    TEST_SHMEM_REDUCE(OP_SUM, short, short);
    TEST_SHMEM_REDUCE(OP_SUM, int, int);
    TEST_SHMEM_REDUCE(OP_SUM, long, long);
    TEST_SHMEM_REDUCE(OP_SUM, longlong, long long);
    TEST_SHMEM_REDUCE(OP_SUM, ptrdiff, ptrdiff_t);
    TEST_SHMEM_REDUCE(OP_SUM, uchar, unsigned char);
    TEST_SHMEM_REDUCE(OP_SUM, ushort, unsigned short);
    TEST_SHMEM_REDUCE(OP_SUM, uint, unsigned int);
    TEST_SHMEM_REDUCE(OP_SUM, ulong, unsigned long);
    TEST_SHMEM_REDUCE(OP_SUM, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(OP_SUM, int8, int8_t);
    TEST_SHMEM_REDUCE(OP_SUM, int16, int16_t);
    TEST_SHMEM_REDUCE(OP_SUM, int32, int32_t);
    TEST_SHMEM_REDUCE(OP_SUM, int64, int64_t);
    TEST_SHMEM_REDUCE(OP_SUM, uint8, uint8_t);
    TEST_SHMEM_REDUCE(OP_SUM, uint16, uint16_t);
    TEST_SHMEM_REDUCE(OP_SUM, uint32, uint32_t);
    TEST_SHMEM_REDUCE(OP_SUM, uint64, uint64_t);
    TEST_SHMEM_REDUCE(OP_SUM, size, size_t);
    TEST_SHMEM_REDUCE(OP_SUM, float, float);
    TEST_SHMEM_REDUCE(OP_SUM, double, double);
    TEST_SHMEM_REDUCE(OP_SUM, longdouble, long double);
    TEST_SHMEM_REDUCE(OP_SUM, complexd, double _Complex);
    TEST_SHMEM_REDUCE(OP_SUM, complexf, float _Complex);

    TEST_SHMEM_REDUCE(OP_PROD, char, char);
    TEST_SHMEM_REDUCE(OP_PROD, schar, signed char);
    TEST_SHMEM_REDUCE(OP_PROD, short, short);
    TEST_SHMEM_REDUCE(OP_PROD, int, int);
    TEST_SHMEM_REDUCE(OP_PROD, long, long);
    TEST_SHMEM_REDUCE(OP_PROD, longlong, long long);
    TEST_SHMEM_REDUCE(OP_PROD, ptrdiff, ptrdiff_t);
    TEST_SHMEM_REDUCE(OP_PROD, uchar, unsigned char);
    TEST_SHMEM_REDUCE(OP_PROD, ushort, unsigned short);
    TEST_SHMEM_REDUCE(OP_PROD, uint, unsigned int);
    TEST_SHMEM_REDUCE(OP_PROD, ulong, unsigned long);
    TEST_SHMEM_REDUCE(OP_PROD, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(OP_PROD, int8, int8_t);
    TEST_SHMEM_REDUCE(OP_PROD, int16, int16_t);
    TEST_SHMEM_REDUCE(OP_PROD, int32, int32_t);
    TEST_SHMEM_REDUCE(OP_PROD, int64, int64_t);
    TEST_SHMEM_REDUCE(OP_PROD, uint8, uint8_t);
    TEST_SHMEM_REDUCE(OP_PROD, uint16, uint16_t);
    TEST_SHMEM_REDUCE(OP_PROD, uint32, uint32_t);
    TEST_SHMEM_REDUCE(OP_PROD, uint64, uint64_t);
    TEST_SHMEM_REDUCE(OP_PROD, size, size_t);
    TEST_SHMEM_REDUCE(OP_PROD, float, float);
    TEST_SHMEM_REDUCE(OP_PROD, double, double);
    TEST_SHMEM_REDUCE(OP_PROD, longdouble, long double);
    TEST_SHMEM_REDUCE(OP_PROD, complexd, double _Complex);
    TEST_SHMEM_REDUCE(OP_PROD, complexf, float _Complex);

    fprintf(stdout, "Test passed with ret = %d\n", rc);
  
    shmem_finalize();
    return rc;
}
