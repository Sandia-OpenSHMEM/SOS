/*
 *  This test program is derived from a unit test created by Nick Park.
 *  The original unit test is a work of the U.S. Government and is not subject
 *  to copyright protection in the United States.  Foreign copyrights may
 *  apply.
 *
 *  Copyright (c) 2023 Intel Corporation. All rights reserved.
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

enum op { OP_MAX, OP_MIN };

const double FLOATING_POINT_TOLERANCE = 1e-6;

#define REDUCTION(OP, TYPE)                                                   \
    do {                                                                      \
        switch (OP) {                                                         \
            case OP_MAX:                                                      \
                ret = shmem_##TYPE##_max_reduce(SHMEM_TEAM_WORLD, dest, src,  \
                                                npes);                        \
                break;                                                        \
            case OP_MIN:                                                      \
                ret = shmem_##TYPE##_min_reduce(SHMEM_TEAM_WORLD, dest, src,  \
                                               npes);                         \
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
      case OP_MAX:                                                             \
          if (floating_point_val)                                              \
              CHECK_DEST_BUFFER_FP(OP, TYPE, 1ULL, FLOATING_POINT_TOLERANCE);  \
          else                                                                 \
              CHECK_DEST_BUFFER(OP, TYPE, 1ULL);                               \
          break;                                                               \
      case OP_MIN:                                                             \
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

    TEST_SHMEM_REDUCE(OP_MAX, char, char);
    TEST_SHMEM_REDUCE(OP_MAX, schar, signed char);
    TEST_SHMEM_REDUCE(OP_MAX, short, short);
    TEST_SHMEM_REDUCE(OP_MAX, int, int);
    TEST_SHMEM_REDUCE(OP_MAX, long, long);
    TEST_SHMEM_REDUCE(OP_MAX, longlong, long long);
    TEST_SHMEM_REDUCE(OP_MAX, ptrdiff, ptrdiff_t);
    TEST_SHMEM_REDUCE(OP_MAX, uchar, unsigned char);
    TEST_SHMEM_REDUCE(OP_MAX, ushort, unsigned short);
    TEST_SHMEM_REDUCE(OP_MAX, uint, unsigned int);
    TEST_SHMEM_REDUCE(OP_MAX, ulong, unsigned long);
    TEST_SHMEM_REDUCE(OP_MAX, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(OP_MAX, int8, int8_t);
    TEST_SHMEM_REDUCE(OP_MAX, int16, int16_t);
    TEST_SHMEM_REDUCE(OP_MAX, int32, int32_t);
    TEST_SHMEM_REDUCE(OP_MAX, int64, int64_t);
    TEST_SHMEM_REDUCE(OP_MAX, uint8, uint8_t);
    TEST_SHMEM_REDUCE(OP_MAX, uint16, uint16_t);
    TEST_SHMEM_REDUCE(OP_MAX, uint32, uint32_t);
    TEST_SHMEM_REDUCE(OP_MAX, uint64, uint64_t);
    TEST_SHMEM_REDUCE(OP_MAX, size, size_t);
    TEST_SHMEM_REDUCE(OP_MAX, float, float);
    TEST_SHMEM_REDUCE(OP_MAX, double, double);
    TEST_SHMEM_REDUCE(OP_MAX, longdouble, long double);

    TEST_SHMEM_REDUCE(OP_MIN, char, char);
    TEST_SHMEM_REDUCE(OP_MIN, schar, signed char);
    TEST_SHMEM_REDUCE(OP_MIN, short, short);
    TEST_SHMEM_REDUCE(OP_MIN, int, int);
    TEST_SHMEM_REDUCE(OP_MIN, long, long);
    TEST_SHMEM_REDUCE(OP_MIN, longlong, long long);
    TEST_SHMEM_REDUCE(OP_MIN, ptrdiff, ptrdiff_t);
    TEST_SHMEM_REDUCE(OP_MIN, uchar, unsigned char);
    TEST_SHMEM_REDUCE(OP_MIN, ushort, unsigned short);
    TEST_SHMEM_REDUCE(OP_MIN, uint, unsigned int);
    TEST_SHMEM_REDUCE(OP_MIN, ulong, unsigned long);
    TEST_SHMEM_REDUCE(OP_MIN, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(OP_MIN, int8, int8_t);
    TEST_SHMEM_REDUCE(OP_MIN, int16, int16_t);
    TEST_SHMEM_REDUCE(OP_MIN, int32, int32_t);
    TEST_SHMEM_REDUCE(OP_MIN, int64, int64_t);
    TEST_SHMEM_REDUCE(OP_MIN, uint8, uint8_t);
    TEST_SHMEM_REDUCE(OP_MIN, uint16, uint16_t);
    TEST_SHMEM_REDUCE(OP_MIN, uint32, uint32_t);
    TEST_SHMEM_REDUCE(OP_MIN, uint64, uint64_t);
    TEST_SHMEM_REDUCE(OP_MIN, size, size_t);
    TEST_SHMEM_REDUCE(OP_MIN, float, float);
    TEST_SHMEM_REDUCE(OP_MIN, double, double);
    TEST_SHMEM_REDUCE(OP_MIN, longdouble, long double);

    fprintf(stdout, "Test passed with ret = %d\n", rc);
  
    shmem_finalize();
    return rc;
}
