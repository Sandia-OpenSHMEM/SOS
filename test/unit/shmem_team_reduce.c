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

#define MAX_NPES 32

enum op { and = 0, or, xor, max, min, sum, prod };

const double FLOATING_POINT_TOLERANCE = 1e-6;

#define REDUCTION(OP, TYPE) \
  do { ret = shmem_##TYPE##_##OP##_reduce(SHMEM_TEAM_WORLD, dest, src, npes); } while (0)

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

#define TEST_SHMEM_REDUCE(OP, TYPENAME, TYPE)                                  \
  do {                                                                         \
    static TYPE src[MAX_NPES];                                                 \
    static TYPE dest[MAX_NPES];                                                \
    int ret;                                                                   \
    const bool floating_point_val = is_floating_point((TYPE)0);                \
                                                                               \
    INIT_SRC_BUFFER(TYPE);                                                     \
                                                                               \
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

    TEST_SHMEM_REDUCE(and, uchar, unsigned char);
    TEST_SHMEM_REDUCE(and, ushort, unsigned short);
    TEST_SHMEM_REDUCE(and, uint, unsigned int);
    TEST_SHMEM_REDUCE(and, ulong, unsigned long);
    TEST_SHMEM_REDUCE(and, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(and, int8, int8_t);
    TEST_SHMEM_REDUCE(and, int16, int16_t);
    TEST_SHMEM_REDUCE(and, int32, int32_t);
    TEST_SHMEM_REDUCE(and, int64, int64_t);
    TEST_SHMEM_REDUCE(and, uint8, uint8_t);
    TEST_SHMEM_REDUCE(and, uint16, uint16_t);
    TEST_SHMEM_REDUCE(and, uint32, uint32_t);
    TEST_SHMEM_REDUCE(and, uint64, uint64_t);
    TEST_SHMEM_REDUCE(and, size, size_t);

    TEST_SHMEM_REDUCE(or, uchar, unsigned char);
    TEST_SHMEM_REDUCE(or, ushort, unsigned short);
    TEST_SHMEM_REDUCE(or, uint, unsigned int);
    TEST_SHMEM_REDUCE(or, ulong, unsigned long);
    TEST_SHMEM_REDUCE(or, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(or, int8, int8_t);
    TEST_SHMEM_REDUCE(or, int16, int16_t);
    TEST_SHMEM_REDUCE(or, int32, int32_t);
    TEST_SHMEM_REDUCE(or, int64, int64_t);
    TEST_SHMEM_REDUCE(or, uint8, uint8_t);
    TEST_SHMEM_REDUCE(or, uint16, uint16_t);
    TEST_SHMEM_REDUCE(or, uint32, uint32_t);
    TEST_SHMEM_REDUCE(or, uint64, uint64_t);
    TEST_SHMEM_REDUCE(or, size, size_t);

    TEST_SHMEM_REDUCE(xor, uchar, unsigned char);
    TEST_SHMEM_REDUCE(xor, ushort, unsigned short);
    TEST_SHMEM_REDUCE(xor, uint, unsigned int);
    TEST_SHMEM_REDUCE(xor, ulong, unsigned long);
    TEST_SHMEM_REDUCE(xor, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(xor, int8, int8_t);
    TEST_SHMEM_REDUCE(xor, int16, int16_t);
    TEST_SHMEM_REDUCE(xor, int32, int32_t);
    TEST_SHMEM_REDUCE(xor, int64, int64_t);
    TEST_SHMEM_REDUCE(xor, uint8, uint8_t);
    TEST_SHMEM_REDUCE(xor, uint16, uint16_t);
    TEST_SHMEM_REDUCE(xor, uint32, uint32_t);
    TEST_SHMEM_REDUCE(xor, uint64, uint64_t);
    TEST_SHMEM_REDUCE(xor, size, size_t);

    TEST_SHMEM_REDUCE(max, char, char);
    TEST_SHMEM_REDUCE(max, schar, signed char);
    TEST_SHMEM_REDUCE(max, short, short);
    TEST_SHMEM_REDUCE(max, int, int);
    TEST_SHMEM_REDUCE(max, long, long);
    TEST_SHMEM_REDUCE(max, longlong, long long);
    TEST_SHMEM_REDUCE(max, ptrdiff, ptrdiff_t);
    TEST_SHMEM_REDUCE(max, uchar, unsigned char);
    TEST_SHMEM_REDUCE(max, ushort, unsigned short);
    TEST_SHMEM_REDUCE(max, uint, unsigned int);
    TEST_SHMEM_REDUCE(max, ulong, unsigned long);
    TEST_SHMEM_REDUCE(max, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(max, int8, int8_t);
    TEST_SHMEM_REDUCE(max, int16, int16_t);
    TEST_SHMEM_REDUCE(max, int32, int32_t);
    TEST_SHMEM_REDUCE(max, int64, int64_t);
    TEST_SHMEM_REDUCE(max, uint8, uint8_t);
    TEST_SHMEM_REDUCE(max, uint16, uint16_t);
    TEST_SHMEM_REDUCE(max, uint32, uint32_t);
    TEST_SHMEM_REDUCE(max, uint64, uint64_t);
    TEST_SHMEM_REDUCE(max, size, size_t);
    TEST_SHMEM_REDUCE(max, float, float);
    TEST_SHMEM_REDUCE(max, double, double);
    TEST_SHMEM_REDUCE(max, longdouble, long double);

    TEST_SHMEM_REDUCE(min, char, char);
    TEST_SHMEM_REDUCE(min, schar, signed char);
    TEST_SHMEM_REDUCE(min, short, short);
    TEST_SHMEM_REDUCE(min, int, int);
    TEST_SHMEM_REDUCE(min, long, long);
    TEST_SHMEM_REDUCE(min, longlong, long long);
    TEST_SHMEM_REDUCE(min, ptrdiff, ptrdiff_t);
    TEST_SHMEM_REDUCE(min, uchar, unsigned char);
    TEST_SHMEM_REDUCE(min, ushort, unsigned short);
    TEST_SHMEM_REDUCE(min, uint, unsigned int);
    TEST_SHMEM_REDUCE(min, ulong, unsigned long);
    TEST_SHMEM_REDUCE(min, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(min, int8, int8_t);
    TEST_SHMEM_REDUCE(min, int16, int16_t);
    TEST_SHMEM_REDUCE(min, int32, int32_t);
    TEST_SHMEM_REDUCE(min, int64, int64_t);
    TEST_SHMEM_REDUCE(min, uint8, uint8_t);
    TEST_SHMEM_REDUCE(min, uint16, uint16_t);
    TEST_SHMEM_REDUCE(min, uint32, uint32_t);
    TEST_SHMEM_REDUCE(min, uint64, uint64_t);
    TEST_SHMEM_REDUCE(min, size, size_t);
    TEST_SHMEM_REDUCE(min, float, float);
    TEST_SHMEM_REDUCE(min, double, double);
    TEST_SHMEM_REDUCE(min, longdouble, long double);

    TEST_SHMEM_REDUCE(sum, char, char);
    TEST_SHMEM_REDUCE(sum, schar, signed char);
    TEST_SHMEM_REDUCE(sum, short, short);
    TEST_SHMEM_REDUCE(sum, int, int);
    TEST_SHMEM_REDUCE(sum, long, long);
    TEST_SHMEM_REDUCE(sum, longlong, long long);
    TEST_SHMEM_REDUCE(sum, ptrdiff, ptrdiff_t);
    TEST_SHMEM_REDUCE(sum, uchar, unsigned char);
    TEST_SHMEM_REDUCE(sum, ushort, unsigned short);
    TEST_SHMEM_REDUCE(sum, uint, unsigned int);
    TEST_SHMEM_REDUCE(sum, ulong, unsigned long);
    TEST_SHMEM_REDUCE(sum, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(sum, int8, int8_t);
    TEST_SHMEM_REDUCE(sum, int16, int16_t);
    TEST_SHMEM_REDUCE(sum, int32, int32_t);
    TEST_SHMEM_REDUCE(sum, int64, int64_t);
    TEST_SHMEM_REDUCE(sum, uint8, uint8_t);
    TEST_SHMEM_REDUCE(sum, uint16, uint16_t);
    TEST_SHMEM_REDUCE(sum, uint32, uint32_t);
    TEST_SHMEM_REDUCE(sum, uint64, uint64_t);
    TEST_SHMEM_REDUCE(sum, size, size_t);
    TEST_SHMEM_REDUCE(sum, float, float);
    TEST_SHMEM_REDUCE(sum, double, double);
    TEST_SHMEM_REDUCE(sum, longdouble, long double);
    TEST_SHMEM_REDUCE(sum, complexd, double _Complex);
    TEST_SHMEM_REDUCE(sum, complexf, float _Complex);

    TEST_SHMEM_REDUCE(prod, char, char);
    TEST_SHMEM_REDUCE(prod, schar, signed char);
    TEST_SHMEM_REDUCE(prod, short, short);
    TEST_SHMEM_REDUCE(prod, int, int);
    TEST_SHMEM_REDUCE(prod, long, long);
    TEST_SHMEM_REDUCE(prod, longlong, long long);
    TEST_SHMEM_REDUCE(prod, ptrdiff, ptrdiff_t);
    TEST_SHMEM_REDUCE(prod, uchar, unsigned char);
    TEST_SHMEM_REDUCE(prod, ushort, unsigned short);
    TEST_SHMEM_REDUCE(prod, uint, unsigned int);
    TEST_SHMEM_REDUCE(prod, ulong, unsigned long);
    TEST_SHMEM_REDUCE(prod, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(prod, int8, int8_t);
    TEST_SHMEM_REDUCE(prod, int16, int16_t);
    TEST_SHMEM_REDUCE(prod, int32, int32_t);
    TEST_SHMEM_REDUCE(prod, int64, int64_t);
    TEST_SHMEM_REDUCE(prod, uint8, uint8_t);
    TEST_SHMEM_REDUCE(prod, uint16, uint16_t);
    TEST_SHMEM_REDUCE(prod, uint32, uint32_t);
    TEST_SHMEM_REDUCE(prod, uint64, uint64_t);
    TEST_SHMEM_REDUCE(prod, size, size_t);
    TEST_SHMEM_REDUCE(prod, float, float);
    TEST_SHMEM_REDUCE(prod, double, double);
    TEST_SHMEM_REDUCE(prod, longdouble, long double);
    TEST_SHMEM_REDUCE(prod, complexd, double _Complex);
    TEST_SHMEM_REDUCE(prod, complexf, float _Complex);

    shmem_finalize();
    return rc;
}
