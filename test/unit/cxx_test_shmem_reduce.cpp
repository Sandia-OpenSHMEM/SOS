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

enum op { AND = 0, OR, XOR, MAX, MIN, SUM, PROD };

const double FLOATING_POINT_TOLERANCE = 1e-6;

#define REDUCTION(OP, TYPE) \
  do {                                                                         \
      switch(OP) {                                                             \
          case AND:                                                            \
              ret = shmem_##TYPE##_and_reduce(SHMEM_TEAM_WORLD, dest, src,     \
                                              npes);                           \
              break;                                                           \
          case OR:                                                             \
              ret = shmem_##TYPE##_or_reduce(SHMEM_TEAM_WORLD, dest, src,      \
                                             npes);                            \
              break;                                                           \
          case XOR:                                                            \
              ret = shmem_##TYPE##_xor_reduce(SHMEM_TEAM_WORLD, dest, src,     \
                                              npes);                           \
              break;                                                           \
          case MAX:                                                            \
              ret = shmem_##TYPE##_max_reduce(SHMEM_TEAM_WORLD, dest, src,     \
                                              npes);                           \
              break;                                                           \
          case MIN:                                                            \
              ret = shmem_##TYPE##_min_reduce(SHMEM_TEAM_WORLD, dest, src,     \
                                              npes);                           \
              break;                                                           \
          case SUM:                                                            \
              ret = shmem_##TYPE##_sum_reduce(SHMEM_TEAM_WORLD, dest, src,     \
                                              npes);                           \
              break;                                                           \
          case PROD:                                                           \
              ret = shmem_##TYPE##_prod_reduce(SHMEM_TEAM_WORLD, dest, src,    \
                                               npes);                          \
              break;                                                           \
          default:                                                             \
              printf("Invalid operation (%d)\n", OP);                          \
              shmem_global_exit(1);                                            \
              break;                                                           \
      }                                                                        \
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
      case AND:                                                                \
          CHECK_DEST_BUFFER(OP, TYPE, 1ULL);                                   \
          break;                                                               \
      case OR:                                                                 \
          CHECK_DEST_BUFFER(OP, TYPE, 1ULL);                                   \
          break;                                                               \
      case XOR:                                                                \
          CHECK_DEST_BUFFER(OP, TYPE, (TYPE)(npes % 2 ? 1ULL : 0ULL));         \
          break;                                                               \
      case MAX:                                                                \
          if (floating_point_val)                                              \
              CHECK_DEST_BUFFER_FP(OP, TYPE, 1ULL, FLOATING_POINT_TOLERANCE);  \
          else                                                                 \
              CHECK_DEST_BUFFER(OP, TYPE, 1ULL);                               \
          break;                                                               \
      case MIN:                                                                \
          if (floating_point_val)                                              \
              CHECK_DEST_BUFFER_FP(OP, TYPE, 1ULL, FLOATING_POINT_TOLERANCE);  \
          else                                                                 \
              CHECK_DEST_BUFFER(OP, TYPE, 1ULL);                               \
          break;                                                               \
      case SUM:                                                                \
          if (floating_point_val)                                              \
              CHECK_DEST_BUFFER_FP(OP, TYPE, npes, FLOATING_POINT_TOLERANCE);  \
          else                                                                 \
              CHECK_DEST_BUFFER(OP, TYPE, npes);                               \
          break;                                                               \
      case PROD:                                                               \
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

    TEST_SHMEM_REDUCE(AND, uchar, unsigned char);
    TEST_SHMEM_REDUCE(AND, ushort, unsigned short);
    TEST_SHMEM_REDUCE(AND, uint, unsigned int);
    TEST_SHMEM_REDUCE(AND, ulong, unsigned long);
    TEST_SHMEM_REDUCE(AND, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(AND, int8, int8_t);
    TEST_SHMEM_REDUCE(AND, int16, int16_t);
    TEST_SHMEM_REDUCE(AND, int32, int32_t);
    TEST_SHMEM_REDUCE(AND, int64, int64_t);
    TEST_SHMEM_REDUCE(AND, uint8, uint8_t);
    TEST_SHMEM_REDUCE(AND, uint16, uint16_t);
    TEST_SHMEM_REDUCE(AND, uint32, uint32_t);
    TEST_SHMEM_REDUCE(AND, uint64, uint64_t);
    TEST_SHMEM_REDUCE(AND, size, size_t);

    TEST_SHMEM_REDUCE(OR, uchar, unsigned char);
    TEST_SHMEM_REDUCE(OR, ushort, unsigned short);
    TEST_SHMEM_REDUCE(OR, uint, unsigned int);
    TEST_SHMEM_REDUCE(OR, ulong, unsigned long);
    TEST_SHMEM_REDUCE(OR, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(OR, int8, int8_t);
    TEST_SHMEM_REDUCE(OR, int16, int16_t);
    TEST_SHMEM_REDUCE(OR, int32, int32_t);
    TEST_SHMEM_REDUCE(OR, int64, int64_t);
    TEST_SHMEM_REDUCE(OR, uint8, uint8_t);
    TEST_SHMEM_REDUCE(OR, uint16, uint16_t);
    TEST_SHMEM_REDUCE(OR, uint32, uint32_t);
    TEST_SHMEM_REDUCE(OR, uint64, uint64_t);
    TEST_SHMEM_REDUCE(OR, size, size_t);

    TEST_SHMEM_REDUCE(XOR, uchar, unsigned char);
    TEST_SHMEM_REDUCE(XOR, ushort, unsigned short);
    TEST_SHMEM_REDUCE(XOR, uint, unsigned int);
    TEST_SHMEM_REDUCE(XOR, ulong, unsigned long);
    TEST_SHMEM_REDUCE(XOR, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(XOR, int8, int8_t);
    TEST_SHMEM_REDUCE(XOR, int16, int16_t);
    TEST_SHMEM_REDUCE(XOR, int32, int32_t);
    TEST_SHMEM_REDUCE(XOR, int64, int64_t);
    TEST_SHMEM_REDUCE(XOR, uint8, uint8_t);
    TEST_SHMEM_REDUCE(XOR, uint16, uint16_t);
    TEST_SHMEM_REDUCE(XOR, uint32, uint32_t);
    TEST_SHMEM_REDUCE(XOR, uint64, uint64_t);
    TEST_SHMEM_REDUCE(XOR, size, size_t);

    TEST_SHMEM_REDUCE(MAX, char, char);
    TEST_SHMEM_REDUCE(MAX, schar, signed char);
    TEST_SHMEM_REDUCE(MAX, short, short);
    TEST_SHMEM_REDUCE(MAX, int, int);
    TEST_SHMEM_REDUCE(MAX, long, long);
    TEST_SHMEM_REDUCE(MAX, longlong, long long);
    TEST_SHMEM_REDUCE(MAX, ptrdiff, ptrdiff_t);
    TEST_SHMEM_REDUCE(MAX, uchar, unsigned char);
    TEST_SHMEM_REDUCE(MAX, ushort, unsigned short);
    TEST_SHMEM_REDUCE(MAX, uint, unsigned int);
    TEST_SHMEM_REDUCE(MAX, ulong, unsigned long);
    TEST_SHMEM_REDUCE(MAX, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(MAX, int8, int8_t);
    TEST_SHMEM_REDUCE(MAX, int16, int16_t);
    TEST_SHMEM_REDUCE(MAX, int32, int32_t);
    TEST_SHMEM_REDUCE(MAX, int64, int64_t);
    TEST_SHMEM_REDUCE(MAX, uint8, uint8_t);
    TEST_SHMEM_REDUCE(MAX, uint16, uint16_t);
    TEST_SHMEM_REDUCE(MAX, uint32, uint32_t);
    TEST_SHMEM_REDUCE(MAX, uint64, uint64_t);
    TEST_SHMEM_REDUCE(MAX, size, size_t);
    TEST_SHMEM_REDUCE(MAX, float, float);
    TEST_SHMEM_REDUCE(MAX, double, double);
    TEST_SHMEM_REDUCE(MAX, longdouble, long double);

    TEST_SHMEM_REDUCE(MIN, char, char);
    TEST_SHMEM_REDUCE(MIN, schar, signed char);
    TEST_SHMEM_REDUCE(MIN, short, short);
    TEST_SHMEM_REDUCE(MIN, int, int);
    TEST_SHMEM_REDUCE(MIN, long, long);
    TEST_SHMEM_REDUCE(MIN, longlong, long long);
    TEST_SHMEM_REDUCE(MIN, ptrdiff, ptrdiff_t);
    TEST_SHMEM_REDUCE(MIN, uchar, unsigned char);
    TEST_SHMEM_REDUCE(MIN, ushort, unsigned short);
    TEST_SHMEM_REDUCE(MIN, uint, unsigned int);
    TEST_SHMEM_REDUCE(MIN, ulong, unsigned long);
    TEST_SHMEM_REDUCE(MIN, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(MIN, int8, int8_t);
    TEST_SHMEM_REDUCE(MIN, int16, int16_t);
    TEST_SHMEM_REDUCE(MIN, int32, int32_t);
    TEST_SHMEM_REDUCE(MIN, int64, int64_t);
    TEST_SHMEM_REDUCE(MIN, uint8, uint8_t);
    TEST_SHMEM_REDUCE(MIN, uint16, uint16_t);
    TEST_SHMEM_REDUCE(MIN, uint32, uint32_t);
    TEST_SHMEM_REDUCE(MIN, uint64, uint64_t);
    TEST_SHMEM_REDUCE(MIN, size, size_t);
    TEST_SHMEM_REDUCE(MIN, float, float);
    TEST_SHMEM_REDUCE(MIN, double, double);
    TEST_SHMEM_REDUCE(MIN, longdouble, long double);

    TEST_SHMEM_REDUCE(SUM, char, char);
    TEST_SHMEM_REDUCE(SUM, schar, signed char);
    TEST_SHMEM_REDUCE(SUM, short, short);
    TEST_SHMEM_REDUCE(SUM, int, int);
    TEST_SHMEM_REDUCE(SUM, long, long);
    TEST_SHMEM_REDUCE(SUM, longlong, long long);
    TEST_SHMEM_REDUCE(SUM, ptrdiff, ptrdiff_t);
    TEST_SHMEM_REDUCE(SUM, uchar, unsigned char);
    TEST_SHMEM_REDUCE(SUM, ushort, unsigned short);
    TEST_SHMEM_REDUCE(SUM, uint, unsigned int);
    TEST_SHMEM_REDUCE(SUM, ulong, unsigned long);
    TEST_SHMEM_REDUCE(SUM, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(SUM, int8, int8_t);
    TEST_SHMEM_REDUCE(SUM, int16, int16_t);
    TEST_SHMEM_REDUCE(SUM, int32, int32_t);
    TEST_SHMEM_REDUCE(SUM, int64, int64_t);
    TEST_SHMEM_REDUCE(SUM, uint8, uint8_t);
    TEST_SHMEM_REDUCE(SUM, uint16, uint16_t);
    TEST_SHMEM_REDUCE(SUM, uint32, uint32_t);
    TEST_SHMEM_REDUCE(SUM, uint64, uint64_t);
    TEST_SHMEM_REDUCE(SUM, size, size_t);
    TEST_SHMEM_REDUCE(SUM, float, float);
    TEST_SHMEM_REDUCE(SUM, double, double);
    TEST_SHMEM_REDUCE(SUM, longdouble, long double);
    TEST_SHMEM_REDUCE(SUM, complexd, double _Complex);
    TEST_SHMEM_REDUCE(SUM, complexf, float _Complex);

    TEST_SHMEM_REDUCE(PROD, char, char);
    TEST_SHMEM_REDUCE(PROD, schar, signed char);
    TEST_SHMEM_REDUCE(PROD, short, short);
    TEST_SHMEM_REDUCE(PROD, int, int);
    TEST_SHMEM_REDUCE(PROD, long, long);
    TEST_SHMEM_REDUCE(PROD, longlong, long long);
    TEST_SHMEM_REDUCE(PROD, ptrdiff, ptrdiff_t);
    TEST_SHMEM_REDUCE(PROD, uchar, unsigned char);
    TEST_SHMEM_REDUCE(PROD, ushort, unsigned short);
    TEST_SHMEM_REDUCE(PROD, uint, unsigned int);
    TEST_SHMEM_REDUCE(PROD, ulong, unsigned long);
    TEST_SHMEM_REDUCE(PROD, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(PROD, int8, int8_t);
    TEST_SHMEM_REDUCE(PROD, int16, int16_t);
    TEST_SHMEM_REDUCE(PROD, int32, int32_t);
    TEST_SHMEM_REDUCE(PROD, int64, int64_t);
    TEST_SHMEM_REDUCE(PROD, uint8, uint8_t);
    TEST_SHMEM_REDUCE(PROD, uint16, uint16_t);
    TEST_SHMEM_REDUCE(PROD, uint32, uint32_t);
    TEST_SHMEM_REDUCE(PROD, uint64, uint64_t);
    TEST_SHMEM_REDUCE(PROD, size, size_t);
    TEST_SHMEM_REDUCE(PROD, float, float);
    TEST_SHMEM_REDUCE(PROD, double, double);
    TEST_SHMEM_REDUCE(PROD, longdouble, long double);
    TEST_SHMEM_REDUCE(PROD, complexd, double _Complex);
    TEST_SHMEM_REDUCE(PROD, complexf, float _Complex);

    shmem_finalize();
    return rc;
}
