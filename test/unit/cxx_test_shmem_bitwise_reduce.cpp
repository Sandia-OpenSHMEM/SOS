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

enum op { OP_AND = 0, OP_OR, OP_XOR };

#define REDUCTION(OP, TYPE)                                                   \
    do {                                                                      \
        switch (OP) {                                                         \
            case OP_AND:                                                      \
                ret = shmem_##TYPE##_and_reduce(SHMEM_TEAM_WORLD, dest, src,  \
                                                npes);                        \
                break;                                                        \
            case OP_OR:                                                       \
                ret = shmem_##TYPE##_or_reduce(SHMEM_TEAM_WORLD, dest, src,   \
                                               npes);                         \
                break;                                                        \
            case OP_XOR:                                                      \
                ret = shmem_##TYPE##_xor_reduce(SHMEM_TEAM_WORLD, dest, src,  \
                                                npes);                        \
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

#define TEST_SHMEM_REDUCE(OP, TYPENAME, TYPE)                                  \
  do {                                                                         \
    static TYPE src[MAX_NPES];                                                 \
    static TYPE dest[MAX_NPES];                                                \
    int ret;                                                                   \
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
      case OP_AND:                                                             \
          CHECK_DEST_BUFFER(OP, TYPE, 1ULL);                                   \
          break;                                                               \
      case OP_OR:                                                              \
          CHECK_DEST_BUFFER(OP, TYPE, 1ULL);                                   \
          break;                                                               \
      case OP_XOR:                                                             \
          CHECK_DEST_BUFFER(OP, TYPE, (TYPE)(npes % 2 ? 1ULL : 0ULL));         \
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

    TEST_SHMEM_REDUCE(OP_AND, uchar, unsigned char);
    TEST_SHMEM_REDUCE(OP_AND, ushort, unsigned short);
    TEST_SHMEM_REDUCE(OP_AND, uint, unsigned int);
    TEST_SHMEM_REDUCE(OP_AND, ulong, unsigned long);
    TEST_SHMEM_REDUCE(OP_AND, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(OP_AND, int8, int8_t);
    TEST_SHMEM_REDUCE(OP_AND, int16, int16_t);
    TEST_SHMEM_REDUCE(OP_AND, int32, int32_t);
    TEST_SHMEM_REDUCE(OP_AND, int64, int64_t);
    TEST_SHMEM_REDUCE(OP_AND, uint8, uint8_t);
    TEST_SHMEM_REDUCE(OP_AND, uint16, uint16_t);
    TEST_SHMEM_REDUCE(OP_AND, uint32, uint32_t);
    TEST_SHMEM_REDUCE(OP_AND, uint64, uint64_t);
    TEST_SHMEM_REDUCE(OP_AND, size, size_t);

    TEST_SHMEM_REDUCE(OP_OR, uchar, unsigned char);
    TEST_SHMEM_REDUCE(OP_OR, ushort, unsigned short);
    TEST_SHMEM_REDUCE(OP_OR, uint, unsigned int);
    TEST_SHMEM_REDUCE(OP_OR, ulong, unsigned long);
    TEST_SHMEM_REDUCE(OP_OR, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(OP_OR, int8, int8_t);
    TEST_SHMEM_REDUCE(OP_OR, int16, int16_t);
    TEST_SHMEM_REDUCE(OP_OR, int32, int32_t);
    TEST_SHMEM_REDUCE(OP_OR, int64, int64_t);
    TEST_SHMEM_REDUCE(OP_OR, uint8, uint8_t);
    TEST_SHMEM_REDUCE(OP_OR, uint16, uint16_t);
    TEST_SHMEM_REDUCE(OP_OR, uint32, uint32_t);
    TEST_SHMEM_REDUCE(OP_OR, uint64, uint64_t);
    TEST_SHMEM_REDUCE(OP_OR, size, size_t);

    TEST_SHMEM_REDUCE(OP_XOR, uchar, unsigned char);
    TEST_SHMEM_REDUCE(OP_XOR, ushort, unsigned short);
    TEST_SHMEM_REDUCE(OP_XOR, uint, unsigned int);
    TEST_SHMEM_REDUCE(OP_XOR, ulong, unsigned long);
    TEST_SHMEM_REDUCE(OP_XOR, ulonglong, unsigned long long);
    TEST_SHMEM_REDUCE(OP_XOR, int8, int8_t);
    TEST_SHMEM_REDUCE(OP_XOR, int16, int16_t);
    TEST_SHMEM_REDUCE(OP_XOR, int32, int32_t);
    TEST_SHMEM_REDUCE(OP_XOR, int64, int64_t);
    TEST_SHMEM_REDUCE(OP_XOR, uint8, uint8_t);
    TEST_SHMEM_REDUCE(OP_XOR, uint16, uint16_t);
    TEST_SHMEM_REDUCE(OP_XOR, uint32, uint32_t);
    TEST_SHMEM_REDUCE(OP_XOR, uint64, uint64_t);
    TEST_SHMEM_REDUCE(OP_XOR, size, size_t);

    fprintf(stdout, "Test passed with ret = %d\n", rc);
  
    shmem_finalize();
    return rc;
}
