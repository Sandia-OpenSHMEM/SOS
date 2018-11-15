/*
 *  This test program is derived from a unit test created by Nick Park.
 *  The original unit test is a work of the U.S. Government and is not subject
 *  to copyright protection in the United States.  Foreign copyrights may
 *  apply.
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

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <shmem.h>

enum op { GET = 0, IGET, GET_NBI };

#define TEST_SHMEM_GET(OP, USE_CTX, TYPE, TYPENAME)             \
  do {                                                          \
    static TYPE remote[10];                                     \
    const int mype = shmem_my_pe();                             \
    const int npes = shmem_n_pes();                             \
    TYPE local[10];                                             \
    for (int i = 0; i < 10; i++)                                \
      remote[i] = (TYPE)mype;                                   \
    shmem_barrier_all();                                        \
    switch (OP) {                                               \
      case GET:                                                 \
        if (USE_CTX)                                            \
          shmem_ctx_##TYPENAME##_get(SHMEM_CTX_DEFAULT, local,  \
                                remote, 10, (mype + 1) % npes); \
        else                                                    \
          shmem_##TYPENAME##_get(local, remote, 10,             \
                                       (mype + 1) % npes);      \
        break;                                                  \
      case IGET:                                                \
        if (USE_CTX)                                            \
          shmem_ctx_##TYPENAME##_iget(SHMEM_CTX_DEFAULT, local, \
                          remote, 1, 1, 10, (mype + 1) % npes); \
        else                                                    \
          shmem_##TYPENAME##_iget(local, remote, 1, 1, 10,      \
                                            (mype + 1) % npes); \
        break;                                                  \
      case GET_NBI:                                             \
        if (USE_CTX)                                            \
          shmem_ctx_##TYPENAME##_get_nbi(SHMEM_CTX_DEFAULT,     \
                         local, remote, 10, (mype + 1) % npes); \
        else                                                    \
          shmem_##TYPENAME##_get_nbi(local, remote, 10,         \
                                           (mype + 1) % npes);  \
        shmem_quiet();                                          \
        break;                                                  \
      default:                                                  \
        printf("Invalid operation (%d)\n", OP);                 \
        shmem_global_exit(1);                                   \
    }                                                           \
    for (int i = 0; i < 10; i++)                                \
      if (local[i] != (TYPE)((mype + 1) % npes)) {              \
        printf("PE %i received incorrect value with"            \
               "TEST_SHMEM_GET(%s, %d, %s)\n", mype, #OP,       \
               (int)(USE_CTX), #TYPE);                          \
        rc = EXIT_FAILURE;                                      \
      }                                                         \
  } while (false)

int main(int argc, char* argv[]) {
  shmem_init();

  int rc = EXIT_SUCCESS;

  TEST_SHMEM_GET(GET, 0, float, float);
  TEST_SHMEM_GET(GET, 0, double, double);
  TEST_SHMEM_GET(GET, 0, long double, longdouble);
  TEST_SHMEM_GET(GET, 0, char, char);
  TEST_SHMEM_GET(GET, 0, signed char, schar);
  TEST_SHMEM_GET(GET, 0, short, short);
  TEST_SHMEM_GET(GET, 0, int, int);
  TEST_SHMEM_GET(GET, 0, long, long);
  TEST_SHMEM_GET(GET, 0, long long, longlong);
  TEST_SHMEM_GET(GET, 0, unsigned char, uchar);
  TEST_SHMEM_GET(GET, 0, unsigned short, ushort);
  TEST_SHMEM_GET(GET, 0, unsigned int, uint);
  TEST_SHMEM_GET(GET, 0, unsigned long, ulong);
  TEST_SHMEM_GET(GET, 0, unsigned long long, ulonglong);
  TEST_SHMEM_GET(GET, 0, int8_t, int8);
  TEST_SHMEM_GET(GET, 0, int16_t, int16);
  TEST_SHMEM_GET(GET, 0, int32_t, int32);
  TEST_SHMEM_GET(GET, 0, int64_t, int64);
  TEST_SHMEM_GET(GET, 0, uint8_t, uint8);
  TEST_SHMEM_GET(GET, 0, uint16_t, uint16);
  TEST_SHMEM_GET(GET, 0, uint32_t, uint32);
  TEST_SHMEM_GET(GET, 0, uint64_t, uint64);
  TEST_SHMEM_GET(GET, 0, size_t, size);
  TEST_SHMEM_GET(GET, 0, ptrdiff_t, ptrdiff);

  TEST_SHMEM_GET(GET, 1, float, float);
  TEST_SHMEM_GET(GET, 1, double, double);
  TEST_SHMEM_GET(GET, 1, long double, longdouble);
  TEST_SHMEM_GET(GET, 1, char, char);
  TEST_SHMEM_GET(GET, 1, signed char, schar);
  TEST_SHMEM_GET(GET, 1, short, short);
  TEST_SHMEM_GET(GET, 1, int, int);
  TEST_SHMEM_GET(GET, 1, long, long);
  TEST_SHMEM_GET(GET, 1, long long, longlong);
  TEST_SHMEM_GET(GET, 1, unsigned char, uchar);
  TEST_SHMEM_GET(GET, 1, unsigned short, ushort);
  TEST_SHMEM_GET(GET, 1, unsigned int, uint);
  TEST_SHMEM_GET(GET, 1, unsigned long, ulong);
  TEST_SHMEM_GET(GET, 1, unsigned long long, ulonglong);
  TEST_SHMEM_GET(GET, 1, int8_t, int8);
  TEST_SHMEM_GET(GET, 1, int16_t, int16);
  TEST_SHMEM_GET(GET, 1, int32_t, int32);
  TEST_SHMEM_GET(GET, 1, int64_t, int64);
  TEST_SHMEM_GET(GET, 1, uint8_t, uint8);
  TEST_SHMEM_GET(GET, 1, uint16_t, uint16);
  TEST_SHMEM_GET(GET, 1, uint32_t, uint32);
  TEST_SHMEM_GET(GET, 1, uint64_t, uint64);
  TEST_SHMEM_GET(GET, 1, size_t, size);
  TEST_SHMEM_GET(GET, 1, ptrdiff_t, ptrdiff);

  TEST_SHMEM_GET(IGET, 0, float, float);
  TEST_SHMEM_GET(IGET, 0, double, double);
  TEST_SHMEM_GET(IGET, 0, long double, longdouble);
  TEST_SHMEM_GET(IGET, 0, char, char);
  TEST_SHMEM_GET(IGET, 0, signed char, schar);
  TEST_SHMEM_GET(IGET, 0, short, short);
  TEST_SHMEM_GET(IGET, 0, int, int);
  TEST_SHMEM_GET(IGET, 0, long, long);
  TEST_SHMEM_GET(IGET, 0, long long, longlong);
  TEST_SHMEM_GET(IGET, 0, unsigned char, uchar);
  TEST_SHMEM_GET(IGET, 0, unsigned short, ushort);
  TEST_SHMEM_GET(IGET, 0, unsigned int, uint);
  TEST_SHMEM_GET(IGET, 0, unsigned long, ulong);
  TEST_SHMEM_GET(IGET, 0, unsigned long long, ulonglong);
  TEST_SHMEM_GET(IGET, 0, int8_t, int8);
  TEST_SHMEM_GET(IGET, 0, int16_t, int16);
  TEST_SHMEM_GET(IGET, 0, int32_t, int32);
  TEST_SHMEM_GET(IGET, 0, int64_t, int64);
  TEST_SHMEM_GET(IGET, 0, uint8_t, uint8);
  TEST_SHMEM_GET(IGET, 0, uint16_t, uint16);
  TEST_SHMEM_GET(IGET, 0, uint32_t, uint32);
  TEST_SHMEM_GET(IGET, 0, uint64_t, uint64);
  TEST_SHMEM_GET(IGET, 0, size_t, size);
  TEST_SHMEM_GET(IGET, 0, ptrdiff_t, ptrdiff);

  TEST_SHMEM_GET(IGET, 1, float, float);
  TEST_SHMEM_GET(IGET, 1, double, double);
  TEST_SHMEM_GET(IGET, 1, long double, longdouble);
  TEST_SHMEM_GET(IGET, 1, char, char);
  TEST_SHMEM_GET(IGET, 1, signed char, schar);
  TEST_SHMEM_GET(IGET, 1, short, short);
  TEST_SHMEM_GET(IGET, 1, int, int);
  TEST_SHMEM_GET(IGET, 1, long, long);
  TEST_SHMEM_GET(IGET, 1, long long, longlong);
  TEST_SHMEM_GET(IGET, 1, unsigned char, uchar);
  TEST_SHMEM_GET(IGET, 1, unsigned short, ushort);
  TEST_SHMEM_GET(IGET, 1, unsigned int, uint);
  TEST_SHMEM_GET(IGET, 1, unsigned long, ulong);
  TEST_SHMEM_GET(IGET, 1, unsigned long long, ulonglong);
  TEST_SHMEM_GET(IGET, 1, int8_t, int8);
  TEST_SHMEM_GET(IGET, 1, int16_t, int16);
  TEST_SHMEM_GET(IGET, 1, int32_t, int32);
  TEST_SHMEM_GET(IGET, 1, int64_t, int64);
  TEST_SHMEM_GET(IGET, 1, uint8_t, uint8);
  TEST_SHMEM_GET(IGET, 1, uint16_t, uint16);
  TEST_SHMEM_GET(IGET, 1, uint32_t, uint32);
  TEST_SHMEM_GET(IGET, 1, uint64_t, uint64);
  TEST_SHMEM_GET(IGET, 1, size_t, size);
  TEST_SHMEM_GET(IGET, 1, ptrdiff_t, ptrdiff);

  TEST_SHMEM_GET(GET_NBI, 0, float, float);
  TEST_SHMEM_GET(GET_NBI, 0, double, double);
  TEST_SHMEM_GET(GET_NBI, 0, long double, longdouble);
  TEST_SHMEM_GET(GET_NBI, 0, char, char);
  TEST_SHMEM_GET(GET_NBI, 0, signed char, schar);
  TEST_SHMEM_GET(GET_NBI, 0, short, short);
  TEST_SHMEM_GET(GET_NBI, 0, int, int);
  TEST_SHMEM_GET(GET_NBI, 0, long, long);
  TEST_SHMEM_GET(GET_NBI, 0, long long, longlong);
  TEST_SHMEM_GET(GET_NBI, 0, unsigned char, uchar);
  TEST_SHMEM_GET(GET_NBI, 0, unsigned short, ushort);
  TEST_SHMEM_GET(GET_NBI, 0, unsigned int, uint);
  TEST_SHMEM_GET(GET_NBI, 0, unsigned long, ulong);
  TEST_SHMEM_GET(GET_NBI, 0, unsigned long long, ulonglong);
  TEST_SHMEM_GET(GET_NBI, 0, int8_t, int8);
  TEST_SHMEM_GET(GET_NBI, 0, int16_t, int16);
  TEST_SHMEM_GET(GET_NBI, 0, int32_t, int32);
  TEST_SHMEM_GET(GET_NBI, 0, int64_t, int64);
  TEST_SHMEM_GET(GET_NBI, 0, uint8_t, uint8);
  TEST_SHMEM_GET(GET_NBI, 0, uint16_t, uint16);
  TEST_SHMEM_GET(GET_NBI, 0, uint32_t, uint32);
  TEST_SHMEM_GET(GET_NBI, 0, uint64_t, uint64);
  TEST_SHMEM_GET(GET_NBI, 0, size_t, size);
  TEST_SHMEM_GET(GET_NBI, 0, ptrdiff_t, ptrdiff);

  TEST_SHMEM_GET(GET_NBI, 1, float, float);
  TEST_SHMEM_GET(GET_NBI, 1, double, double);
  TEST_SHMEM_GET(GET_NBI, 1, long double, longdouble);
  TEST_SHMEM_GET(GET_NBI, 1, char, char);
  TEST_SHMEM_GET(GET_NBI, 1, signed char, schar);
  TEST_SHMEM_GET(GET_NBI, 1, short, short);
  TEST_SHMEM_GET(GET_NBI, 1, int, int);
  TEST_SHMEM_GET(GET_NBI, 1, long, long);
  TEST_SHMEM_GET(GET_NBI, 1, long long, longlong);
  TEST_SHMEM_GET(GET_NBI, 1, unsigned char, uchar);
  TEST_SHMEM_GET(GET_NBI, 1, unsigned short, ushort);
  TEST_SHMEM_GET(GET_NBI, 1, unsigned int, uint);
  TEST_SHMEM_GET(GET_NBI, 1, unsigned long, ulong);
  TEST_SHMEM_GET(GET_NBI, 1, unsigned long long, ulonglong);
  TEST_SHMEM_GET(GET_NBI, 1, int8_t, int8);
  TEST_SHMEM_GET(GET_NBI, 1, int16_t, int16);
  TEST_SHMEM_GET(GET_NBI, 1, int32_t, int32);
  TEST_SHMEM_GET(GET_NBI, 1, int64_t, int64);
  TEST_SHMEM_GET(GET_NBI, 1, uint8_t, uint8);
  TEST_SHMEM_GET(GET_NBI, 1, uint16_t, uint16);
  TEST_SHMEM_GET(GET_NBI, 1, uint32_t, uint32);
  TEST_SHMEM_GET(GET_NBI, 1, uint64_t, uint64);
  TEST_SHMEM_GET(GET_NBI, 1, size_t, size);
  TEST_SHMEM_GET(GET_NBI, 1, ptrdiff_t, ptrdiff);

  shmem_finalize();
  return rc;
}
