/*
 *  This test program is derived from a unit test created by Nick Park.
 *  The original unit test is a work of the U.S. Government and is not subject
 *  to copyright protection in the United States.  Foreign copyrights may
 *  apply.
 *
 *  Copyright (c) 2016 Intel Corporation. All rights reserved.
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

#define TEST_SHMEM_GET(TYPE)                                    \
  do {                                                          \
    static TYPE remote[10];                                     \
    const int mype = shmem_my_pe();                             \
    const int npes = shmem_n_pes();                             \
    TYPE local[10];                                             \
    for (int i = 0; i < 10; i++)                                \
      remote[i] = (TYPE)mype;                                   \
    shmem_barrier_all();                                        \
    shmem_get(local, remote, 10, (mype + 1) % npes);            \
    for (int i = 0; i < 10; i++)                                \
      if (local[i] != (TYPE)((mype + 1) % npes)) {              \
        fprintf(stderr,                                         \
                "PE %i received incorrect value "               \
                "for shmem_get(%s,...)\n", mype, #TYPE);        \
        rc = EXIT_FAILURE;                                      \
      }                                                         \
  } while (false)

int main(int argc, char* argv[]) {
  shmem_init();

  int rc = EXIT_SUCCESS;
  TEST_SHMEM_GET(float);
  TEST_SHMEM_GET(double);
  TEST_SHMEM_GET(long double);
  TEST_SHMEM_GET(char);
  TEST_SHMEM_GET(signed char);
  TEST_SHMEM_GET(short);
  TEST_SHMEM_GET(int);
  TEST_SHMEM_GET(long);
  TEST_SHMEM_GET(long long);
  TEST_SHMEM_GET(unsigned char);
  TEST_SHMEM_GET(unsigned short);
  TEST_SHMEM_GET(unsigned int);
  TEST_SHMEM_GET(unsigned long);
  TEST_SHMEM_GET(unsigned long long);
  TEST_SHMEM_GET(int8_t);
  TEST_SHMEM_GET(int16_t);
  TEST_SHMEM_GET(int32_t);
  TEST_SHMEM_GET(int64_t);
  TEST_SHMEM_GET(uint8_t);
  TEST_SHMEM_GET(uint16_t);
  TEST_SHMEM_GET(uint32_t);
  TEST_SHMEM_GET(uint64_t);
  TEST_SHMEM_GET(size_t);
  TEST_SHMEM_GET(ptrdiff_t);

  shmem_finalize();
  return rc;
}
