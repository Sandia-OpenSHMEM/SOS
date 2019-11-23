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
#include <stdio.h>
#include <shmem.h>
#include <shmemx.h>

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L

#define TEST_SHMEM_WAIT_UNTIL_ALL(TYPE)                                      \
  do {                                                                       \
    static TYPE remote = 0;                                                  \
    const int mype = shmem_my_pe();                                          \
    const int npes = shmem_n_pes();                                          \
    int status = 0;                                                          \
    shmem_p(&remote, (TYPE)mype+1, (mype + 1) % npes);                       \
    shmemx_wait_until_all(&remote, 1, &status, SHMEM_CMP_NE, 0);             \
    if (remote != (TYPE)((mype + npes - 1) % npes)+1) {                      \
      printf("PE %i received incorrect value with "                          \
             "TEST_SHMEM_WAIT_UNTIL_ALL(%s)\n", mype, #TYPE);                \
      rc = EXIT_FAILURE;                                                     \
    }                                                                        \
  } while (0)

#define TEST_SHMEM_WAIT_UNTIL_ANY(TYPE)                                      \
  do {                                                                       \
    static TYPE remote = 0;                                                  \
    const int mype = shmem_my_pe();                                          \
    const int npes = shmem_n_pes();                                          \
    int status = 0;                                                          \
    shmem_p(&remote, (TYPE)mype+1, (mype + 1) % npes);                       \
    shmemx_wait_until_any(&remote, 1, &status, SHMEM_CMP_NE, 0);             \
    if (remote != (TYPE)((mype + npes - 1) % npes)+1) {                      \
      printf("PE %i received incorrect value with "                          \
             "TEST_SHMEM_WAIT_UNTIL_ANY(%s)\n", mype, #TYPE);                \
      rc = EXIT_FAILURE;                                                     \
    }                                                                        \
  } while (0)

#define TEST_SHMEM_WAIT_UNTIL_SOME(TYPE)                                     \
  do {                                                                       \
    static TYPE remote = 0;                                                  \
    const int mype = shmem_my_pe();                                          \
    const int npes = shmem_n_pes();                                          \
    int status = 0;                                                          \
    size_t indices;                                                          \
    shmem_p(&remote, (TYPE)mype+1, (mype + 1) % npes);                       \
    shmemx_wait_until_some(&remote, 1, &indices, &status, SHMEM_CMP_NE, 0);  \
    if (remote != (TYPE)((mype + npes - 1) % npes)+1) {                      \
      printf("PE %i received incorrect value with "                          \
             "TEST_SHMEM_WAIT_UNTIL_SOME(%s)\n", mype, #TYPE);               \
      rc = EXIT_FAILURE;                                                     \
    }                                                                        \
  } while (0)

#else
#define TEST_SHMEM_WAIT_UNTIL_ALL(TYPE)
#define TEST_SHMEM_WAIT_UNTIL_ANY(TYPE)
#define TEST_SHMEM_WAIT_UNTIL_SOME(TYPE)

#endif

int main(int argc, char* argv[]) {
  shmem_init();

  int rc = EXIT_SUCCESS;
  TEST_SHMEM_WAIT_UNTIL_ALL(short);
  TEST_SHMEM_WAIT_UNTIL_ALL(int);
  TEST_SHMEM_WAIT_UNTIL_ALL(long);
  TEST_SHMEM_WAIT_UNTIL_ALL(long long);
  TEST_SHMEM_WAIT_UNTIL_ALL(unsigned short);
  TEST_SHMEM_WAIT_UNTIL_ALL(unsigned int);
  TEST_SHMEM_WAIT_UNTIL_ALL(unsigned long);
  TEST_SHMEM_WAIT_UNTIL_ALL(unsigned long long);
  TEST_SHMEM_WAIT_UNTIL_ALL(int32_t);
  TEST_SHMEM_WAIT_UNTIL_ALL(int64_t);
  TEST_SHMEM_WAIT_UNTIL_ALL(uint32_t);
  TEST_SHMEM_WAIT_UNTIL_ALL(uint64_t);
  TEST_SHMEM_WAIT_UNTIL_ALL(size_t);
  TEST_SHMEM_WAIT_UNTIL_ALL(ptrdiff_t);

  TEST_SHMEM_WAIT_UNTIL_ANY(short);
  TEST_SHMEM_WAIT_UNTIL_ANY(int);
  TEST_SHMEM_WAIT_UNTIL_ANY(long);
  TEST_SHMEM_WAIT_UNTIL_ANY(long long);
  TEST_SHMEM_WAIT_UNTIL_ANY(unsigned short);
  TEST_SHMEM_WAIT_UNTIL_ANY(unsigned int);
  TEST_SHMEM_WAIT_UNTIL_ANY(unsigned long);
  TEST_SHMEM_WAIT_UNTIL_ANY(unsigned long long);
  TEST_SHMEM_WAIT_UNTIL_ANY(int32_t);
  TEST_SHMEM_WAIT_UNTIL_ANY(int64_t);
  TEST_SHMEM_WAIT_UNTIL_ANY(uint32_t);
  TEST_SHMEM_WAIT_UNTIL_ANY(uint64_t);
  TEST_SHMEM_WAIT_UNTIL_ANY(size_t);
  TEST_SHMEM_WAIT_UNTIL_ANY(ptrdiff_t);

  TEST_SHMEM_WAIT_UNTIL_SOME(short);
  TEST_SHMEM_WAIT_UNTIL_SOME(int);
  TEST_SHMEM_WAIT_UNTIL_SOME(long);
  TEST_SHMEM_WAIT_UNTIL_SOME(long long);
  TEST_SHMEM_WAIT_UNTIL_SOME(unsigned short);
  TEST_SHMEM_WAIT_UNTIL_SOME(unsigned int);
  TEST_SHMEM_WAIT_UNTIL_SOME(unsigned long);
  TEST_SHMEM_WAIT_UNTIL_SOME(unsigned long long);
  TEST_SHMEM_WAIT_UNTIL_SOME(int32_t);
  TEST_SHMEM_WAIT_UNTIL_SOME(int64_t);
  TEST_SHMEM_WAIT_UNTIL_SOME(uint32_t);
  TEST_SHMEM_WAIT_UNTIL_SOME(uint64_t);
  TEST_SHMEM_WAIT_UNTIL_SOME(size_t);
  TEST_SHMEM_WAIT_UNTIL_SOME(ptrdiff_t);

  shmem_finalize();
  return rc;
}
