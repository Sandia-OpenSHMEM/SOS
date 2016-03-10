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

#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L)

#define TEST_SHMEM_P(TYPE)                              \
  do {                                                  \
    static TYPE remote;                                 \
    const int mype = shmem_my_pe();                     \
    const int npes = shmem_n_pes();                     \
    shmem_p(&remote, (TYPE)mype, (mype + 1) % npes);    \
    shmem_barrier_all();                                \
    if (remote != (TYPE)((mype + npes - 1) % npes)) {   \
      fprintf(stderr,                                   \
              "PE %i received incorrect value "         \
              "for shmem_p(%s, ...)\n", mype, #TYPE);   \
      rc = EXIT_FAILURE;                                \
    }                                                   \
  } while (false)

#else
#define TEST_SHMEM_P(TYPE)

#endif

int main(int argc, char* argv[]) {
  shmem_init();

  int rc = EXIT_SUCCESS;
  TEST_SHMEM_P(float);
  TEST_SHMEM_P(double);
  TEST_SHMEM_P(long double);
  TEST_SHMEM_P(char);
  TEST_SHMEM_P(short);
  TEST_SHMEM_P(int);
  TEST_SHMEM_P(long);
  TEST_SHMEM_P(long long);

  shmem_finalize();
  return rc;
}
