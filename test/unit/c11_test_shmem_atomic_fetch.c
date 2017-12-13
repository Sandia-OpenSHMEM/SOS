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

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L

enum op { FETCH = 0, ATOMIC_FETCH, CTX_ATOMIC_FETCH };

#ifdef ENABLE_DEPRECATED_TESTS
#define DEPRECATED_FETCH shmem_fetch
#else
#define DEPRECATED_FETCH shmem_atomic_fetch
#endif

#define TEST_SHMEM_FETCH(OP, TYPE)                              \
  do {                                                          \
    static TYPE remote;                                         \
    TYPE val;                                                   \
    const int mype = shmem_my_pe();                             \
    const int npes = shmem_n_pes();                             \
    remote = (TYPE)mype;                                        \
    shmem_barrier_all();                                        \
    switch (OP) {                                               \
      case FETCH:                                               \
        val = DEPRECATED_FETCH(&remote, (mype + 1) % npes);     \
        break;                                                  \
      case ATOMIC_FETCH:                                        \
        val = shmem_atomic_fetch(&remote, (mype + 1) % npes);   \
        break;                                                  \
      case CTX_ATOMIC_FETCH:                                    \
        val = shmem_atomic_fetch(SHMEM_CTX_DEFAULT, &remote, (mype + 1) % npes); \
        break;                                                  \
      default:                                                  \
        printf("Invalid operation (%d)\n", OP);                 \
        shmem_global_exit(1);                                   \
    }                                                           \
    if (val != (TYPE)((mype + 1) % npes)) {                     \
      printf("PE %i received incorrect value with "             \
             "TEST_SHMEM_FETCH(%s, %s)\n", mype, #OP, #TYPE);   \
      rc = EXIT_FAILURE;                                        \
    }                                                           \
  } while (false)

#else
#define TEST_SHMEM_FETCH(OP, TYPE)

#endif

int main(int argc, char* argv[]) {
  shmem_init();

  int rc = EXIT_SUCCESS;

#ifdef ENABLE_DEPRECATED_TESTS
  TEST_SHMEM_FETCH(FETCH, float);
  TEST_SHMEM_FETCH(FETCH, double);
  TEST_SHMEM_FETCH(FETCH, int);
  TEST_SHMEM_FETCH(FETCH, long);
  TEST_SHMEM_FETCH(FETCH, long long);
  TEST_SHMEM_FETCH(FETCH, unsigned int);
  TEST_SHMEM_FETCH(FETCH, unsigned long);
  TEST_SHMEM_FETCH(FETCH, unsigned long long);
  TEST_SHMEM_FETCH(FETCH, int32_t);
  TEST_SHMEM_FETCH(FETCH, int64_t);
  TEST_SHMEM_FETCH(FETCH, uint32_t);
  TEST_SHMEM_FETCH(FETCH, uint64_t);
  TEST_SHMEM_FETCH(FETCH, size_t);
  TEST_SHMEM_FETCH(FETCH, ptrdiff_t);
#endif /* ENABLE_DEPRECATED_TESTS */

  TEST_SHMEM_FETCH(ATOMIC_FETCH, float);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, double);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, int);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, long);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, long long);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, unsigned int);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, unsigned long);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, unsigned long long);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, int32_t);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, int64_t);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, uint32_t);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, uint64_t);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, size_t);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, ptrdiff_t);

  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, float);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, double);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, int);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, long);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, long long);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, unsigned int);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, unsigned long);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, unsigned long long);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, int32_t);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, int64_t);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, uint32_t);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, uint64_t);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, size_t);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, ptrdiff_t);

  shmem_finalize();
  return rc;
}
