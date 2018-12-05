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

enum op { SET = 0, ATOMIC_SET, CTX_ATOMIC_SET };

#ifdef ENABLE_DEPRECATED_TESTS
#define DEPRECATED_SET shmem_set
#else
#define DEPRECATED_SET shmem_atomic_set
#endif

#define TEST_SHMEM_SET(OP, TYPE)                        \
  do {                                                  \
    static TYPE remote;                                 \
    const int mype = shmem_my_pe();                     \
    const int npes = shmem_n_pes();                     \
    switch (OP) {                                       \
      case SET:                                         \
        DEPRECATED_SET(&remote, (TYPE)mype, (mype + 1) % npes); \
        break;                                          \
      case ATOMIC_SET:                                  \
        shmem_atomic_set(&remote, (TYPE)mype, (mype + 1) % npes); \
        break;                                          \
      case CTX_ATOMIC_SET:                              \
        shmem_atomic_set(SHMEM_CTX_DEFAULT, &remote, (TYPE)mype, (mype + 1) % npes); \
        break;                                          \
      default:                                          \
        printf("Invalid operation (%d)\n", OP);         \
        shmem_global_exit(1);                           \
    }                                                   \
    shmem_barrier_all();                                \
    if (remote != (TYPE)((mype + npes - 1) % npes)) {   \
      printf("PE %i received incorrect value with "     \
             "TEST_SHMEM_SET(%s, %s)\n", mype, #OP, #TYPE); \
      rc = EXIT_FAILURE;                                \
    }                                                   \
  } while (false)


int main(int argc, char* argv[]) {
  shmem_init();

  int rc = EXIT_SUCCESS;

#ifdef ENABLE_DEPRECATED_TESTS
  TEST_SHMEM_SET(SET, float);
  TEST_SHMEM_SET(SET, double);
  TEST_SHMEM_SET(SET, int);
  TEST_SHMEM_SET(SET, long);
  TEST_SHMEM_SET(SET, long long);
  TEST_SHMEM_SET(SET, unsigned int);
  TEST_SHMEM_SET(SET, unsigned long);
  TEST_SHMEM_SET(SET, unsigned long long);
  TEST_SHMEM_SET(SET, int32_t);
  TEST_SHMEM_SET(SET, int64_t);
  TEST_SHMEM_SET(SET, uint32_t);
  TEST_SHMEM_SET(SET, uint64_t);
  TEST_SHMEM_SET(SET, size_t);
  TEST_SHMEM_SET(SET, ptrdiff_t);
#endif /* ENABLE_DEPRECATED_TESTS */

  TEST_SHMEM_SET(ATOMIC_SET, float);
  TEST_SHMEM_SET(ATOMIC_SET, double);
  TEST_SHMEM_SET(ATOMIC_SET, int);
  TEST_SHMEM_SET(ATOMIC_SET, long);
  TEST_SHMEM_SET(ATOMIC_SET, long long);
  TEST_SHMEM_SET(ATOMIC_SET, unsigned int);
  TEST_SHMEM_SET(ATOMIC_SET, unsigned long);
  TEST_SHMEM_SET(ATOMIC_SET, unsigned long long);
  TEST_SHMEM_SET(ATOMIC_SET, int32_t);
  TEST_SHMEM_SET(ATOMIC_SET, int64_t);
  TEST_SHMEM_SET(ATOMIC_SET, uint32_t);
  TEST_SHMEM_SET(ATOMIC_SET, uint64_t);
  TEST_SHMEM_SET(ATOMIC_SET, size_t);
  TEST_SHMEM_SET(ATOMIC_SET, ptrdiff_t);

  TEST_SHMEM_SET(CTX_ATOMIC_SET, float);
  TEST_SHMEM_SET(CTX_ATOMIC_SET, double);
  TEST_SHMEM_SET(CTX_ATOMIC_SET, int);
  TEST_SHMEM_SET(CTX_ATOMIC_SET, long);
  TEST_SHMEM_SET(CTX_ATOMIC_SET, long long);
  TEST_SHMEM_SET(CTX_ATOMIC_SET, unsigned int);
  TEST_SHMEM_SET(CTX_ATOMIC_SET, unsigned long);
  TEST_SHMEM_SET(CTX_ATOMIC_SET, unsigned long long);
  TEST_SHMEM_SET(CTX_ATOMIC_SET, int32_t);
  TEST_SHMEM_SET(CTX_ATOMIC_SET, int64_t);
  TEST_SHMEM_SET(CTX_ATOMIC_SET, uint32_t);
  TEST_SHMEM_SET(CTX_ATOMIC_SET, uint64_t);
  TEST_SHMEM_SET(CTX_ATOMIC_SET, size_t);
  TEST_SHMEM_SET(CTX_ATOMIC_SET, ptrdiff_t);

  shmem_finalize();
  return rc;
}
