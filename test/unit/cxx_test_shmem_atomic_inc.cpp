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

enum op { INC = 0, ATOMIC_INC, CTX_ATOMIC_INC, FINC, ATOMIC_FETCH_INC,
          CTX_ATOMIC_FETCH_INC };

#ifdef ENABLE_DEPRECATED_TESTS
#define DEPRECATED_INC shmem_inc
#define DEPRECATED_FINC shmem_finc
#else
#define DEPRECATED_INC shmem_atomic_inc
#define DEPRECATED_FINC shmem_atomic_fetch_inc
#endif

#define TEST_SHMEM_INC(OP, TYPE)                                        \
  do {                                                                  \
    static TYPE remote = (TYPE)0;                                       \
    TYPE old;                                                           \
    const int mype = shmem_my_pe();                                     \
    const int npes = shmem_n_pes();                                     \
    remote = (TYPE)0;                                                   \
    shmem_barrier_all();                                                \
    for (int i = 0; i < npes; i++)                                      \
      switch (OP) {                                                     \
        case INC:                                                       \
          DEPRECATED_INC(&remote, i);                                   \
          break;                                                        \
        case ATOMIC_INC:                                                \
          shmem_atomic_inc(&remote, i);                                 \
          break;                                                        \
        case CTX_ATOMIC_INC:                                            \
          shmem_atomic_inc(SHMEM_CTX_DEFAULT, &remote, i);              \
          break;                                                        \
        case FINC:                                                      \
          old = DEPRECATED_FINC(&remote, i);                            \
          if (old > npes) {                                             \
            printf("PE %i error inconsistent value of old (%s, %s)\n",  \
                   mype, #OP, #TYPE);                                   \
            rc = EXIT_FAILURE;                                          \
          }                                                             \
          break;                                                        \
        case ATOMIC_FETCH_INC:                                          \
          old = shmem_atomic_fetch_inc(&remote, i);                     \
          if (old > npes) {                                             \
            printf("PE %i error inconsistent value of old (%s, %s)\n",  \
                   mype, #OP, #TYPE);                                   \
            rc = EXIT_FAILURE;                                          \
          }                                                             \
          break;                                                        \
        case CTX_ATOMIC_FETCH_INC:                                      \
          old = shmem_atomic_fetch_inc(SHMEM_CTX_DEFAULT, &remote, i);  \
          if (old > npes) {                                             \
            printf("PE %i error inconsistent value of old (%s, %s)\n",  \
                   mype, #OP, #TYPE);                                   \
            rc = EXIT_FAILURE;                                          \
          }                                                             \
          break;                                                        \
        default:                                                        \
          printf("Invalid operation (%d)\n", OP);                       \
          shmem_global_exit(1);                                         \
      }                                                                 \
    shmem_barrier_all();                                                \
    if (remote != (TYPE)npes) {                                         \
      printf("PE %i observed error with TEST_SHMEM_INC(%s, %s)\n",      \
              mype, #OP, #TYPE);                                        \
      rc = EXIT_FAILURE;                                                \
    }                                                                   \
  } while (false)


int main(int argc, char* argv[]) {
  shmem_init();

  int rc = EXIT_SUCCESS;

#ifdef ENABLE_DEPRECATED_TESTS
  TEST_SHMEM_INC(INC, int);
  TEST_SHMEM_INC(INC, long);
  TEST_SHMEM_INC(INC, long long);
  TEST_SHMEM_INC(INC, unsigned int);
  TEST_SHMEM_INC(INC, unsigned long);
  TEST_SHMEM_INC(INC, unsigned long long);
  TEST_SHMEM_INC(INC, int32_t);
  TEST_SHMEM_INC(INC, int64_t);
  TEST_SHMEM_INC(INC, uint32_t);
  TEST_SHMEM_INC(INC, uint64_t);
  TEST_SHMEM_INC(INC, size_t);
  TEST_SHMEM_INC(INC, ptrdiff_t);
#endif /* ENABLE_DEPRECATED_TESTS */

  TEST_SHMEM_INC(ATOMIC_INC, int);
  TEST_SHMEM_INC(ATOMIC_INC, long);
  TEST_SHMEM_INC(ATOMIC_INC, long long);
  TEST_SHMEM_INC(ATOMIC_INC, unsigned int);
  TEST_SHMEM_INC(ATOMIC_INC, unsigned long);
  TEST_SHMEM_INC(ATOMIC_INC, unsigned long long);
  TEST_SHMEM_INC(ATOMIC_INC, int32_t);
  TEST_SHMEM_INC(ATOMIC_INC, int64_t);
  TEST_SHMEM_INC(ATOMIC_INC, uint32_t);
  TEST_SHMEM_INC(ATOMIC_INC, uint64_t);
  TEST_SHMEM_INC(ATOMIC_INC, size_t);
  TEST_SHMEM_INC(ATOMIC_INC, ptrdiff_t);

  TEST_SHMEM_INC(CTX_ATOMIC_INC, int);
  TEST_SHMEM_INC(CTX_ATOMIC_INC, long);
  TEST_SHMEM_INC(CTX_ATOMIC_INC, long long);
  TEST_SHMEM_INC(CTX_ATOMIC_INC, unsigned int);
  TEST_SHMEM_INC(CTX_ATOMIC_INC, unsigned long);
  TEST_SHMEM_INC(CTX_ATOMIC_INC, unsigned long long);
  TEST_SHMEM_INC(CTX_ATOMIC_INC, int32_t);
  TEST_SHMEM_INC(CTX_ATOMIC_INC, int64_t);
  TEST_SHMEM_INC(CTX_ATOMIC_INC, uint32_t);
  TEST_SHMEM_INC(CTX_ATOMIC_INC, uint64_t);
  TEST_SHMEM_INC(CTX_ATOMIC_INC, size_t);
  TEST_SHMEM_INC(CTX_ATOMIC_INC, ptrdiff_t);

  TEST_SHMEM_INC(FINC, int);
  TEST_SHMEM_INC(FINC, long);
  TEST_SHMEM_INC(FINC, long long);
  TEST_SHMEM_INC(FINC, unsigned int);
  TEST_SHMEM_INC(FINC, unsigned long);
  TEST_SHMEM_INC(FINC, unsigned long long);
  TEST_SHMEM_INC(FINC, int32_t);
  TEST_SHMEM_INC(FINC, int64_t);
  TEST_SHMEM_INC(FINC, uint32_t);
  TEST_SHMEM_INC(FINC, uint64_t);
  TEST_SHMEM_INC(FINC, size_t);
  TEST_SHMEM_INC(FINC, ptrdiff_t);

  TEST_SHMEM_INC(ATOMIC_FETCH_INC, int);
  TEST_SHMEM_INC(ATOMIC_FETCH_INC, long);
  TEST_SHMEM_INC(ATOMIC_FETCH_INC, long long);
  TEST_SHMEM_INC(ATOMIC_FETCH_INC, unsigned int);
  TEST_SHMEM_INC(ATOMIC_FETCH_INC, unsigned long);
  TEST_SHMEM_INC(ATOMIC_FETCH_INC, unsigned long long);
  TEST_SHMEM_INC(ATOMIC_FETCH_INC, int32_t);
  TEST_SHMEM_INC(ATOMIC_FETCH_INC, int64_t);
  TEST_SHMEM_INC(ATOMIC_FETCH_INC, uint32_t);
  TEST_SHMEM_INC(ATOMIC_FETCH_INC, uint64_t);
  TEST_SHMEM_INC(ATOMIC_FETCH_INC, size_t);
  TEST_SHMEM_INC(ATOMIC_FETCH_INC, ptrdiff_t);

  TEST_SHMEM_INC(CTX_ATOMIC_FETCH_INC, int);
  TEST_SHMEM_INC(CTX_ATOMIC_FETCH_INC, long);
  TEST_SHMEM_INC(CTX_ATOMIC_FETCH_INC, long long);
  TEST_SHMEM_INC(CTX_ATOMIC_FETCH_INC, unsigned int);
  TEST_SHMEM_INC(CTX_ATOMIC_FETCH_INC, unsigned long);
  TEST_SHMEM_INC(CTX_ATOMIC_FETCH_INC, unsigned long long);
  TEST_SHMEM_INC(CTX_ATOMIC_FETCH_INC, int32_t);
  TEST_SHMEM_INC(CTX_ATOMIC_FETCH_INC, int64_t);
  TEST_SHMEM_INC(CTX_ATOMIC_FETCH_INC, uint32_t);
  TEST_SHMEM_INC(CTX_ATOMIC_FETCH_INC, uint64_t);
  TEST_SHMEM_INC(CTX_ATOMIC_FETCH_INC, size_t);
  TEST_SHMEM_INC(CTX_ATOMIC_FETCH_INC, ptrdiff_t);

  shmem_finalize();
  return rc;
}
