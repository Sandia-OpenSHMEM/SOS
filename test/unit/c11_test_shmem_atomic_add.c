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

enum op { ADD = 0, ATOMIC_ADD, CTX_ATOMIC_ADD, FADD, ATOMIC_FETCH_ADD,
          CTX_ATOMIC_FETCH_ADD };

#ifdef ENABLE_DEPRECATED_TESTS
#define DEPRECATED_ADD shmem_add
#define DEPRECATED_FADD shmem_fadd
#else
#define DEPRECATED_ADD shmem_atomic_add
#define DEPRECATED_FADD shmem_atomic_fetch_add
#endif

#define TEST_SHMEM_ADD(OP, TYPE)                                        \
  do {                                                                  \
    static TYPE remote;                                                 \
    TYPE old;                                                           \
    const int mype = shmem_my_pe();                                     \
    const int npes = shmem_n_pes();                                     \
    remote = (TYPE)0;                                                   \
    shmem_barrier_all();                                                \
    for (int i = 0; i < npes; i++)                                      \
      switch (OP) {                                                     \
        case ADD:                                                       \
          DEPRECATED_ADD(&remote, (TYPE)(mype + 1), i);                 \
          break;                                                        \
        case ATOMIC_ADD:                                                \
          shmem_atomic_add(&remote, (TYPE)(mype + 1), i);               \
          break;                                                        \
        case CTX_ATOMIC_ADD:                                            \
          shmem_atomic_add(SHMEM_CTX_DEFAULT, &remote, (TYPE)(mype + 1), i); \
          break;                                                        \
        case FADD:                                                      \
          old = DEPRECATED_FADD(&remote, (TYPE)(mype + 1), i);          \
          if (old > (TYPE)(npes * (npes + 1) / 2)) {                    \
            printf("PE %i error inconsistent value of old (%s, %s)\n",  \
                   mype, #OP, #TYPE);                                   \
            rc = EXIT_FAILURE;                                          \
          }                                                             \
          break;                                                        \
        case ATOMIC_FETCH_ADD:                                          \
          old = shmem_atomic_fetch_add(&remote, (TYPE)(mype + 1), i);   \
          if (old > (TYPE)(npes * (npes + 1) / 2)) {                    \
            printf("PE %i error inconsistent value of old (%s, %s)\n",  \
                   mype, #OP, #TYPE);                                   \
            rc = EXIT_FAILURE;                                          \
          }                                                             \
          break;                                                        \
        case CTX_ATOMIC_FETCH_ADD:                                      \
          old = shmem_atomic_fetch_add(SHMEM_CTX_DEFAULT, &remote, (TYPE)(mype + 1), i); \
          if (old > (TYPE)(npes * (npes + 1) / 2)) {                    \
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
    if (remote != (TYPE)(npes * (npes + 1) / 2)) {                      \
      printf("PE %i observed error with TEST_SHMEM_ADD(%s, %s)\n",      \
             mype, #OP, #TYPE);                                         \
      rc = EXIT_FAILURE;                                                \
    }                                                                   \
  } while (false)

#else
#define TEST_SHMEM_ADD(OP, TYPE)

#endif

int main(int argc, char* argv[]) {
  shmem_init();

  int rc = EXIT_SUCCESS;

#ifdef ENABLE_DEPRECATED_TESTS
  TEST_SHMEM_ADD(ADD, int);
  TEST_SHMEM_ADD(ADD, long);
  TEST_SHMEM_ADD(ADD, long long);
  TEST_SHMEM_ADD(ADD, unsigned int);
  TEST_SHMEM_ADD(ADD, unsigned long);
  TEST_SHMEM_ADD(ADD, unsigned long long);
  TEST_SHMEM_ADD(ADD, int32_t);
  TEST_SHMEM_ADD(ADD, int64_t);
  TEST_SHMEM_ADD(ADD, uint32_t);
  TEST_SHMEM_ADD(ADD, uint64_t);
  TEST_SHMEM_ADD(ADD, size_t);
  TEST_SHMEM_ADD(ADD, ptrdiff_t);
#endif /* ENABLE_DEPRECATED_TESTS */

  TEST_SHMEM_ADD(ATOMIC_ADD, int);
  TEST_SHMEM_ADD(ATOMIC_ADD, long);
  TEST_SHMEM_ADD(ATOMIC_ADD, long long);
  TEST_SHMEM_ADD(ATOMIC_ADD, unsigned int);
  TEST_SHMEM_ADD(ATOMIC_ADD, unsigned long);
  TEST_SHMEM_ADD(ATOMIC_ADD, unsigned long long);
  TEST_SHMEM_ADD(ATOMIC_ADD, int32_t);
  TEST_SHMEM_ADD(ATOMIC_ADD, int64_t);
  TEST_SHMEM_ADD(ATOMIC_ADD, uint32_t);
  TEST_SHMEM_ADD(ATOMIC_ADD, uint64_t);
  TEST_SHMEM_ADD(ATOMIC_ADD, size_t);
  TEST_SHMEM_ADD(ATOMIC_ADD, ptrdiff_t);

  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, int);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, long);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, long long);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, unsigned int);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, unsigned long);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, unsigned long long);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, int32_t);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, int64_t);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, uint32_t);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, uint64_t);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, size_t);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, ptrdiff_t);

  TEST_SHMEM_ADD(FADD, int);
  TEST_SHMEM_ADD(FADD, long);
  TEST_SHMEM_ADD(FADD, long long);
  TEST_SHMEM_ADD(FADD, unsigned int);
  TEST_SHMEM_ADD(FADD, unsigned long);
  TEST_SHMEM_ADD(FADD, unsigned long long);
  TEST_SHMEM_ADD(FADD, int32_t);
  TEST_SHMEM_ADD(FADD, int64_t);
  TEST_SHMEM_ADD(FADD, uint32_t);
  TEST_SHMEM_ADD(FADD, uint64_t);
  TEST_SHMEM_ADD(FADD, size_t);
  TEST_SHMEM_ADD(FADD, ptrdiff_t);

  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, int);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, long);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, long long);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, unsigned int);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, unsigned long);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, unsigned long long);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, int32_t);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, int64_t);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, uint32_t);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, uint64_t);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, size_t);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, ptrdiff_t);

  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, int);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, long);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, long long);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, unsigned int);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, unsigned long);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, unsigned long long);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, int32_t);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, int64_t);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, uint32_t);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, uint64_t);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, size_t);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, ptrdiff_t);

  shmem_finalize();
  return rc;
}
