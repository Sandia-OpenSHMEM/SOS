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

#ifdef ENABLE_SHMEMX_TESTS
#include <shmemx.h>
#endif

enum op { ADD = 0, ATOMIC_ADD, CTX_ATOMIC_ADD, FADD, ATOMIC_FETCH_ADD,
          CTX_ATOMIC_FETCH_ADD, ATOMIC_FETCH_ADD_NBI, CTX_ATOMIC_FETCH_ADD_NBI };

#ifdef ENABLE_DEPRECATED_TESTS
#define DEPRECATED_ADD(TYPENAME, ...) shmem_##TYPENAME##_add(__VA_ARGS__)
#define DEPRECATED_FADD(TYPENAME, ...) shmem_##TYPENAME##_fadd(__VA_ARGS__)
#else
#define DEPRECATED_ADD(TYPENAME, ...) shmem_##TYPENAME##_atomic_add(__VA_ARGS__)
#define DEPRECATED_FADD(TYPENAME, ...) shmem_##TYPENAME##_atomic_fetch_add(__VA_ARGS__)
#endif

#ifdef ENABLE_SHMEMX_TESTS
#define SHMEMX_NBI_OPS_CASES(OP, TYPE, TYPENAME)                        \
        case ATOMIC_FETCH_ADD_NBI:                                      \
          shmemx_##TYPENAME##_atomic_fetch_add_nbi(&old, &remote,       \
                                                  (TYPE)(mype + 1), i); \
          shmem_quiet();                                                \
          if (old > (TYPE)(npes * (npes + 1) / 2)) {                    \
            printf("PE %i error inconsistent value of old (%s, %s)\n",  \
                   mype, #OP, #TYPE);                                   \
            rc = EXIT_FAILURE;                                          \
          }                                                             \
          break;                                                        \
        case CTX_ATOMIC_FETCH_ADD_NBI:                                  \
          shmemx_ctx_##TYPENAME##_atomic_fetch_add_nbi(SHMEM_CTX_DEFAULT,\
                                    &old, &remote, (TYPE)(mype + 1), i);\
          shmem_quiet();                                                \
          if (old > (TYPE)(npes * (npes + 1) / 2)) {                    \
            printf("PE %i error inconsistent value of old (%s, %s)\n",  \
                   mype, #OP, #TYPE);                                   \
            rc = EXIT_FAILURE;                                          \
          }                                                             \
          break;
#else
#define SHMEMX_NBI_OPS_CASES(OP, TYPE, TYPENAME)
#endif

#define TEST_SHMEM_ADD(OP, TYPE, TYPENAME)                              \
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
          DEPRECATED_ADD(TYPENAME, &remote, (TYPE)(mype + 1), i);       \
          break;                                                        \
        case ATOMIC_ADD:                                                \
          shmem_##TYPENAME##_atomic_add(&remote, (TYPE)(mype + 1), i);  \
          break;                                                        \
        case CTX_ATOMIC_ADD:                                            \
          shmem_ctx_##TYPENAME##_atomic_add(SHMEM_CTX_DEFAULT, &remote, \
                                                  (TYPE)(mype + 1), i); \
          break;                                                        \
        case FADD:                                                      \
          old = DEPRECATED_FADD(TYPENAME, &remote, (TYPE)(mype + 1), i);\
          if (old > (TYPE)(npes * (npes + 1) / 2)) {                    \
            printf("PE %i error inconsistent value of old (%s, %s)\n",  \
                   mype, #OP, #TYPE);                                   \
            rc = EXIT_FAILURE;                                          \
          }                                                             \
          break;                                                        \
        case ATOMIC_FETCH_ADD:                                          \
          old = shmem_##TYPENAME##_atomic_fetch_add(&remote,            \
                                                   (TYPE)(mype + 1), i);\
          if (old > (TYPE)(npes * (npes + 1) / 2)) {                    \
            printf("PE %i error inconsistent value of old (%s, %s)\n",  \
                   mype, #OP, #TYPE);                                   \
            rc = EXIT_FAILURE;                                          \
          }                                                             \
          break;                                                        \
        case CTX_ATOMIC_FETCH_ADD:                                      \
          old = shmem_ctx_##TYPENAME##_atomic_fetch_add(                \
                  SHMEM_CTX_DEFAULT, &remote, (TYPE)(mype + 1), i);     \
          if (old > (TYPE)(npes * (npes + 1) / 2)) {                    \
            printf("PE %i error inconsistent value of old (%s, %s)\n",  \
                   mype, #OP, #TYPE);                                   \
            rc = EXIT_FAILURE;                                          \
          }                                                             \
          break;                                                        \
        SHMEMX_NBI_OPS_CASES(OP, TYPE, TYPENAME)                        \
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


int main(int argc, char* argv[]) {
  shmem_init();

  int rc = EXIT_SUCCESS;

#ifdef ENABLE_DEPRECATED_TESTS
  TEST_SHMEM_ADD(ADD, int, int);
  TEST_SHMEM_ADD(ADD, long, long);
  TEST_SHMEM_ADD(ADD, long long, longlong);
  TEST_SHMEM_ADD(ADD, unsigned int, uint);
  TEST_SHMEM_ADD(ADD, unsigned long, ulong);
  TEST_SHMEM_ADD(ADD, unsigned long long, ulonglong);
  TEST_SHMEM_ADD(ADD, int32_t, int32);
  TEST_SHMEM_ADD(ADD, int64_t, int64);
  TEST_SHMEM_ADD(ADD, uint32_t, uint32);
  TEST_SHMEM_ADD(ADD, uint64_t, uint64);
  TEST_SHMEM_ADD(ADD, size_t, size);
  TEST_SHMEM_ADD(ADD, ptrdiff_t, ptrdiff);
#endif /* ENABLE_DEPRECATED_TESTS */

  TEST_SHMEM_ADD(ATOMIC_ADD, int, int);
  TEST_SHMEM_ADD(ATOMIC_ADD, long, long);
  TEST_SHMEM_ADD(ATOMIC_ADD, long long, longlong);
  TEST_SHMEM_ADD(ATOMIC_ADD, unsigned int, uint);
  TEST_SHMEM_ADD(ATOMIC_ADD, unsigned long, ulong);
  TEST_SHMEM_ADD(ATOMIC_ADD, unsigned long long, ulonglong);
  TEST_SHMEM_ADD(ATOMIC_ADD, int32_t, int32);
  TEST_SHMEM_ADD(ATOMIC_ADD, int64_t, int64);
  TEST_SHMEM_ADD(ATOMIC_ADD, uint32_t, uint32);
  TEST_SHMEM_ADD(ATOMIC_ADD, uint64_t, uint64);
  TEST_SHMEM_ADD(ATOMIC_ADD, size_t, size);
  TEST_SHMEM_ADD(ATOMIC_ADD, ptrdiff_t, ptrdiff);

  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, int, int);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, long, long);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, long long, longlong);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, unsigned int, uint);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, unsigned long, ulong);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, unsigned long long, ulonglong);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, int32_t, int32);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, int64_t, int64);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, uint32_t, uint32);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, uint64_t, uint64);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, size_t, size);
  TEST_SHMEM_ADD(CTX_ATOMIC_ADD, ptrdiff_t, ptrdiff);

  TEST_SHMEM_ADD(FADD, int, int);
  TEST_SHMEM_ADD(FADD, long, long);
  TEST_SHMEM_ADD(FADD, long long, longlong);
  TEST_SHMEM_ADD(FADD, unsigned int, uint);
  TEST_SHMEM_ADD(FADD, unsigned long, ulong);
  TEST_SHMEM_ADD(FADD, unsigned long long, ulonglong);
  TEST_SHMEM_ADD(FADD, int32_t, int32);
  TEST_SHMEM_ADD(FADD, int64_t, int64);
  TEST_SHMEM_ADD(FADD, uint32_t, uint32);
  TEST_SHMEM_ADD(FADD, uint64_t, uint64);
  TEST_SHMEM_ADD(FADD, size_t, size);
  TEST_SHMEM_ADD(FADD, ptrdiff_t, ptrdiff);

  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, int, int);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, long, long);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, long long, longlong);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, unsigned int, uint);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, unsigned long, ulong);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, unsigned long long, ulonglong);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, int32_t, int32);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, int64_t, int64);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, uint32_t, uint32);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, uint64_t, uint64);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, size_t, size);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD, ptrdiff_t, ptrdiff);

  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, int, int);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, long, long);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, long long, longlong);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, unsigned int, uint);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, unsigned long, ulong);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, unsigned long long, ulonglong);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, int32_t, int32);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, int64_t, int64);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, uint32_t, uint32);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, uint64_t, uint64);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, size_t, size);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD, ptrdiff_t, ptrdiff);

#ifdef ENABLE_SHMEMX_TESTS
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD_NBI, int, int);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD_NBI, long, long);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD_NBI, long long, longlong);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD_NBI, unsigned int, uint);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD_NBI, unsigned long, ulong);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD_NBI, unsigned long long, ulonglong);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD_NBI, int32_t, int32);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD_NBI, int64_t, int64);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD_NBI, uint32_t, uint32);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD_NBI, uint64_t, uint64);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD_NBI, size_t, size);
  TEST_SHMEM_ADD(ATOMIC_FETCH_ADD_NBI, ptrdiff_t, ptrdiff);

  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD_NBI, int, int);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD_NBI, long, long);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD_NBI, long long, longlong);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD_NBI, unsigned int, uint);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD_NBI, unsigned long, ulong);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD_NBI, unsigned long long, ulonglong);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD_NBI, int32_t, int32);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD_NBI, int64_t, int64);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD_NBI, uint32_t, uint32);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD_NBI, uint64_t, uint64);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD_NBI, size_t, size);
  TEST_SHMEM_ADD(CTX_ATOMIC_FETCH_ADD_NBI, ptrdiff_t, ptrdiff);
#endif

  shmem_finalize();
  return rc;
}
