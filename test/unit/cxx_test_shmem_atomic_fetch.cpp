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

enum op { FETCH = 0, ATOMIC_FETCH, CTX_ATOMIC_FETCH, ATOMIC_FETCH_NBI,
          CTX_ATOMIC_FETCH_NBI };

#ifdef ENABLE_DEPRECATED_TESTS
#define DEPRECATED_FETCH(TYPENAME,...) shmem_##TYPENAME##_fetch(__VA_ARGS__)
#else
#define DEPRECATED_FETCH(TYPENAME,...) shmem_##TYPENAME##_atomic_fetch(__VA_ARGS__)
#endif

#ifdef ENABLE_SHMEMX_TESTS
#define SHMEMX_NBI_OPS_CASES(OP, TYPE, TYPENAME)                \
      case ATOMIC_FETCH_NBI:                                    \
        shmemx_##TYPENAME##_atomic_fetch_nbi(&val, &remote,     \
                                (mype + 1) % npes);             \
        shmem_quiet();                                          \
        break;                                                  \
      case CTX_ATOMIC_FETCH_NBI:                                \
        shmemx_ctx_##TYPENAME##_atomic_fetch_nbi(SHMEM_CTX_DEFAULT,\
                          &val, &remote, (mype + 1) % npes);    \
        shmem_quiet();                                          \
        break;
#else
#define SHMEMX_NBI_OPS_CASES(OP, TYPE, TYPENAME)
#endif

#define TEST_SHMEM_FETCH(OP, TYPE, TYPENAME)                    \
  do {                                                          \
    static TYPE remote;                                         \
    TYPE val;                                                   \
    const int mype = shmem_my_pe();                             \
    const int npes = shmem_n_pes();                             \
    remote = (TYPE)mype;                                        \
    shmem_barrier_all();                                        \
    switch (OP) {                                               \
      case FETCH:                                               \
        val = DEPRECATED_FETCH(TYPENAME, &remote,               \
                                        (mype + 1) % npes);     \
        break;                                                  \
      case ATOMIC_FETCH:                                        \
        val = shmem_##TYPENAME##_atomic_fetch(&remote,          \
                                          (mype + 1) % npes);   \
        break;                                                  \
      case CTX_ATOMIC_FETCH:                                    \
        val = shmem_ctx_##TYPENAME##_atomic_fetch(              \
                SHMEM_CTX_DEFAULT, &remote, (mype + 1) % npes); \
        break;                                                  \
      SHMEMX_NBI_OPS_CASES(OP, TYPE, TYPENAME)                  \
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


int main(int argc, char* argv[]) {
  shmem_init();

  int rc = EXIT_SUCCESS;

#ifdef ENABLE_DEPRECATED_TESTS
  TEST_SHMEM_FETCH(FETCH, float, float);
  TEST_SHMEM_FETCH(FETCH, double, double);
  TEST_SHMEM_FETCH(FETCH, int, int);
  TEST_SHMEM_FETCH(FETCH, long, long);
  TEST_SHMEM_FETCH(FETCH, long long, longlong);
  TEST_SHMEM_FETCH(FETCH, unsigned int, uint);
  TEST_SHMEM_FETCH(FETCH, unsigned long, ulong);
  TEST_SHMEM_FETCH(FETCH, unsigned long long, ulonglong);
  TEST_SHMEM_FETCH(FETCH, int32_t, int32);
  TEST_SHMEM_FETCH(FETCH, int64_t, int64);
  TEST_SHMEM_FETCH(FETCH, uint32_t, uint32);
  TEST_SHMEM_FETCH(FETCH, uint64_t, uint64);
  TEST_SHMEM_FETCH(FETCH, size_t, size);
  TEST_SHMEM_FETCH(FETCH, ptrdiff_t, ptrdiff);
#endif /* ENABLE_DEPRECATED_TESTS */

  TEST_SHMEM_FETCH(ATOMIC_FETCH, float, float);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, double, double);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, int, int);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, long, long);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, long long, longlong);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, unsigned int, uint);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, unsigned long, ulong);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, unsigned long long, ulonglong);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, int32_t, int32);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, int64_t, int64);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, uint32_t, uint32);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, uint64_t, uint64);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, size_t, size);
  TEST_SHMEM_FETCH(ATOMIC_FETCH, ptrdiff_t, ptrdiff);

  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, float, float);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, double, double);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, int, int);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, long, long);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, long long, longlong);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, unsigned int, uint);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, unsigned long, ulong);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, unsigned long long, ulonglong);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, int32_t, int32);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, int64_t, int64);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, uint32_t, uint32);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, uint64_t, uint64);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, size_t, size);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH, ptrdiff_t, ptrdiff);


#ifdef ENABLE_SHMEMX_TESTS
  TEST_SHMEM_FETCH(ATOMIC_FETCH_NBI, float, float);
  TEST_SHMEM_FETCH(ATOMIC_FETCH_NBI, double, double);
  TEST_SHMEM_FETCH(ATOMIC_FETCH_NBI, int, int);
  TEST_SHMEM_FETCH(ATOMIC_FETCH_NBI, long, long);
  TEST_SHMEM_FETCH(ATOMIC_FETCH_NBI, long long, longlong);
  TEST_SHMEM_FETCH(ATOMIC_FETCH_NBI, unsigned int, uint);
  TEST_SHMEM_FETCH(ATOMIC_FETCH_NBI, unsigned long, ulong);
  TEST_SHMEM_FETCH(ATOMIC_FETCH_NBI, unsigned long long, ulonglong);
  TEST_SHMEM_FETCH(ATOMIC_FETCH_NBI, int32_t, int32);
  TEST_SHMEM_FETCH(ATOMIC_FETCH_NBI, int64_t, int64);
  TEST_SHMEM_FETCH(ATOMIC_FETCH_NBI, uint32_t, uint32);
  TEST_SHMEM_FETCH(ATOMIC_FETCH_NBI, uint64_t, uint64);
  TEST_SHMEM_FETCH(ATOMIC_FETCH_NBI, size_t, size);
  TEST_SHMEM_FETCH(ATOMIC_FETCH_NBI, ptrdiff_t, ptrdiff);

  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH_NBI, float, float);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH_NBI, double, double);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH_NBI, int, int);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH_NBI, long, long);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH_NBI, long long, longlong);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH_NBI, unsigned int, uint);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH_NBI, unsigned long, ulong);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH_NBI, unsigned long long, ulonglong);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH_NBI, int32_t, int32);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH_NBI, int64_t, int64);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH_NBI, uint32_t, uint32);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH_NBI, uint64_t, uint64);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH_NBI, size_t, size);
  TEST_SHMEM_FETCH(CTX_ATOMIC_FETCH_NBI, ptrdiff_t, ptrdiff);
#endif

  shmem_finalize();
  return rc;
}
