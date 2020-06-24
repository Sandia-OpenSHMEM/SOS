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

enum op { SWAP = 0, ATOMIC_SWAP, CTX_ATOMIC_SWAP, ATOMIC_SWAP_NBI,
          CTX_ATOMIC_SWAP_NBI };

#ifdef ENABLE_DEPRECATED_TESTS
#define DEPRECATED_SWAP(TYPENAME, ...) shmem_##TYPENAME##_swap(__VA_ARGS__)
#else
#define DEPRECATED_SWAP(TYPENAME, ...) shmem_##TYPENAME##_atomic_swap(__VA_ARGS__)
#endif

#ifdef ENABLE_SHMEMX_TESTS
#define SHMEM_NBI_OPS_CASES(OP, TYPE, TYPENAME)                         \
        case ATOMIC_SWAP_NBI:                                           \
            shmem_##TYPENAME##_atomic_swap_nbi(&old, &remote,           \
                                   (TYPE)mype, (mype + 1) % npes);      \
            break;                                                      \
        case CTX_ATOMIC_SWAP_NBI:                                       \
            shmem_ctx_##TYPENAME##_atomic_swap_nbi(SHMEM_CTX_DEFAULT,   \
                         &old, &remote, (TYPE)mype, (mype + 1) % npes); \
            break;
#else
#define SHMEM_NBI_OPS_CASES(OP, TYPE, TYPENAME)
#endif

#define TEST_SHMEM_SWAP(OP, TYPE, TYPENAME)                             \
  do {                                                                  \
    static TYPE remote;                                                 \
    TYPE old;                                                           \
    const int mype = shmem_my_pe();                                     \
    const int npes = shmem_n_pes();                                     \
    remote = npes;                                                      \
    shmem_barrier_all();                                                \
    switch (OP) {                                                       \
        case SWAP:                                                      \
            old = DEPRECATED_SWAP(TYPENAME, &remote, (TYPE)mype,        \
                                                    (mype + 1) % npes); \
            break;                                                      \
        case ATOMIC_SWAP:                                               \
            old = shmem_##TYPENAME##_atomic_swap(&remote, (TYPE)mype,   \
                                                    (mype + 1) % npes); \
            break;                                                      \
        case CTX_ATOMIC_SWAP:                                           \
            old = shmem_ctx_##TYPENAME##_atomic_swap(SHMEM_CTX_DEFAULT, \
                           &remote, (TYPE)mype, (mype + 1) % npes);     \
            break;                                                      \
        SHMEM_NBI_OPS_CASES(OP, TYPE, TYPENAME)                         \
        default:                                                        \
          printf("invalid operation (%d)\n", OP);                       \
          shmem_global_exit(1);                                         \
    }                                                                   \
    shmem_barrier_all();                                                \
    if (remote != (TYPE)((mype + npes - 1) % npes)) {                   \
      printf("PE %i received incorrect value with "                     \
             "TEST_SHMEM_SWAP(%s, %s)\n", mype, #OP, #TYPE);            \
      rc = EXIT_FAILURE;                                                \
    }                                                                   \
    if (old != (TYPE) npes) {                                           \
      printf("PE %i error inconsistent value of old (%s, %s)\n",        \
             mype, #OP, #TYPE);                                         \
      rc = EXIT_FAILURE;                                                \
    }                                                                   \
  } while (false)


int main(int argc, char* argv[]) {
  shmem_init();

  int rc = EXIT_SUCCESS;

#ifdef ENABLE_DEPRECATED_TESTS
  TEST_SHMEM_SWAP(SWAP, float, float);
  TEST_SHMEM_SWAP(SWAP, double, double);
  TEST_SHMEM_SWAP(SWAP, int, int);
  TEST_SHMEM_SWAP(SWAP, long, long);
  TEST_SHMEM_SWAP(SWAP, long long, longlong);
  TEST_SHMEM_SWAP(SWAP, unsigned int, uint);
  TEST_SHMEM_SWAP(SWAP, unsigned long, ulong);
  TEST_SHMEM_SWAP(SWAP, unsigned long long, ulonglong);
  TEST_SHMEM_SWAP(SWAP, int32_t, int32);
  TEST_SHMEM_SWAP(SWAP, int64_t, int64);
  TEST_SHMEM_SWAP(SWAP, uint32_t, uint32);
  TEST_SHMEM_SWAP(SWAP, uint64_t, uint64);
  TEST_SHMEM_SWAP(SWAP, size_t, size);
  TEST_SHMEM_SWAP(SWAP, ptrdiff_t, ptrdiff);
#endif /* ENABLE_DEPRECATED_TESTS */

  TEST_SHMEM_SWAP(ATOMIC_SWAP, float, float);
  TEST_SHMEM_SWAP(ATOMIC_SWAP, double, double);
  TEST_SHMEM_SWAP(ATOMIC_SWAP, int, int);
  TEST_SHMEM_SWAP(ATOMIC_SWAP, long, long);
  TEST_SHMEM_SWAP(ATOMIC_SWAP, long long, longlong);
  TEST_SHMEM_SWAP(ATOMIC_SWAP, unsigned int, uint);
  TEST_SHMEM_SWAP(ATOMIC_SWAP, unsigned long, ulong);
  TEST_SHMEM_SWAP(ATOMIC_SWAP, unsigned long long, ulonglong);
  TEST_SHMEM_SWAP(ATOMIC_SWAP, int32_t, int32);
  TEST_SHMEM_SWAP(ATOMIC_SWAP, int64_t, int64);
  TEST_SHMEM_SWAP(ATOMIC_SWAP, uint32_t, uint32);
  TEST_SHMEM_SWAP(ATOMIC_SWAP, uint64_t, uint64);
  TEST_SHMEM_SWAP(ATOMIC_SWAP, size_t, size);
  TEST_SHMEM_SWAP(ATOMIC_SWAP, ptrdiff_t, ptrdiff);

  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP, float, float);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP, double, double);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP, int, int);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP, long, long);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP, long long, longlong);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP, unsigned int, uint);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP, unsigned long, ulong);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP, unsigned long long, ulonglong);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP, int32_t, int32);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP, int64_t, int64);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP, uint32_t, uint32);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP, uint64_t, uint64);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP, size_t, size);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP, ptrdiff_t, ptrdiff);

#ifdef ENABLE_SHMEMX_TESTS
  TEST_SHMEM_SWAP(ATOMIC_SWAP_NBI, float, float);
  TEST_SHMEM_SWAP(ATOMIC_SWAP_NBI, double, double);
  TEST_SHMEM_SWAP(ATOMIC_SWAP_NBI, int, int);
  TEST_SHMEM_SWAP(ATOMIC_SWAP_NBI, long, long);
  TEST_SHMEM_SWAP(ATOMIC_SWAP_NBI, long long, longlong);
  TEST_SHMEM_SWAP(ATOMIC_SWAP_NBI, unsigned int, uint);
  TEST_SHMEM_SWAP(ATOMIC_SWAP_NBI, unsigned long, ulong);
  TEST_SHMEM_SWAP(ATOMIC_SWAP_NBI, unsigned long long, ulonglong);
  TEST_SHMEM_SWAP(ATOMIC_SWAP_NBI, int32_t, int32);
  TEST_SHMEM_SWAP(ATOMIC_SWAP_NBI, int64_t, int64);
  TEST_SHMEM_SWAP(ATOMIC_SWAP_NBI, uint32_t, uint32);
  TEST_SHMEM_SWAP(ATOMIC_SWAP_NBI, uint64_t, uint64);
  TEST_SHMEM_SWAP(ATOMIC_SWAP_NBI, size_t, size);
  TEST_SHMEM_SWAP(ATOMIC_SWAP_NBI, ptrdiff_t, ptrdiff);

  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP_NBI, float, float);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP_NBI, double, double);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP_NBI, int, int);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP_NBI, long, long);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP_NBI, long long, longlong);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP_NBI, unsigned int, uint);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP_NBI, unsigned long, ulong);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP_NBI, unsigned long long, ulonglong);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP_NBI, int32_t, int32);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP_NBI, int64_t, int64);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP_NBI, uint32_t, uint32);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP_NBI, uint64_t, uint64);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP_NBI, size_t, size);
  TEST_SHMEM_SWAP(CTX_ATOMIC_SWAP_NBI, ptrdiff_t, ptrdiff);
#endif

  shmem_finalize();
  return rc;
}
