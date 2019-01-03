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

#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <shmem.h>

#ifdef ENABLE_SHMEMX_TESTS
#include <shmemx.h>
#endif

enum op { AND = 0, CTX_AND, FETCH_AND, CTX_FETCH_AND, FETCH_AND_NBI,
          CTX_FETCH_AND_NBI };

/* Initially, remote = 111...b.  Each PE performs an atomic AND where the
 * PEth bit of the input value is set to 0 and all other bits are set to 1.
 * The result has the NPES least significant bits cleared, 111...000...b.
 */

#ifdef ENABLE_SHMEMX_TESTS
#define SHMEMX_NBI_OPS_CASES(OP, TYPE, TYPENAME)                        \
        case FETCH_AND_NBI:                                             \
          shmemx_##TYPENAME##_atomic_fetch_and_nbi(&old, &remote,       \
                                      ~(TYPE)(1LLU << mype), i);        \
          shmem_quiet();                                                \
          if ((old & (TYPE)(1LLU << mype)) == 0) {                      \
            printf("PE %i error inconsistent value of old (%s, %s)\n",  \
                   mype, #OP, #TYPE);                                   \
            rc = EXIT_FAILURE;                                          \
          }                                                             \
          break;                                                        \
        case CTX_FETCH_AND_NBI:                                         \
          shmemx_ctx_##TYPENAME##_atomic_fetch_and_nbi(SHMEM_CTX_DEFAULT,\
                  &old, &remote, ~(TYPE)(1LLU << mype), i);             \
          shmem_quiet();                                                \
          if ((old & (TYPE)(1LLU << mype)) == 0) {                      \
            printf("PE %i error inconsistent value of old (%s, %s)\n",  \
                   mype, #OP, #TYPE);                                   \
            rc = EXIT_FAILURE;                                          \
          }                                                             \
          break;
#else
#define SHMEMX_NBI_OPS_CASES(OP, TYPE, TYPENAME)
#endif

#define TEST_SHMEM_AND(OP, TYPE, TYPENAME)                              \
  do {                                                                  \
    static TYPE remote = ~(TYPE)0;                                      \
    TYPE old = (TYPE)0;                                                 \
    if ((size_t) npes-1 > sizeof(TYPE)) break; /* Avoid overflow */     \
    for (int i = 0; i < npes; i++)                                      \
      switch (OP) {                                                     \
        case AND:                                                       \
          shmem_##TYPENAME##_atomic_and(&remote,                        \
                                    ~(TYPE)(1LLU << mype), i);          \
          break;                                                        \
        case CTX_AND:                                                   \
          shmem_ctx_##TYPENAME##_atomic_and(SHMEM_CTX_DEFAULT, &remote, \
                                             ~(TYPE)(1LLU << mype), i); \
          break;                                                        \
        case FETCH_AND:                                                 \
          old = shmem_##TYPENAME##_atomic_fetch_and(&remote,            \
                                             ~(TYPE)(1LLU << mype), i); \
          if ((old & (TYPE)(1LLU << mype)) == 0) {                      \
            printf("PE %i error inconsistent value of old (%s, %s)\n",  \
                   mype, #OP, #TYPE);                                   \
            rc = EXIT_FAILURE;                                          \
          }                                                             \
          break;                                                        \
        case CTX_FETCH_AND:                                             \
          old = shmem_ctx_##TYPENAME##_atomic_fetch_and(                \
                 SHMEM_CTX_DEFAULT, &remote, ~(TYPE)(1LLU << mype), i); \
          if ((old & (TYPE)(1LLU << mype)) == 0) {                      \
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
    if (remote != ~(TYPE)((1LLU << npes) - 1LLU)) {                     \
      printf("PE %i observed error with TEST_SHMEM_AND(%s, %s)\n",      \
              mype, #OP, #TYPE);                                        \
      rc = EXIT_FAILURE;                                                \
    }                                                                   \
  } while (false)


int main(int argc, char* argv[]) {
  shmem_init();

  const int mype = shmem_my_pe();
  const int npes = shmem_n_pes();

  int rc = EXIT_SUCCESS;
  TEST_SHMEM_AND(AND, unsigned int, uint);
  TEST_SHMEM_AND(AND, unsigned long, ulong);
  TEST_SHMEM_AND(AND, unsigned long long, ulonglong);
  TEST_SHMEM_AND(AND, int32_t, int32);
  TEST_SHMEM_AND(AND, int64_t, int64);
  TEST_SHMEM_AND(AND, uint32_t, uint32);
  TEST_SHMEM_AND(AND, uint64_t, uint64);

  TEST_SHMEM_AND(CTX_AND, unsigned int, uint);
  TEST_SHMEM_AND(CTX_AND, unsigned long, ulong);
  TEST_SHMEM_AND(CTX_AND, unsigned long long, ulonglong);
  TEST_SHMEM_AND(CTX_AND, int32_t, int32);
  TEST_SHMEM_AND(CTX_AND, int64_t, int64);
  TEST_SHMEM_AND(CTX_AND, uint32_t, uint32);
  TEST_SHMEM_AND(CTX_AND, uint64_t, uint64);

  TEST_SHMEM_AND(FETCH_AND, unsigned int, uint);
  TEST_SHMEM_AND(FETCH_AND, unsigned long, ulong);
  TEST_SHMEM_AND(FETCH_AND, unsigned long long, ulonglong);
  TEST_SHMEM_AND(FETCH_AND, int32_t, int32);
  TEST_SHMEM_AND(FETCH_AND, int64_t, int64);
  TEST_SHMEM_AND(FETCH_AND, uint32_t, uint32);
  TEST_SHMEM_AND(FETCH_AND, uint64_t, uint64);

  TEST_SHMEM_AND(CTX_FETCH_AND, unsigned int, uint);
  TEST_SHMEM_AND(CTX_FETCH_AND, unsigned long, ulong);
  TEST_SHMEM_AND(CTX_FETCH_AND, unsigned long long, ulonglong);
  TEST_SHMEM_AND(CTX_FETCH_AND, int32_t, int32);
  TEST_SHMEM_AND(CTX_FETCH_AND, int64_t, int64);
  TEST_SHMEM_AND(CTX_FETCH_AND, uint32_t, uint32);
  TEST_SHMEM_AND(CTX_FETCH_AND, uint64_t, uint64);

#ifdef ENABLE_SHMEMX_TESTS
  TEST_SHMEM_AND(FETCH_AND_NBI, unsigned int, uint);
  TEST_SHMEM_AND(FETCH_AND_NBI, unsigned long, ulong);
  TEST_SHMEM_AND(FETCH_AND_NBI, unsigned long long, ulonglong);
  TEST_SHMEM_AND(FETCH_AND_NBI, int32_t, int32);
  TEST_SHMEM_AND(FETCH_AND_NBI, int64_t, int64);
  TEST_SHMEM_AND(FETCH_AND_NBI, uint32_t, uint32);
  TEST_SHMEM_AND(FETCH_AND_NBI, uint64_t, uint64);

  TEST_SHMEM_AND(CTX_FETCH_AND_NBI, unsigned int, uint);
  TEST_SHMEM_AND(CTX_FETCH_AND_NBI, unsigned long, ulong);
  TEST_SHMEM_AND(CTX_FETCH_AND_NBI, unsigned long long, ulonglong);
  TEST_SHMEM_AND(CTX_FETCH_AND_NBI, int32_t, int32);
  TEST_SHMEM_AND(CTX_FETCH_AND_NBI, int64_t, int64);
  TEST_SHMEM_AND(CTX_FETCH_AND_NBI, uint32_t, uint32);
  TEST_SHMEM_AND(CTX_FETCH_AND_NBI, uint64_t, uint64);
#endif
  shmem_finalize();
  return rc;
}
