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

enum op { OR = 0, CTX_OR, FETCH_OR, CTX_FETCH_OR };

/* Initially, remote = 000...b.  Each PE performs an atomic OR where the
 * PEth bit of the input value is set to 1 and all other bits are set to 0.
 * The result has the NPES least significant bits set, 000...111...b.
 */

#define TEST_SHMEM_OR(OP, TYPE)                                         \
  do {                                                                  \
    static TYPE remote = (TYPE)0;                                       \
    const int mype = shmem_my_pe();                                     \
    const int npes = shmem_n_pes();                                     \
    TYPE old = (TYPE)0;                                                 \
    if (npes-1 > sizeof(TYPE)) break; /* Avoid overflow */              \
    for (int i = 0; i < npes; i++)                                      \
      switch (OP) {                                                     \
        case OR:                                                        \
          shmem_atomic_or(&remote, (TYPE)(1LLU << mype), i);            \
          break;                                                        \
        case CTX_OR:                                                    \
          shmem_atomic_or(SHMEM_CTX_DEFAULT, &remote, (TYPE)(1LLU << mype), i); \
          break;                                                        \
        case FETCH_OR:                                                  \
          old = shmem_atomic_fetch_or(&remote, (TYPE)(1LLU << mype), i);\
          if ((old & (TYPE)(1LLU << mype)) != 0) {                      \
            printf("PE %i error inconsistent value of old (%s, %s)\n",  \
                   mype, #OP, #TYPE);                                   \
            rc = EXIT_FAILURE;                                          \
          }                                                             \
          break;                                                        \
        case CTX_FETCH_OR:                                              \
          old = shmem_atomic_fetch_or(SHMEM_CTX_DEFAULT, &remote, (TYPE)(1LLU << mype), i); \
          if ((old & (TYPE)(1LLU << mype)) != 0) {                      \
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
    if (remote != (TYPE)((1LLU << npes) - 1LLU)) {                      \
      printf("PE %i observed error with TEST_SHMEM_OR(%s, %s)\n",       \
              mype, #OP, #TYPE);                                        \
      rc = EXIT_FAILURE;                                                \
    }                                                                   \
  } while (false)

#else
#define TEST_SHMEM_OR(OP, TYPE)

#endif

int main(int argc, char* argv[]) {
  shmem_init();

  int rc = EXIT_SUCCESS;
  TEST_SHMEM_OR(OR, unsigned int);
  TEST_SHMEM_OR(OR, unsigned long);
  TEST_SHMEM_OR(OR, unsigned long long);
  TEST_SHMEM_OR(OR, int32_t);
  TEST_SHMEM_OR(OR, int64_t);
  TEST_SHMEM_OR(OR, uint32_t);
  TEST_SHMEM_OR(OR, uint64_t);

  TEST_SHMEM_OR(CTX_OR, unsigned int);
  TEST_SHMEM_OR(CTX_OR, unsigned long);
  TEST_SHMEM_OR(CTX_OR, unsigned long long);
  TEST_SHMEM_OR(CTX_OR, int32_t);
  TEST_SHMEM_OR(CTX_OR, int64_t);
  TEST_SHMEM_OR(CTX_OR, uint32_t);
  TEST_SHMEM_OR(CTX_OR, uint64_t);

  TEST_SHMEM_OR(FETCH_OR, unsigned int);
  TEST_SHMEM_OR(FETCH_OR, unsigned long);
  TEST_SHMEM_OR(FETCH_OR, unsigned long long);
  TEST_SHMEM_OR(FETCH_OR, int32_t);
  TEST_SHMEM_OR(FETCH_OR, int64_t);
  TEST_SHMEM_OR(FETCH_OR, uint32_t);
  TEST_SHMEM_OR(FETCH_OR, uint64_t);

  TEST_SHMEM_OR(CTX_FETCH_OR, unsigned int);
  TEST_SHMEM_OR(CTX_FETCH_OR, unsigned long);
  TEST_SHMEM_OR(CTX_FETCH_OR, unsigned long long);
  TEST_SHMEM_OR(CTX_FETCH_OR, int32_t);
  TEST_SHMEM_OR(CTX_FETCH_OR, int64_t);
  TEST_SHMEM_OR(CTX_FETCH_OR, uint32_t);
  TEST_SHMEM_OR(CTX_FETCH_OR, uint64_t);

  shmem_finalize();
  return rc;
}
