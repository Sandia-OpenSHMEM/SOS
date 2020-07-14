/*
 *  This test program is derived from a unit test created by Nick Park.
 *  The original unit test is a work of the U.S. Government and is not subject
 *  to copyright protection in the United States.  Foreign copyrights may
 *  apply.
 *
 *  Copyright (c) 2019 Intel Corporation. All rights reserved.
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
#include <stdio.h>
#include <shmem.h>
#include <shmemx.h>

#define MAX_NPES 32

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L

enum op { BCAST = 0, COLLECT, FCOLLECT, ALLTOALL, ALLTOALLS };

#define TEST_SHMEM_COLLECTIVE(OP, TYPE)                                   \
  do {                                                                    \
    static TYPE src[MAX_NPES];                                            \
    static TYPE dest[MAX_NPES*MAX_NPES];                                  \
                                                                          \
    for (int i = 0; i < MAX_NPES; i++) {                                  \
      src[i] = (TYPE)mype;                                                \
    }                                                                     \
                                                                          \
    switch (OP) {                                                         \
      case BCAST:                                                         \
        shmem_broadcast(SHMEM_TEAM_WORLD, dest, src, MAX_NPES, npes-1);   \
        break;                                                            \
      case COLLECT:                                                       \
        shmem_collect(SHMEM_TEAM_WORLD, dest, src, MAX_NPES);             \
        break;                                                            \
      case FCOLLECT:                                                      \
        shmem_fcollect(SHMEM_TEAM_WORLD, dest, src, MAX_NPES);            \
        break;                                                            \
      case ALLTOALL:                                                      \
        shmem_alltoall(SHMEM_TEAM_WORLD, dest, src, 1);                   \
        break;                                                            \
      case ALLTOALLS:                                                     \
        shmem_alltoalls(SHMEM_TEAM_WORLD, dest, src, 1, 1, 1);            \
        break;                                                            \
      default:                                                            \
        printf("Invalid operation (%d)\n", OP);                           \
        shmem_global_exit(1);                                             \
    }                                                                     \
    shmem_barrier_all();                                                  \
                                                                          \
    switch (OP) {                                                         \
      case BCAST:                                                         \
        if (mype != npes-1) {                                             \
          for (int i = 0; i < MAX_NPES; i++) {                            \
            if (dest[i] != (TYPE)npes-1) {                                \
              printf("PE %i received incorrect value with "               \
                     "TEST_SHMEM_COLLECTIVE(%s, %s)\n", mype,             \
                     #OP, #TYPE);                                         \
              rc = EXIT_FAILURE;                                          \
            }                                                             \
          }                                                               \
        break;                                                            \
      case COLLECT:                                                       \
        for (int i = 0; i < MAX_NPES*npes; i++) {                         \
          if (dest[i] != (TYPE)(i / MAX_NPES)) {                          \
            printf("PE %i received incorrect value with "                 \
                   "TEST_SHMEM_COLLECTIVE(%s, %s)\n", mype,               \
                   #OP, #TYPE);                                           \
            rc = EXIT_FAILURE;                                            \
          }                                                               \
        }                                                                 \
        break;                                                            \
      case FCOLLECT:                                                      \
        for (int i = 0; i < MAX_NPES*npes; i++) {                         \
          if (dest[i] != (TYPE)(i / MAX_NPES)) {                          \
            printf("PE %i received incorrect value with "                 \
                   "TEST_SHMEM_COLLECTIVE(%s, %s)\n", mype,               \
                   #OP, #TYPE);                                           \
            rc = EXIT_FAILURE;                                            \
          }                                                               \
        }                                                                 \
        break;                                                            \
      case ALLTOALL:                                                      \
        for (int i = 0; i < npes; i++) {                                  \
          if (dest[i] != (TYPE)i) {                                       \
            printf("PE %i received incorrect value with "                 \
                   "TEST_SHMEM_COLLECTIVE(%s, %s)\n", mype,               \
                   #OP, #TYPE);                                           \
            rc = EXIT_FAILURE;                                            \
          }                                                               \
        }                                                                 \
        break;                                                            \
      case ALLTOALLS:                                                     \
        for (int i = 0; i < npes; i++) {                                  \
          if (dest[i] != (TYPE)i) {                                       \
            printf("PE %i received incorrect value with "                 \
                   "TEST_SHMEM_COLLECTIVE(%s, %s)\n", mype,               \
                   #OP, #TYPE);                                           \
            rc = EXIT_FAILURE;                                            \
          }                                                               \
        }                                                                 \
        break;                                                            \
      default:                                                            \
        printf("Invalid operation (%d)\n", OP);                           \
        shmem_global_exit(1);                                             \
    }                                                                     \
    }                                                                     \
  } while (0)

#else
#define TEST_SHMEM_COLLECTIVE(OP, TYPE)

#endif

int main(void) {
  shmem_init();

  int rc = EXIT_SUCCESS;

  const int mype = shmem_my_pe();
  const int npes = shmem_n_pes();

  if (npes > MAX_NPES) {
      if (mype == 0)
          fprintf(stderr, "ERR - Requires at least %d PEs\n", MAX_NPES);
      shmem_finalize();
      return 0;
  }

  TEST_SHMEM_COLLECTIVE(BCAST, float);
  TEST_SHMEM_COLLECTIVE(BCAST, double);
  TEST_SHMEM_COLLECTIVE(BCAST, long double);
  TEST_SHMEM_COLLECTIVE(BCAST, char);
  TEST_SHMEM_COLLECTIVE(BCAST, signed char);
  TEST_SHMEM_COLLECTIVE(BCAST, short);
  TEST_SHMEM_COLLECTIVE(BCAST, int);
  TEST_SHMEM_COLLECTIVE(BCAST, long);
  TEST_SHMEM_COLLECTIVE(BCAST, long long);
  TEST_SHMEM_COLLECTIVE(BCAST, unsigned char);
  TEST_SHMEM_COLLECTIVE(BCAST, unsigned short);
  TEST_SHMEM_COLLECTIVE(BCAST, unsigned int);
  TEST_SHMEM_COLLECTIVE(BCAST, unsigned long);
  TEST_SHMEM_COLLECTIVE(BCAST, unsigned long long);
  TEST_SHMEM_COLLECTIVE(BCAST, int8_t);
  TEST_SHMEM_COLLECTIVE(BCAST, int16_t);
  TEST_SHMEM_COLLECTIVE(BCAST, int32_t);
  TEST_SHMEM_COLLECTIVE(BCAST, int64_t);
  TEST_SHMEM_COLLECTIVE(BCAST, uint8_t);
  TEST_SHMEM_COLLECTIVE(BCAST, uint16_t);
  TEST_SHMEM_COLLECTIVE(BCAST, uint32_t);
  TEST_SHMEM_COLLECTIVE(BCAST, uint64_t);
  TEST_SHMEM_COLLECTIVE(BCAST, size_t);
  TEST_SHMEM_COLLECTIVE(BCAST, ptrdiff_t);

  TEST_SHMEM_COLLECTIVE(COLLECT, float);
  TEST_SHMEM_COLLECTIVE(COLLECT, double);
  TEST_SHMEM_COLLECTIVE(COLLECT, long double);
  TEST_SHMEM_COLLECTIVE(COLLECT, char);
  TEST_SHMEM_COLLECTIVE(COLLECT, signed char);
  TEST_SHMEM_COLLECTIVE(COLLECT, short);
  TEST_SHMEM_COLLECTIVE(COLLECT, int);
  TEST_SHMEM_COLLECTIVE(COLLECT, long);
  TEST_SHMEM_COLLECTIVE(COLLECT, long long);
  TEST_SHMEM_COLLECTIVE(COLLECT, unsigned char);
  TEST_SHMEM_COLLECTIVE(COLLECT, unsigned short);
  TEST_SHMEM_COLLECTIVE(COLLECT, unsigned int);
  TEST_SHMEM_COLLECTIVE(COLLECT, unsigned long);
  TEST_SHMEM_COLLECTIVE(COLLECT, unsigned long long);
  TEST_SHMEM_COLLECTIVE(COLLECT, int8_t);
  TEST_SHMEM_COLLECTIVE(COLLECT, int16_t);
  TEST_SHMEM_COLLECTIVE(COLLECT, int32_t);
  TEST_SHMEM_COLLECTIVE(COLLECT, int64_t);
  TEST_SHMEM_COLLECTIVE(COLLECT, uint8_t);
  TEST_SHMEM_COLLECTIVE(COLLECT, uint16_t);
  TEST_SHMEM_COLLECTIVE(COLLECT, uint32_t);
  TEST_SHMEM_COLLECTIVE(COLLECT, uint64_t);
  TEST_SHMEM_COLLECTIVE(COLLECT, size_t);
  TEST_SHMEM_COLLECTIVE(COLLECT, ptrdiff_t);

  TEST_SHMEM_COLLECTIVE(FCOLLECT, float);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, double);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, long double);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, char);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, signed char);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, short);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, int);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, long);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, long long);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, unsigned char);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, unsigned short);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, unsigned int);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, unsigned long);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, unsigned long long);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, int8_t);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, int16_t);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, int32_t);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, int64_t);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, uint8_t);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, uint16_t);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, uint32_t);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, uint64_t);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, size_t);
  TEST_SHMEM_COLLECTIVE(FCOLLECT, ptrdiff_t);

  TEST_SHMEM_COLLECTIVE(ALLTOALL, float);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, double);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, long double);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, char);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, signed char);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, short);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, int);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, long);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, long long);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, unsigned char);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, unsigned short);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, unsigned int);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, unsigned long);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, unsigned long long);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, int8_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, int16_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, int32_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, int64_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, uint8_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, uint16_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, uint32_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, uint64_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, size_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALL, ptrdiff_t);

  TEST_SHMEM_COLLECTIVE(ALLTOALLS, float);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, double);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, long double);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, char);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, signed char);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, short);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, int);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, long);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, long long);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, unsigned char);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, unsigned short);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, unsigned int);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, unsigned long);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, unsigned long long);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, int8_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, int16_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, int32_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, int64_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, uint8_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, uint16_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, uint32_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, uint64_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, size_t);
  TEST_SHMEM_COLLECTIVE(ALLTOALLS, ptrdiff_t);

  shmem_finalize();
  return rc;
}
