/*
 *  Copyright (c) 2021 Intel Corporation. All rights reserved.
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

/*
 * Validate shmem_put_signal operation through blocking and non-blocking
 * APIs. 
*/

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <shmem.h>

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L

enum op { PUT_SIGNAL = 0, PUT_SIGNAL_NBI };

#define TEST_SHMEM_PUT_SIGNAL(OP, USE_CTX, SIGNAL_OP, TYPE)                                 \
  do {                                                                                      \
    TYPE *remote = (TYPE *) shmem_malloc(10 * sizeof(TYPE));                                 \
    const int mype = shmem_my_pe();                                                         \
    const int npes = shmem_n_pes();                                                         \
    TYPE local[10];                                                                         \
    static uint64_t sig_addr = 0;                                                           \
    for (int i = 0; i < 10; i++)                                                            \
      local[i] = (TYPE) i;                                                                  \
    int atomic_op = (SIGNAL_OP == 0) ? SHMEM_SIGNAL_SET : SHMEM_SIGNAL_ADD;                 \
    shmem_barrier_all();                                                                    \
    switch (OP) {                                                                           \
      case PUT_SIGNAL:                                                                      \
        if (USE_CTX) {                                                                      \
          if (mype == 0) {                                                                  \
            shmem_put_signal(SHMEM_CTX_DEFAULT, remote, local, 10, &sig_addr, (uint64_t) 1, \
                             atomic_op, (mype + 1) % npes);                                 \
            shmem_wait_until(&sig_addr, SHMEM_CMP_EQ, 1);                                   \
          } else {                                                                          \
            shmem_wait_until(&sig_addr, SHMEM_CMP_EQ, 1);                                   \
            shmem_put_signal(SHMEM_CTX_DEFAULT, remote, remote, 10, &sig_addr, (uint64_t) 1,\
                             atomic_op, (mype + 1) % npes);                                 \
          }                                                                                 \
        }                                                                                   \
        else {                                                                              \
          if (mype == 0) {                                                                  \
            shmem_put_signal(remote, local, 10, &sig_addr, (uint64_t) 1, atomic_op,         \
                             (mype + 1) % npes);                                            \
            shmem_wait_until(&sig_addr, SHMEM_CMP_EQ, 1);                                   \
          } else {                                                                          \
            shmem_wait_until(&sig_addr, SHMEM_CMP_EQ, 1);                                   \
            shmem_put_signal(remote, remote, 10, &sig_addr, (uint64_t) 1, atomic_op,        \
                             (mype + 1) % npes);                                            \
          }                                                                                 \
        }                                                                                   \
        break;                                                                              \
      case PUT_SIGNAL_NBI:                                                                  \
        if (USE_CTX) {                                                                      \
          if (mype == 0) {                                                                  \
            for (int i = 0; i < npes; i++) {                                                \
              shmem_put_signal_nbi(SHMEM_CTX_DEFAULT, remote, local, 10, &sig_addr,         \
                                   (uint64_t) 1, atomic_op, i);                             \
            }                                                                               \
            shmem_quiet();                                                                  \
          }                                                                                 \
          shmem_wait_until(&sig_addr, SHMEM_CMP_EQ, 1);                                     \
        } else {                                                                            \
          if (mype == 0) {                                                                  \
            for (int i = 0; i < npes; i++) {                                                \
              shmem_put_signal_nbi(remote, local, 10, &sig_addr, (uint64_t) 1,              \
                                   atomic_op, i);                                           \
            }                                                                               \
            shmem_quiet();                                                                  \
          }                                                                                 \
          shmem_wait_until(&sig_addr, SHMEM_CMP_EQ, 1);                                     \
        }                                                                                   \
        break;                                                                              \
      default:                                                                              \
        printf("Invalid operation (%d)\n", OP);                                             \
        shmem_global_exit(1);                                                               \
    }                                                                                       \
    sig_addr = 0;                                                                           \
    shmem_barrier_all();                                                                    \
    for (int i = 0; i < 10; i++)                                                            \
      if (remote[i] != (TYPE)(i)) {                                                         \
        printf("PE %i received incorrect value with "                                       \
               "TEST_SHMEM_PUT_SIGNAL(%s, %d, %s)\n", mype,                                 \
               #OP, (int)(USE_CTX), #TYPE);                                                 \
        rc = EXIT_FAILURE;                                                                  \
      }                                                                                     \
    shmem_free(remote);                                                                     \
  } while (0)

#else
#define TEST_SHMEM_PUT_SIGNAL(OP, USE_CTX, SIGNAL_OP, TYPE)

#endif

int main(int argc, char* argv[]) {
  shmem_init();

  int rc = EXIT_SUCCESS;

  /* Loop over on context usage */
  for (int i = 0; i < 2; i++) {
    /* Loop over on atomic ops */
    for (int j = 0; j < 2; j++) {
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, float);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, double);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, long double);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, char);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, signed char);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, short);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, int);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, long);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, long long);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, unsigned char);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, unsigned short);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, unsigned int);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, unsigned long);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, unsigned long long);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, int8_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, int16_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, int32_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, int64_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, uint8_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, uint16_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, uint32_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, uint64_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, size_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL, i, j, ptrdiff_t);
    }
  }

  /* Loop over on context usage */
  for (int i = 0; i < 2; i++) {
    /* Loop over on atomic ops */
    for (int j = 0; j < 2; j++) {
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, float);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, double);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, long double);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, char);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, signed char);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, short);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, int);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, long);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, long long);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, unsigned char);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, unsigned short);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, unsigned int);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, unsigned long);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, unsigned long long);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, int8_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, int16_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, int32_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, int64_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, uint8_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, uint16_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, uint32_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, uint64_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, size_t);
      TEST_SHMEM_PUT_SIGNAL(PUT_SIGNAL_NBI, i, j, ptrdiff_t);
    }
  }

  shmem_finalize();
  return rc;
}
