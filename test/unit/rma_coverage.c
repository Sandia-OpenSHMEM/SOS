/*
 *  Copyright (c) 2015 Intel Corporation. All rights reserved.
 *  This software is available to you under the BSD license below:
 *
 * *	Redistribution and use in source and binary forms, with or
 *	without modification, are permitted provided that the following
 *	conditions are met:
 *
 *	- Redistributions of source code must retain the above
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

#define SHMEM_INTERNAL_INCLUDE
#include <shmem.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

#define CHUNK_SIZE 10
#define ARR_SIZE (4*(CHUNK_SIZE))

#define DECLARE_TEST(TYPENAME,TYPE) \
  int TYPENAME##_rmaTest(int target_pe, int verbose);
SHMEM_DECLARE_FOR_RMA(DECLARE_TEST);

int main(int argc, char* argv[]) {
    int verbose = 0;
    if(argc > 1) {
        verbose = !strcmp("-v",argv[1]);
    }

    int errors = 0;

    int me, myshmem_n_pes;
    shmem_init();
    myshmem_n_pes = shmem_n_pes();
    me = shmem_my_pe();

    srand(1+me);

    int nextpe = (me+1)%myshmem_n_pes;

#define RUN_TEST(TYPENAME,TYPE) do { \
        errors += (TYPENAME##_rmaTest(nextpe,verbose)); \
    } while(0)

    SHMEM_DECLARE_FOR_RMA(RUN_TEST);

    shmem_finalize();

    return errors;
}

#define DEFINE_TEST(TYPENAME,TYPE) \
  TYPE TYPENAME##_shared[ARR_SIZE];                                    \
                                                                       \
  int TYPENAME##_rmaTest(int target_pe, int verbose) {                 \
      TYPE* shared = TYPENAME##_shared;                                \
      TYPE myvals[ARR_SIZE];                                           \
      TYPE result[ARR_SIZE];                                           \
      size_t i;                                                        \
                                                                       \
      for(i = 0; i < ARR_SIZE; ++i) {                                  \
          myvals[i] = (TYPE)rand();                                    \
      }                                                                \
                                                                       \
      shmem_##TYPENAME##_put(shared, myvals, CHUNK_SIZE, target_pe);   \
      for(i = 0; i < CHUNK_SIZE; ++i) {                                \
          shmem_##TYPENAME##_p(&shared[CHUNK_SIZE+i],                  \
              myvals[CHUNK_SIZE+i], target_pe);                        \
      }                                                                \
      shmem_##TYPENAME##_iput(shared+2*CHUNK_SIZE,myvals+2*CHUNK_SIZE, \
          1, 2, CHUNK_SIZE/2, target_pe);                              \
      shmem_##TYPENAME##_iput(shared+2*CHUNK_SIZE+CHUNK_SIZE/2,        \
          myvals+2*CHUNK_SIZE+1, 1, 2, CHUNK_SIZE/2, target_pe);       \
      shmem_##TYPENAME##_put_nbi(shared+3*CHUNK_SIZE,                  \
          myvals+3*CHUNK_SIZE, CHUNK_SIZE, target_pe);                 \
                                                                       \
      shmem_quiet();                                                   \
      shmem_barrier_all();                                             \
                                                                       \
      shmem_##TYPENAME##_get(result,shared,CHUNK_SIZE,target_pe);      \
      for(i = 0; i < CHUNK_SIZE; ++i) {                                \
          result[CHUNK_SIZE+i] = shmem_##TYPENAME##_g(                 \
                                  &shared[CHUNK_SIZE+i], target_pe);   \
      }                                                                \
      shmem_##TYPENAME##_iget(result+2*CHUNK_SIZE, shared+2*CHUNK_SIZE,\
          2, 1, CHUNK_SIZE/2, target_pe);                              \
      shmem_##TYPENAME##_iget(result+2*CHUNK_SIZE+1,                   \
          shared+2*CHUNK_SIZE+CHUNK_SIZE/2, 2, 1, CHUNK_SIZE/2,        \
          target_pe);                                                  \
      shmem_##TYPENAME##_get_nbi(result+3*CHUNK_SIZE,                  \
          shared+3*CHUNK_SIZE, CHUNK_SIZE, target_pe);                 \
                                                                       \
      shmem_quiet();                                                   \
      shmem_barrier_all();                                             \
      int ret = 0;                                                     \
      for(i = 0; i < ARR_SIZE; ++i) {                                  \
          if(result[i] != myvals[i]) {                                 \
              ++ret;                                                   \
              if(verbose) {                                            \
                  fprintf(stderr,"result[%lu] != myvals[%lu]", i, i);  \
              }                                                        \
          }                                                            \
      }                                                                \
      if(verbose) {                                                    \
          fprintf(stderr,"%s (type '%s') %s: %d\n",#TYPENAME,          \
                  #TYPE,ret ? "Failed" : "Succeeded",ret);             \
      }                                                                \
      return ret;                                                      \
  }

SHMEM_DEFINE_FOR_RMA(DECLARE_TEST);

