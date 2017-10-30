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
#include <string.h>
#include <stdio.h>
#include <complex.h>
#include <shmem.h>

#define MAX(a, b) ((a) > (b)) ? (a) : (b)

#define DECLARE_FOR(TYPE) \
  extern "C"  {                                                     \
    TYPE _Complex TYPE##_dest[10];                                  \
    TYPE _Complex TYPE##_src[10];                                   \
    TYPE _Complex* TYPE##_workData;                                 \
    TYPE _Complex* TYPE##_alloc_data() {                            \
      size_t size = sizeof(TYPE##_src[0]);                          \
      size_t minData = SHMEM_REDUCE_MIN_WRKDATA_SIZE*size;          \
      size_t workDataSize = MAX(sizeof(TYPE##_src),minData);        \
      return (TYPE _Complex*)shmem_malloc(workDataSize);            \
    }                                                               \
  }

long syncArr[SHMEM_REDUCE_SYNC_SIZE];

#define TEST_COMPLEX(TYPE,LETTER,OP) \
  {                                                                  \
    TYPE##_workData = TYPE##_alloc_data();                           \
                                                                     \
    memset(TYPE##_src,0,sizeof(TYPE##_src));                         \
                                                                     \
    shmem_complex##LETTER##_##OP##_to_all(TYPE##_dest,TYPE##_src,10, \
      0,0, shmem_n_pes(), TYPE##_workData, syncArr);                 \
                                                                     \
    shmem_barrier_all();                                             \
                                                                     \
    if(shmem_my_pe() == 0) {                                         \
      int i;                                                         \
      for(i = 1; i < shmem_n_pes(); ++i) {                           \
        shmem_getmem(TYPE##_src,TYPE##_dest,sizeof(TYPE##_dest),i);  \
        if(0 != memcmp(TYPE##_src,TYPE##_dest,sizeof(TYPE##_src))) { \
          ++rc;                                                      \
        }                                                            \
      }                                                              \
    }                                                                \
                                                                     \
    shmem_barrier_all();                                             \
  }

DECLARE_FOR(float);
DECLARE_FOR(double);

int main(int argc, char* argv[]) {
  shmem_init();

  int rc = 0;

  TEST_COMPLEX(float,f,sum);
  TEST_COMPLEX(float,f,prod);
  TEST_COMPLEX(double,d,sum);
  TEST_COMPLEX(double,d,prod);

  shmem_finalize();
  return rc;
}

