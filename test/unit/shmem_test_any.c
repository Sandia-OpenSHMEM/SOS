/*
 *  Copyright (c) 2018 Intel Corporation. All rights reserved.
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
 *
 * This test is derived from an example provided in the OpenSHMEM 1.4
 * specification.  Additional copyrights may apply.
 *
 */

#include <shmem.h>
#include <shmemx.h>
#include <stdlib.h>
 
int main(void)
{
  shmem_init();
  int mype = shmem_my_pe();
  int npes = shmem_n_pes();
 
  int *flags = shmem_calloc(npes, sizeof(int));
  int *status = calloc(npes, sizeof(int));
  int *processed = calloc(npes, sizeof(int));
 
  for (int i = 0; i < npes; i++)
      shmem_p(&flags[mype], 1, i);
 
  int ncompleted = 0, completed_idx;
 
  while (ncompleted < npes) {
      completed_idx = shmemx_test_any(flags, npes, status, SHMEM_CMP_EQ, 1);
      if (completed_idx != SIZE_MAX && !processed[completed_idx]) {
          ncompleted++;
          processed[completed_idx] = 1;
      } else {
          /* Overlap some computation here */
      }
  }
 
  free(status);
  shmem_free(flags);
  shmem_finalize();
  return 0;
}
