/*
 *  Copyright (c) 2020 Intel Corporation. All rights reserved.
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

#include <shmem.h>
#include <shmemx.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  int ret = 0, i, errors = 0;

  shmem_init();

  int me = shmem_my_pe();
  int npes = shmem_n_pes();

  int *var_alloc = (int *) shmemx_malloc_varsize((me + 1) * sizeof(int), npes * sizeof(int));
  int *src = (int *) malloc((me + 1) * sizeof(int));
  if (!var_alloc || !src) {
    fprintf(stdout, "malloc failed\n");
    ret = -1;
    goto fn_end;
  }

  for (i = 0; i <= me; i++) {
    src[i] = me + 1 + i;
  }

  shmem_barrier_all();

  shmem_int_put(var_alloc, src, me + 1, (me + 1) % npes);

  shmem_barrier_all();

  if (!me) {
    for (i = 0; i < npes; i++) {
      if (var_alloc[i] != npes + i) {
        errors++;
        fprintf(stdout, "Invalid data found at %d is %d, expected %d\n", i, var_alloc[i], npes + i);
      }
    }
  }
  ret = errors;

  shmem_free(var_alloc);
fn_end:
  shmem_finalize();
  return ret;
}
