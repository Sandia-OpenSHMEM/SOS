/*
 *  Copyright (c) 2015 Intel Corporation. All rights reserved.
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
**
**  This is a bandwidth centric test for put: back-to-back message rate
**
**  Features of Test: uni-directional bandwidth
**
**  -by default megabytes/second results
**
**NOTE: this test assumes correctness of reduction algorithm
*/

#include <bw_common.h>
#include <assert.h>
#include <omp.h>
#include <unistd.h>
#include <shmem.h>

int main(int argc, char *argv[])
{
  uni_dir_bw_main(argc, argv, STYLE_PUT);

  return 0;
}

void uni_dir_bw(int len, perf_metrics_t *metric_info)
{
  int streaming = !streaming_node(*metric_info);
  double start = 0.0, end = 0.0;
  int j = 0;
  int dest = partner_node(*metric_info);

  char *src = aligned_buffer_alloc (metric_info->nthreads * len);
  char *dst = aligned_buffer_alloc (metric_info->nthreads * len);
  assert(src && dst);

  shmem_barrier_all();

  if (streaming) {
      int i, k;
      int chunksize = len < metric_info->nthreads ? 1 : len/metric_info->nthreads;
      shmem_ctx_t *contexts = malloc(sizeof(shmem_ctx_t) * metric_info->nthreads);

      for(i = 0; i < metric_info->nthreads; ++i) {
        shmem_ctx_create(SHMEM_CTX_PRIVATE, &contexts[i]);
      }
    
      for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
        if(i == metric_info->warmup) {
#pragma omp barrier // Keep threads in sync

#pragma omp master
          {
            start = perf_shmemx_wtime();
          }
        }
#pragma omp parallel for
        for(j = 0; j < metric_info->nthreads; ++j) {
          int start_index = j * chunksize;
          int end_index = (j + 1) * chunksize;
          if(end_index > len) { 
            end_index = len; 
          }
          for(k = 0; k < metric_info->window_size; k++) {
            shmem_ctx_putmem(contexts[j], dst + start_index, src + start_index,
			end_index - start_index, dest);
            shmem_ctx_quiet(contexts[j]);
	  }
        }
      }
      end = perf_shmemx_wtime();

      for(i = 0; i < metric_info->nthreads; ++i) {
        shmem_ctx_destroy(contexts[i]);
      }
      free(contexts);
    calc_and_print_results((end - start), len, *metric_info);
  }

  shmem_barrier_all();

  aligned_buffer_free(src);
  aligned_buffer_free(dst);

}
