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
#include <uni_dir.h>
#include <assert.h>
#include <omp.h>

int main(int argc, char *argv[])
{
  int tl,tl_provided;
  tl = SHMEMX_THREAD_MULTIPLE;

  /*must be before data_init*/
  shmemx_init_thread(SHMEMX_THREAD_MULTIPLE,&tl_provided);
  assert(tl_provided == tl);

  uni_dir_bw_main(argc, argv);

  shmem_finalize();

  return 0;
}

void
uni_dir_bw(int len, perf_metrics_t *metric_info)
{
  int n_threads = omp_get_max_threads();
  int n_dom = n_threads;
  shmemx_domain_t* doms = malloc(sizeof(shmemx_domain_t)*n_dom);
  shmemx_ctx_t* contexts = malloc(sizeof(shmemx_ctx_t)*n_threads);
  int streaming = !streaming_node(*metric_info);
  double start = 0.0, end = 0.0;
  int i = 0, j = 0;
  int dest = partner_node(*metric_info);

  shmemx_domain_create(SHMEMX_THREAD_MULTIPLE,n_dom,doms);
  for(i = 0; i < n_threads; ++i) {
    shmemx_ctx_create(doms[i%n_dom],&contexts[i]);
  }

  /* FIXME: This is a horrible way to change the buffer, but I didn't
   * want to mess with the initialization code in common.h
   */
  aligned_buffer_free(metric_info->src);
  aligned_buffer_free(metric_info->dest);

  metric_info->src = aligned_buffer_alloc(metric_info->max_len*n_threads);
  init_array(metric_info->src, metric_info->max_len*n_threads, metric_info->my_node);

  metric_info->dest = aligned_buffer_alloc(metric_info->max_len*n_threads);
  init_array(metric_info->dest, metric_info->max_len*n_threads, metric_info->my_node);

  /* end FIXME */

  shmem_barrier_all();

  if (streaming) {
    for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
      if(i == metric_info->warmup)
        start = perf_shmemx_wtime();

#pragma omp parallel for
      for(j = 0; j < metric_info->window_size; j++) {
        shmemx_ctx_putmem(metric_info->dest, metric_info->src, len,
            dest, contexts[omp_get_thread_num()]);
      }

#pragma omp parallel for
      for(j = 0; j < n_threads; j++) {
        shmemx_ctx_quiet(contexts[j]);
      }
    }
    end = perf_shmemx_wtime();

    calc_and_print_results((end - start), len, *metric_info);
  }

  for(i = 0; i < n_threads; ++i) {
    shmemx_ctx_destroy(contexts[i]);
  }
  shmemx_domain_destroy(n_dom,doms);

  free(contexts);
  free(doms);
}
