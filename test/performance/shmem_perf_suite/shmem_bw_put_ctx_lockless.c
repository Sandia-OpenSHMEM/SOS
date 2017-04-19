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
#include <unistd.h>
#include <shmemx.h>

int n_threads, n_dom;
shmemx_domain_t* doms;
shmemx_ctx_t* contexts;
int first = 1;
char hostname[1024];
uint64_t bytes_sent = 0;


int main(int argc, char *argv[])
{
  int i;
  int tl,tl_provided;
  tl = SHMEMX_THREAD_MULTIPLE;

  n_threads = 8;
  n_dom = n_threads;
  doms = malloc(sizeof(shmemx_domain_t)*n_dom);
  contexts = malloc(sizeof(shmemx_ctx_t)*n_threads);
  omp_set_num_threads(n_threads);

  /*must be before data_init*/
  shmemx_init_thread(SHMEMX_THREAD_MULTIPLE,&tl_provided);
  assert(tl_provided == tl);

  shmemx_domain_create(SHMEMX_THREAD_SINGLE,n_dom,doms);
  for(i = 0; i < n_threads; ++i) {
    shmemx_ctx_create(doms[i%n_dom],&contexts[i]);
  }

  gethostname(hostname,sizeof(hostname));

  printf("PE %d on host %s\n",shmem_my_pe(),hostname);
  fflush(stdout);

  uni_dir_bw_main(argc, argv);

  shmem_barrier_all();

  for(i = 0; i < n_threads; ++i) {
    shmemx_ctx_destroy(contexts[i]);
  }
  shmemx_domain_destroy(n_dom,doms);

  free(contexts);
  free(doms);

  shmem_finalize();
printf("%llu bytes sent\n",bytes_sent);

  return 0;
}

void
uni_dir_bw(int len, perf_metrics_t *metric_info)
{
  int streaming = !streaming_node(*metric_info);
  double start = 0.0, end = 0.0;
  int i = 0, j = 0, k = 0;
  int dest = partner_node(*metric_info);


  /* FIXME: This is a horrible way to change the buffer, but I didn't
   * want to mess with the initialization code in common.h
   */
  if(first) {
    first = 0;
  
    metric_info->src = aligned_buffer_alloc(metric_info->max_len*n_threads);
    init_array(metric_info->src, metric_info->max_len*n_threads, metric_info->my_node);
  
    metric_info->dest = aligned_buffer_alloc(metric_info->max_len*n_threads);
    init_array(metric_info->dest, metric_info->max_len*n_threads, metric_info->my_node);
  }
  /* end FIXME */

  shmem_barrier_all();

  int chunksize = len/n_threads;
  int numchunks = (len + chunksize - 1)/chunksize;

  if (streaming) {
    for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
      if(i == metric_info->warmup)
        start = perf_shmemx_wtime();

#pragma omp parallel for
	for(j = 0; j < n_threads; ++j) {
		int start = j*chunksize;
		int end = (j+1)*chunksize;
		if(end > len) { end = len; }
	      for(k = 0; k < metric_info->window_size; k++) {
		shmemx_ctx_putmem(metric_info->dest+start, metric_info->src+start,
				end - start, dest, contexts[j]);
		shmemx_ctx_quiet(contexts[j]);
	      }
	}

    }
    end = perf_shmemx_wtime();

    for(i = 0; i < metric_info->trials + metric_info->warmup; ++i) {
        for(j = 0; j < metric_info->window_size; ++j) {
          bytes_sent += len;
        }
    }

    calc_and_print_results((end - start), len, *metric_info);
  }
}
