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
 */

#include <common.h>

#define INIT_VALUE 1

static 
void init_metrics(perf_metrics_t *metric_info) {
    metric_info->t_type = LAT;
    set_metric_defaults(metric_info);
    metric_info->target = NULL;
    metric_info->cstyle = COMM_PAIRWISE;
}

static inline 
void print_latency_header(perf_metrics_t metric_info) {
    printf("\nMessage Size%15sLatency\n", " ");
    printf("%4sin bytes%17sin us\n", " ", " ");
}

/* calculation and printing of the latency */
static inline 
void calc_and_print_results(double start, double end, int len,
                            perf_metrics_t metric_info) {
    int stride = 0, start_pe = 0, nPEs = 0;
    int nred_elements = 1;
    static double latency = 0.0, avg_latency = 0.0;
    static double pwrk[SHMEM_REDUCE_MIN_WRKDATA_SIZE];
    
    PE_set_used_adjustments(&nPEs, &stride, &start_pe, metric_info);

    if (end > 0 && start > 0 && (end - start) > 0) {
        latency = (end - start) / metric_info.trials;
    } else {
        fprintf(stderr, "Incorrect time measured from latency test: "
                        "start = %lf, end = %lf\n", start, end);
    }

    if (metric_info.individual_report == 1) {
        printf("Individual latency for PE %6d is %10.2f\n",
                metric_info.my_node, latency);
    }
    shmem_barrier(start_pe, stride, nPEs, bar_psync);

    if (nPEs >= 2) {
        shmem_double_sum_to_all(&avg_latency, &latency, 
                                nred_elements, start_pe, stride,
                                nPEs, pwrk, red_psync);
        avg_latency /= nPEs;
    } else {
        avg_latency = latency;
    }

    if (metric_info.my_node == 0) {
        printf("%2s%10d%12s%10.2f\n", " ", len, " ", avg_latency);
    }

}

/**************************************************************/
/*                   Latency data gathering                   */
/**************************************************************/

/*have single symmetric long element "target" from perf_metrics_t
 *  that needs to be initialized in function*/
extern void long_element_round_trip_latency(perf_metrics_t data);

extern void int_element_latency(perf_metrics_t data);

/*have symmetric buffers src/dest from perf_metrics_t
 *  that has been initialized to my_node number */
extern void streaming_latency(int len, perf_metrics_t *data);

static inline 
void multi_size_latency(perf_metrics_t data, char *argv[]) {
    unsigned int len;
    int partner_pe = partner_node(data);

    if (data.my_node == 0) {
        print_latency_header(data);
    }

    for (len = data.start_len; len <= data.max_len; len *= data.size_inc) {
        large_message_metric_chg(&data, len);
        streaming_latency(len, &data);
    }

    shmem_barrier_all();

    if((data.my_node == 0) && data.validate)
        validate_recv(data.dest, data.max_len, partner_pe);
}



/**************************************************************/
/*                   INIT and teardown of resources           */
/**************************************************************/

static inline 
int latency_init_resources(int argc, char *argv[],
                           perf_metrics_t *metric_info) {
    init_metrics(metric_info);
    int ret = command_line_arg_check(argc, argv, metric_info);
    if (ret != 0) {
        return ret;
    }

#ifndef VERSION_1_0
    int tl;
    shmem_init_thread(metric_info->thread_safety, &tl);
    if(tl != metric_info->thread_safety) {
        fprintf(stderr,"Could not initialize with requested thread "
                "level %d: got %d\n", metric_info->thread_safety, tl);
        return -2;
    }
#else
    start_pes(0);
#endif

    update_metrics(metric_info);
    if (error_checking_init_target_usage(metric_info) == -1)
        return -2;
#if defined(ENABLE_THREADS)
    thread_safety_validation_check(metric_info);
#endif
    init_psync_arrays();

    if(only_even_PEs_check(metric_info->my_node, metric_info->num_pes) != 0) {
        return -2;
    }

    metric_info->src = aligned_buffer_alloc(metric_info->max_len);
    init_array(metric_info->src, metric_info->max_len, metric_info->my_node);

    metric_info->dest = aligned_buffer_alloc(metric_info->max_len);
    init_array(metric_info->dest, metric_info->max_len, metric_info->my_node);

#ifndef VERSION_1_0
    metric_info->target = shmem_malloc(sizeof(long));
#else
    metric_info->target = shmalloc(sizeof(long));
#endif

    return 0;
}

static inline 
void latency_free_resources(perf_metrics_t *metric_info) {
    shmem_barrier_all();

#ifndef VERSION_1_0
    shmem_free(metric_info->target);
#else
    shfree(metric_info->target);
#endif
    aligned_buffer_free(metric_info->src);
    aligned_buffer_free(metric_info->dest);
}

static inline 
void latency_finalize(void) {
#ifndef VERSION_1_0
    shmem_finalize();
#endif
}

static inline 
void latency_main(int argc, char *argv[]) {
    perf_metrics_t metric_info;

    int ret = latency_init_resources(argc, argv, &metric_info);

    if (ret == 0) {
        if (metric_info.my_node == 0) {
            print_header(metric_info);
        }
        long_element_round_trip_latency(metric_info);
        int_element_latency(metric_info);
        multi_size_latency(metric_info, argv);
        latency_free_resources(&metric_info);
    }
    if (ret != -1) {
        latency_finalize();
    }
}
