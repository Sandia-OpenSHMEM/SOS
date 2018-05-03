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

#define PUT_IO_NODE 1
#define GET_IO_NODE !PUT_IO_NODE
#define INIT_VALUE 1

static 
void init_metrics(perf_metrics_t *metric_info) {
    metric_info->t_type = LAT;
    set_metric_defaults(metric_info);
    metric_info->target = NULL;
}

static inline 
void print_latency_header(perf_metrics_t metric_info) {
    printf("\nMessage Size%15sLatency\n", " ");
    printf("%4sin bytes%17sin us\n", " ", " ");
}

/* calculation and printing of the latency */
static inline 
void calc_and_print_results(double start, double end, int len,
                                         perf_metrics_t data) {
    double latency = 0.0;
    latency = (end - start) / data.trials;

    printf("%2s%10d%12s%10.2f\n", " ", len, " ", latency);
}

static inline 
int command_line_arg_check(int argc, char *argv[],
                            perf_metrics_t *metric_info) {
    int ch, error = false;
    extern char *optarg;

    /* check command line args */
    while ((ch = getopt(argc, argv, "e:s:n:v")) != EOF) {
        switch (ch) {
        case 's':
            metric_info->start_len = strtoul(optarg, (char **)NULL, 0);
            if ( metric_info->start_len < 1 ) metric_info->start_len = 1;
            if(!is_pow_of_2(metric_info->start_len)) {
                fprintf(stderr, "Error: start_length must be a power of two\n");
                error = true;
            }
            if (metric_info->start_len > INT_MAX) {
                fprintf(stderr, "Error: start_length is out of integer range\n");
                error = true;
            }
            break;
        case 'e':
            metric_info->max_len = strtoul(optarg, (char **)NULL, 0);
            if(!is_pow_of_2(metric_info->max_len)) {
                fprintf(stderr, "Error: end_length must be a power of two\n");
                error = true;
            }
            if(metric_info->max_len < metric_info->start_len) {
                fprintf(stderr, "Error: end_length (%ld) must be >= "
                        "start_length (%ld)\n", metric_info->max_len,
                        metric_info->start_len);
                error = true;
            }
            if (metric_info->max_len > INT_MAX) {
                fprintf(stderr, "Error: end_length is out of integer range\n");
                error = true;
            }
            break;
        case 'n':
            metric_info->trials = strtoul(optarg, (char **)NULL, 0);
            if(metric_info->trials < (metric_info->warmup * 2)) {
                fprintf(stderr, "Error: trials (%ld) must be >= 2*warmup "
                        "(%ld)\n", metric_info->trials, metric_info->warmup * 2);
                error = true;
            }
            break;
        case 'v':
            metric_info->validate = true;
            break;
        default:
            error = true;
            break;
        }
    }

    if (error) {
        if (metric_info->my_node == 0) {
            fprintf(stderr, "Usage: [-s start_length] [-e end_length] "\
                    ": lengths must be a power of two \n " \
                    "[-n trials (must be greater than 20)] "\
                    "[-v (validate results)]\n");
        }
        return -1;
    }

    return 0;
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
    thread_safety_validation_check(metric_info);

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
