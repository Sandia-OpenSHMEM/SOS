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

#define MAX_MSG_SIZE (1<<23)
#define START_LEN 1

#define INC 2
#define TRIALS 100
#define WARMUP 10

typedef struct perf_metrics {
   unsigned int start_len, max_len;
   unsigned int inc, trials;
   unsigned int warmup;
   int validate;
   int my_node, npes;
   long * target;
   char * src, *dest;
} perf_metrics_t;

static void data_init(perf_metrics_t * data) {
   data->start_len = START_LEN;
   data->max_len = MAX_MSG_SIZE;
   data->inc = INC;
   data->trials = TRIALS;
   data->warmup = WARMUP; /*number of initial iterations to skip*/
   data->validate = false;
   data->my_node = shmem_my_pe();
   data->npes = shmem_n_pes();
   data->target = NULL;
   data->src = NULL;
   data->dest = NULL;
}

static inline void print_results_header(void) {
   printf("\nLength                  Latency                       \n");
   printf("in bytes            in micro seconds              \n");
}

/*not storing results, only outputing it*/
static inline void calc_and_print_results(double start, double end, int len,
                                         perf_metrics_t data) {
    double latency = 0.0;
    latency = (end - start) / data.trials;

    printf("%9d           %8.2f             \n", len, latency);
}

static inline int partner_node(int my_node)
{
    return ((my_node % 2 == 0) ? (my_node + 1) : (my_node - 1));
}

static inline void command_line_arg_check(int argc, char *argv[],
                            perf_metrics_t *metric_info) {
    int ch, error = false;
    extern char *optarg;

    /* check command line args */
    while ((ch = getopt(argc, argv, "e:s:n:v")) != EOF) {
        switch (ch) {
        case 's':
            metric_info->start_len = strtol(optarg, (char **)NULL, 0);
            if ( metric_info->start_len < 1 ) metric_info->start_len = 1;
            if(!is_pow_of_2(metric_info->start_len)) error = true;
            break;
        case 'e':
            metric_info->max_len = strtol(optarg, (char **)NULL, 0);
            if(!is_pow_of_2(metric_info->max_len)) error = true;
            if(metric_info->max_len < metric_info->start_len) error = true;
            break;
        case 'n':
            metric_info->trials = strtol(optarg, (char **)NULL, 0);
            if(metric_info->trials <= (metric_info->warmup*2)) error = true;
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
#ifndef VERSION_1_0
        shmem_finalize();
#endif
        exit (-1);
    }
}

static inline void only_two_PEs_check(int my_node, int num_pes) {
    if (num_pes != 2) {
        if (my_node == 0) {
            fprintf(stderr, "2-nodes only test\n");
        }
#ifndef VERSION_1_0
        shmem_finalize();
#endif
        exit(77);
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

static inline void multi_size_latency(perf_metrics_t data, char *argv[]) {
    unsigned int len;
    int partner_pe = partner_node(data.my_node);

    for (len = data.start_len; len <= data.max_len; len *= data.inc) {

        shmem_barrier_all();

        streaming_latency(len, &data);

        shmem_barrier_all();
    }

    shmem_barrier_all();

    if((data.my_node == 0) && data.validate)
        validate_recv(data.dest, data.max_len, partner_pe);
}



/**************************************************************/
/*                   INIT and teardown of resources           */
/**************************************************************/

static inline void latency_init_resources(int argc, char *argv[],
                                          perf_metrics_t *data) {
#ifndef VERSION_1_0
    shmem_init();
#else
    start_pes(0);
#endif

    data_init(data);

    only_two_PEs_check(data->my_node, data->npes);

    command_line_arg_check(argc, argv, data);

    data->src = aligned_buffer_alloc(data->max_len);
    init_array(data->src, data->max_len, data->my_node);

    data->dest = aligned_buffer_alloc(data->max_len);
    init_array(data->dest, data->max_len, data->my_node);

#ifndef VERSION_1_0
    data->target = shmem_malloc(sizeof(long));
#else
    data->target = shmalloc(sizeof(long));
#endif
}

static inline void latency_free_resources(perf_metrics_t *data) {
    shmem_barrier_all();

#ifndef VERSION_1_0
    shmem_free(data->target);
#else
    shfree(data->target);
#endif
    aligned_buffer_free(data->src);
    aligned_buffer_free(data->dest);
#ifndef VERSION_1_0
    shmem_finalize();
#endif
}

static inline void latency_main(int argc, char *argv[]) {
    perf_metrics_t data;

    latency_init_resources(argc, argv, &data);

    long_element_round_trip_latency(data);

    int_element_latency(data);

    multi_size_latency(data, argv);

    latency_free_resources(&data);
}
