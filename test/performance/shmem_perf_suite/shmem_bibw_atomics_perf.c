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
**  Features of Test: bi-directional bandwidth
**
**  -by default megabytes/second results
**
**NOTE: this test assumes correctness of reduction algorithm
*/

#include <bw_common.h>


#define bi_bw(len, metric_info, streaming_node, NAME, TYPE, op)               \
    do {                                                                       \
        double start = 0.0, end = 0.0;                                         \
        int i = 0, j = 0, num_itr = metric_info->trials + metric_info->warmup; \
        int dest = partner_node(*metric_info);                                 \
        shmem_barrier_all();                                                   \
                                                                               \
        switch(op) {                                                       \
            case OP_ADD:                                                   \
                for(i = 0; i < num_itr; i++) {                             \
                    if(metric_info->my_node == 0 && i == metric_info->warmup)   \
                        start = perf_shmemx_wtime();                       \
                                                                               \
                    for(j = 0; j < metric_info->window_size; j++)          \
                        shmem_##NAME##_add((TYPE *)(metric_info->dest),    \
                            (TYPE)(metric_info->my_node), dest);           \
                                                                               \
                    shmem_quiet();                                         \
                }                                                          \
                if(metric_info->my_node == 0)                              \
                    end = perf_shmemx_wtime();                             \
            break;                                                         \
            case OP_INC:                                                   \
                for(i = 0; i < num_itr; i++) {                             \
                    if(metric_info->my_node == 0 && i == metric_info->warmup)   \
                        start = perf_shmemx_wtime();                       \
                                                                               \
                    for(j = 0; j < metric_info->window_size; j++)          \
                        shmem_##NAME##_inc((TYPE *)(metric_info->dest),    \
                            dest);                                         \
                                                                               \
                    shmem_quiet();                                         \
                }                                                          \
                if(metric_info->my_node == 0)                              \
                    end = perf_shmemx_wtime();                                 \
            break;                                                         \
            case OP_FADD:                                                  \
                for(i = 0; i < num_itr; i++) {                             \
                    if(metric_info->my_node == 0 && i == metric_info->warmup)   \
                        start = perf_shmemx_wtime();                       \
                                                                               \
                    for(j = 0; j < metric_info->window_size; j++)          \
                        shmem_##NAME##_fadd((TYPE *)(metric_info->dest),   \
                            (TYPE)(metric_info->my_node), dest);           \
                }                                                          \
                if(metric_info->my_node == 0)                              \
                    end = perf_shmemx_wtime();                                 \
            break;                                                         \
            case OP_FINC:                                                  \
                for(i = 0; i < num_itr; i++) {                             \
                    if(metric_info->my_node == 0 && i == metric_info->warmup)   \
                        start = perf_shmemx_wtime();                       \
                                                                               \
                    for(j = 0; j < metric_info->window_size; j++)          \
                        shmem_##NAME##_finc((TYPE *)(metric_info->dest),   \
                            dest);                                         \
                }                                                          \
                if(metric_info->my_node == 0)                              \
                    end = perf_shmemx_wtime();                                 \
            break;                                                         \
            default:                                                       \
                fprintf(stderr, "Error %d not a valid op case              \
                                                 for atomics\n", op);           \
                break;                                                         \
        }                                                                  \
            if(metric_info->my_node == 0)                                      \
                calc_and_print_results((end - start), len, *metric_info);      \
    } while(0)


#define NUM_INC 100

#define SIZE 3

typedef enum {
    OP_ADD,
    OP_INC,
    OP_FADD,
    OP_FINC,
    SIZE_OF_OP
} atomic_op_type;

static const char * op_names [] = { "add", "inc", "fadd", "finc" };

static inline void bw_set_metric_info_len(perf_metrics_t *metric_info)
{
    unsigned int atomic_sizes[SIZE] = {sizeof(int), sizeof(long),
                                        sizeof(long long)};
    atomic_op_type op_type = OP_ADD;

    for(op_type = OP_ADD; op_type < SIZE_OF_OP; op_type++)
    {
        if(metric_info->my_node == 0)
            printf("%s\n", op_names[op_type]);

        metric_info->start_len = atomic_sizes[0];
        metric_info->max_len = atomic_sizes[0];
        metric_info->size_inc = NUM_INC;

        bi_bw(atomic_sizes[0], metric_info, snode, int, int, op_type);

        metric_info->start_len = atomic_sizes[1];
        metric_info->max_len = atomic_sizes[1];

        bi_bw(atomic_sizes[1], metric_info, snode, long, long, op_type);

        metric_info->start_len = atomic_sizes[2];
        metric_info->max_len = atomic_sizes[2];

        bi_bw(atomic_sizes[2], metric_info, snode, longlong, long long, op_type);
    }
}

void bi_dir_bw(int len, perf_metrics_t *metric_info)
{
    bw_set_metric_info_len(metric_info);
}

int main(int argc, char *argv[])
{
    bi_dir_bw_main(argc, argv);

    return 0;
}
