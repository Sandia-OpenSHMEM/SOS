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

/*
**
**  This is a bandwidth centric test for atomic operations
**
**  Features of Test: uni-directional bandwidth
**
**  -by default megabytes/second results
**
*/
#include <bw_common.h>

#define uni_bw(metric_info, NAME, TYPE, op)                        \
    do {                                                                       \
        double start = 0.0, end = 0.0;                                         \
        unsigned long int i = 0, j = 0;                                        \
        int snode = streaming_node(metric_info);                               \
        int dest = partner_node(metric_info);                                  \
        shmem_barrier_all();                                                   \
                                                                               \
        switch(op) {                                                           \
            case OP_SET:                                                       \
                if(snode) {                                                    \
                    for(i = 0; i < metric_info->warmup; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_set(                         \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                                                                               \
                        shmem_quiet();                                         \
                    }                                                          \
                }                                                              \
                shmem_barrier_all();                                           \
                if(snode) {                                                    \
                    start = perf_shmemx_wtime();                               \
                    for(i = 0; i < metric_info->trials; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_set(                         \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                                                                               \
                        shmem_quiet();                                         \
                    }                                                          \
                    end = perf_shmemx_wtime();                                 \
                }                                                              \
            break;                                                             \
            case OP_AND:                                                       \
                if(snode) {                                                    \
                    for(i = 0; i < metric_info->warmup; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_and(                         \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                                                                               \
                        shmem_quiet();                                         \
                    }                                                          \
                }                                                              \
                shmem_barrier_all();                                           \
                if(snode) {                                                    \
                    start = perf_shmemx_wtime();                               \
                    for(i = 0; i < metric_info->trials; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_and(                         \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                                                                               \
                        shmem_quiet();                                         \
                    }                                                          \
                    end = perf_shmemx_wtime();                                 \
                }                                                              \
            break;                                                             \
            case OP_OR:                                                        \
                if(snode) {                                                    \
                    for(i = 0; i < metric_info->warmup; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_or(                          \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                                                                               \
                        shmem_quiet();                                         \
                    }                                                          \
                }                                                              \
                shmem_barrier_all();                                           \
                if(snode) {                                                    \
                    start = perf_shmemx_wtime();                               \
                    for(i = 0; i < metric_info->trials; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_or(                          \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                                                                               \
                        shmem_quiet();                                         \
                    }                                                          \
                    end = perf_shmemx_wtime();                                 \
                }                                                              \
            break;                                                             \
            case OP_XOR:                                                       \
                if(snode) {                                                    \
                    for(i = 0; i < metric_info->warmup; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_xor(                         \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                                                                               \
                        shmem_quiet();                                         \
                    }                                                          \
                }                                                              \
                shmem_barrier_all();                                           \
                if(snode) {                                                    \
                    start = perf_shmemx_wtime();                               \
                    for(i = 0; i < metric_info->trials; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_xor(                         \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                                                                               \
                        shmem_quiet();                                         \
                    }                                                          \
                    end = perf_shmemx_wtime();                                 \
                }                                                              \
            break;                                                             \
            case OP_ADD:                                                       \
                if(snode) {                                                    \
                    for(i = 0; i < metric_info->warmup; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_add(                         \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                                                                               \
                        shmem_quiet();                                         \
                    }                                                          \
                }                                                              \
                shmem_barrier_all();                                           \
                if(snode) {                                                    \
                    start = perf_shmemx_wtime();                               \
                    for(i = 0; i < metric_info->trials; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_add(                         \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                                                                               \
                        shmem_quiet();                                         \
                    }                                                          \
                    end = perf_shmemx_wtime();                                 \
                }                                                              \
            break;                                                             \
            case OP_INC:                                                       \
                if(snode) {                                                    \
                    for(i = 0; i < metric_info->warmup; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_inc(                         \
                                (TYPE *)(metric_info->dest), dest);            \
                                                                               \
                        shmem_quiet();                                         \
                    }                                                          \
                }                                                              \
                shmem_barrier_all();                                           \
                if(snode) {                                                    \
                    start = perf_shmemx_wtime();                               \
                    for(i = 0; i < metric_info->trials; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_inc(                         \
                                (TYPE *)(metric_info->dest), dest);            \
                                                                               \
                        shmem_quiet();                                         \
                    }                                                          \
                    end = perf_shmemx_wtime();                                 \
                }                                                              \
            break;                                                             \
            case OP_FETCH:                                                     \
                if(snode) {                                                    \
                    for(i = 0; i < metric_info->warmup; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_fetch(                       \
                                (TYPE *)(metric_info->dest), dest);            \
                    }                                                          \
                }                                                              \
                shmem_barrier_all();                                           \
                if(snode) {                                                    \
                    start = perf_shmemx_wtime();                               \
                    for(i = 0; i < metric_info->trials; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_fetch(                       \
                                (TYPE *)(metric_info->dest), dest);            \
                    }                                                          \
                    end = perf_shmemx_wtime();                                 \
                }                                                              \
            break;                                                             \
            case OP_FAND:                                                      \
                if(snode) {                                                    \
                    for(i = 0; i < metric_info->warmup; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_fetch_and(                   \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                    }                                                          \
                }                                                              \
                shmem_barrier_all();                                           \
                if(snode) {                                                    \
                    start = perf_shmemx_wtime();                               \
                    for(i = 0; i < metric_info->trials; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_fetch_and(                   \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                    }                                                          \
                    end = perf_shmemx_wtime();                                 \
                }                                                              \
            break;                                                             \
            case OP_FOR:                                                       \
                if(snode) {                                                    \
                    for(i = 0; i < metric_info->warmup; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_fetch_or(                    \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                    }                                                          \
                }                                                              \
                shmem_barrier_all();                                           \
                if(snode) {                                                    \
                    start = perf_shmemx_wtime();                               \
                    for(i = 0; i < metric_info->trials; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_fetch_or(                    \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                    }                                                          \
                    end = perf_shmemx_wtime();                                 \
                }                                                              \
            break;                                                             \
            case OP_FXOR:                                                      \
                if(snode) {                                                    \
                    for(i = 0; i < metric_info->warmup; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_fetch_xor(                   \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                    }                                                          \
                }                                                              \
                shmem_barrier_all();                                           \
                if(snode) {                                                    \
                    start = perf_shmemx_wtime();                               \
                    for(i = 0; i < metric_info->trials; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_fetch_xor(                   \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                    }                                                          \
                    end = perf_shmemx_wtime();                                 \
                }                                                              \
            break;                                                             \
            case OP_FADD:                                                      \
                if(snode) {                                                    \
                    for(i = 0; i < metric_info->warmup; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_fetch_add(                   \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                    }                                                          \
                }                                                              \
                shmem_barrier_all();                                           \
                if(snode) {                                                    \
                    start = perf_shmemx_wtime();                               \
                    for(i = 0; i < metric_info->trials; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_fetch_add(                   \
                                (TYPE *)(metric_info->dest), ONE, dest);       \
                    }                                                          \
                    end = perf_shmemx_wtime();                                 \
                }                                                              \
            break;                                                             \
            case OP_FINC:                                                      \
                if(snode) {                                                    \
                    for(i = 0; i < metric_info->warmup; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_fetch_inc(                   \
                                (TYPE *)(metric_info->dest), dest);            \
                    }                                                          \
                }                                                              \
                shmem_barrier_all();                                           \
                if(snode) {                                                    \
                    start = perf_shmemx_wtime();                               \
                    for(i = 0; i < metric_info->trials; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_fetch_inc(                   \
                                (TYPE *)(metric_info->dest), dest);            \
                    }                                                          \
                    end = perf_shmemx_wtime();                                 \
                }                                                              \
            break;                                                             \
            case OP_SWAP:                                                      \
                if(snode) {                                                    \
                    for(i = 0; i < metric_info->warmup; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_swap(                        \
                                (TYPE *)(metric_info->src), ONE, dest);        \
                    }                                                          \
                }                                                              \
                shmem_barrier_all();                                           \
                if(snode) {                                                    \
                    start = perf_shmemx_wtime();                               \
                    for(i = 0; i < metric_info->trials; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_swap(                        \
                                (TYPE *)(metric_info->src), ONE, dest);        \
                    }                                                          \
                    end = perf_shmemx_wtime();                                 \
                }                                                              \
            break;                                                             \
            case OP_CSWAP:                                                     \
                if(snode) {                                                    \
                    for(i = 0; i < metric_info->warmup; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_compare_swap(                \
                                (TYPE *)(metric_info->src), dest, ONE, dest);  \
                    }                                                          \
                }                                                              \
                shmem_barrier_all();                                           \
                if(snode) {                                                    \
                    start = perf_shmemx_wtime();                               \
                    for(i = 0; i < metric_info->trials; i++) {                 \
                        for(j = 0; j < metric_info->window_size; j++)          \
                            shmem_##NAME##_atomic_compare_swap(                \
                                (TYPE *)(metric_info->src), dest, ONE, dest);  \
                    }                                                          \
                    end = perf_shmemx_wtime();                                 \
                }                                                              \
            break;                                                             \
            default:                                                           \
                fprintf(stderr, "Error %d not a valid op case                  \
                                                for atomics\n", op);           \
            break;                                                             \
        }                                                                      \
        if(snode) {                                                            \
            calc_and_print_results(end, start, metric_info->start_len, metric_info);             \
        }                                                                      \
    } while(0)

static const char * atomic_op_names [] = { "fetch", "cswap", "swap", "finc", 
                                           "fadd", "fand", "for", 
                                           "fxor", "set", "inc", "add", "and", 
                                           "or", "xor" };


static inline void bw_set_metric_info_len(perf_metrics_t * const metric_info)
{
    atomic_op_type op_type;

    for(op_type = FIRST_FETCH_OP; op_type <= LAST_OP; op_type++) {
        if(metric_info->my_node == 0) { 
            printf("\nshmem_%s\n", atomic_op_names[op_type]);
            printf("-----------\n");
        }

        metric_info->start_len = sizeof(unsigned int);
        shmem_barrier_all();
        uni_bw(metric_info, uint, unsigned int, op_type);

        metric_info->start_len = sizeof(unsigned long);
        shmem_barrier_all();
        uni_bw(metric_info, ulong, unsigned long, op_type);

        metric_info->start_len = sizeof(unsigned long long);
        shmem_barrier_all();
        uni_bw(metric_info, ulonglong, unsigned long long, op_type);
    }
}

void uni_dir_bw(int len, perf_metrics_t * const metric_info)
{
    bw_set_metric_info_len(metric_info);
}

int main(int argc, char *argv[])
{
    uni_dir_bw_main(argc, argv, STYLE_ATOMIC);

    return 0;
}
