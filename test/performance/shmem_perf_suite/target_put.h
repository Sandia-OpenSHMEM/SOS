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

static inline int get_size_of_side(const perf_metrics_t * const my_info) {
    if(my_info->my_node < my_info->midpt)
        return my_info->szinitiator;
    else
        return my_info->sztarget;
}

static inline int get_size_of_other_side(const perf_metrics_t * const my_info) {
    if(my_info->my_node < my_info->midpt)
        return my_info->sztarget;
    else
        return my_info->szinitiator;
}

static inline int get_num_partners(perf_metrics_t * const my_info, int snode) {
    int unused_PEs = 0, num_partners = 0;
    int active_PEs = get_size_of_side(my_info);
    int other_side = get_size_of_other_side(my_info);

    if(active_PEs >= other_side) 
        return 1;

    num_partners = other_side / active_PEs;
    unused_PEs = other_side % active_PEs;

    if (snode) {
        if((my_info->my_node % active_PEs) < unused_PEs)
            num_partners++;
    } else {
        if(((my_info->my_node - my_info->midpt) % active_PEs) < unused_PEs)
            num_partners++;
    }

    return num_partners;
}

/* target only needs to know num of partners */
static inline int *get_initiators_partners(const perf_metrics_t * const my_info, int num_partners) {
    int node_to_shadow = my_info->my_node;
    int i = 0;
    int *partner_nodes = NULL;

    assert(my_info->cstyle == COMM_PAIRWISE && !target_node(my_info));
    if(num_partners < 1)
        return partner_nodes;

    partner_nodes = (int *) malloc(sizeof(int) * num_partners);
    assert(partner_nodes);

    for(i = 0; i < num_partners; i++) {
        partner_nodes[i] = ((node_to_shadow % my_info->sztarget) + my_info->midpt);
        node_to_shadow += my_info->szinitiator;
    }

    return partner_nodes;
}

static inline void target_data_uni_bw(int len, perf_metrics_t * const metric_info)
{
    double start = 0.0, end = 0.0;
    int i = 0;
    unsigned long int j, k;
    int snode = (metric_info->num_pes != 1)? streaming_node(metric_info) : true;
    int num_partners = get_num_partners(metric_info, snode);
    static int completion_signal = 0;
    int *my_PE_partners = (snode ?
        get_initiators_partners(metric_info, num_partners): NULL);

    metric_info->num_partners = num_partners;
    shmem_barrier_all();
    if (target_node(metric_info)) {
        shmem_int_wait_until(&completion_signal, SHMEM_CMP_EQ, num_partners);
    } else if (snode) {
        for (i = 0; i < num_partners; i++) {
            for(j = 0; j < metric_info->warmup; j++) {
                for(k = 0; k < metric_info->window_size; k++) {
#ifdef USE_NONBLOCKING_API
                    shmem_putmem_nbi(metric_info->dest, metric_info->src, len, my_PE_partners[i]);
#else
                    shmem_putmem(metric_info->dest, metric_info->src, len, my_PE_partners[i]);
#endif
                }
                shmem_quiet();
            }
            shmem_int_atomic_inc(&completion_signal, my_PE_partners[i]);
        }
    }

    completion_signal = 0;
    shmem_barrier_all();
    start = perf_shmemx_wtime();

    if (target_node(metric_info)) {
        shmem_int_wait_until(&completion_signal, SHMEM_CMP_EQ, num_partners);
    } else if (snode) {
        for (i = 0; i < num_partners; i++) {
            for(j = 0; j < metric_info->trials; j++) {
                for(k = 0; k < metric_info->window_size; k++) {
#ifdef USE_NONBLOCKING_API
                    shmem_putmem_nbi(metric_info->dest, metric_info->src, len, my_PE_partners[i]);
#else
                    shmem_putmem(metric_info->dest, metric_info->src, len, my_PE_partners[i]);
#endif
                }
                shmem_quiet();
            }
            shmem_int_atomic_inc(&completion_signal, my_PE_partners[i]);
        }
    }

    if (snode || target_node(metric_info)) {
        end = perf_shmemx_wtime();
        calc_and_print_results(end, start, len, metric_info);
    }
    completion_signal = 0;
    free(my_PE_partners);
}

static inline void target_bw_itr(int len, perf_metrics_t * const metric_info)
{
    target_data_uni_bw(len, metric_info);
}
