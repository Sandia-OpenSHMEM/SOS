/*
 *  Copyright (c) 2017 Intel Corporation. All rights reserved.
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

int get_size_of_side(perf_metrics_t my_info) {
    if(my_info.my_node < my_info.midpt)
        return my_info.szinitiator;
    else
        return my_info.sztarget;
}

int get_num_partners(perf_metrics_t my_info) {
    int unused_PEs = 0, num_partners = 0, num_xtra_partners = 0;
    int active_PEs = get_size_of_side(my_info);

    if(active_PEs == my_info.midpt)
        return 1;

    unused_PEs = my_info.midpt - active_PEs;
    num_partners = my_info.midpt / active_PEs;
    num_xtra_partners = unused_PEs % active_PEs;

    if((my_info.my_node % my_info.midpt) < num_xtra_partners)
        num_partners++;

    return num_partners;
}

/* target only needs to know num of partners */
int *get_initiators_partners(perf_metrics_t my_info, int num_partners) {
    int node_to_shadow = my_info.my_node;
    int i = 0;
    int *partner_nodes = NULL;

    assert(my_info.cstyle == COMM_PAIRWISE && !target_node(my_info));
    if(num_partners < 1)
        return partner_nodes;

    partner_nodes = (int *) malloc(sizeof(int) * num_partners);
    assert(partner_nodes);

    for(i = 0; i < num_partners; i++) {
        partner_nodes[i] = ((node_to_shadow % my_info.sztarget) + my_info.midpt);
        node_to_shadow += my_info.szinitiator;
    }

    return partner_nodes;
}

void static inline target_data_uni_bw(int len, perf_metrics_t metric_info)
{
    double start = 0.0, end = 0.0;
    int i = 0, j = 0;
    int snode = (metric_info.num_pes != 1)? streaming_node(metric_info) : true;
    int num_partners = get_num_partners(metric_info);
    static int completion_signal = 0;
    int *my_PE_partners = (snode ?
        get_initiators_partners(metric_info, num_partners): NULL);

    shmem_barrier_all();
    start = perf_shmemx_wtime();

    if(target_node(metric_info)) {
        shmem_int_wait_until(&completion_signal, SHMEM_CMP_EQ, num_partners);
        end = perf_shmemx_wtime();

        calc_and_print_results((end - start), len, metric_info);
    } else if(snode) {
        for (i = 0; i < num_partners; i++) {
            for(j = 0; j < metric_info.trials + metric_info.warmup; j++) {
                shmem_putmem(metric_info.dest, metric_info.src, len, my_PE_partners[i]);
                shmem_quiet();
            }
            shmem_int_inc(&completion_signal, my_PE_partners[i]);
        }
        end = perf_shmemx_wtime();
        free(my_PE_partners);
    }

    shmem_barrier_all();
    completion_signal = 0;

    if(snode)
        calc_and_print_results((end - start), len, metric_info);
}

void static inline target_bw_itr(int len, perf_metrics_t *metric_info)
{
    target_data_uni_bw(len, *metric_info);

    metric_info->start_len = TARGET_SZ_MAX;
    len = TARGET_SZ_MAX;

    target_data_uni_bw(len, *metric_info);

    /* stopping upper layer from iterating, we are done */
    metric_info->max_len = TARGET_SZ_MIN;
}
