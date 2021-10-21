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
#ifdef ENABLE_OPENMP
#include <omp.h>
#endif

static const char * dt_names [] = { "uint", "ulong", "ulonglong" };

/*default settings if no input is provided */
static
void init_metrics(perf_metrics_t *metric_info) {
    metric_info->t_type = BW;
    set_metric_defaults(metric_info);

    metric_info->unit = MB;
    metric_info->target_data = false;
    metric_info->cstyle = COMM_PAIRWISE;
    metric_info->opstyle = STYLE_RMA;
}

static
void update_bw_type(perf_metrics_t *data, int b_type) {
    if (b_type == BI_DIR) {
        data->bw_type_str = "Bi-dir";
        data->b_type = BI_DIR;
    } else {
        data->bw_type_str = "Uni-dir";
        data->b_type = UNI_DIR;
    }
}

/**************************************************************/
/*                   Result Printing and Calc                 */
/**************************************************************/

static
void print_atomic_header(perf_metrics_t * const metric_info) {
    print_header(metric_info);
    printf("\n\nBandwidth test type:    %10s\n", metric_info->bw_type_str);

    if (metric_info->cstyle == COMM_INCAST) {
        printf("Communication style:        INCAST\n");
    } else {
        assert(metric_info->cstyle == COMM_PAIRWISE);
        printf("Communication style:      PAIRWISE\n");
    }

    printf("\nOperation%15sBandwidth%15sMessage Rate%15sLatency\n",
            " ", " ", " ");

    if (metric_info->unit == MB) {
        printf("%19s in mbytes/sec"," ");
    } else if (metric_info->unit == KB) {
        printf("%19s in kbytes/sec", " ");
    } else {
        printf("%20s in bytes/sec", " ");
    }

    printf("%15s in Mops/sec%15s  in us\n", " ", " ");
}

static
void print_bw_header(perf_metrics_t * const metric_info) {
    print_header(metric_info);
    printf("\n\nBandwidth test type:    %10s\n", metric_info->bw_type_str);

    printf("\nMessage Size%15sBandwidth%15sMessage Rate\n", " ", " ");

    printf("%4sin bytes", " ");
    if (metric_info->unit == MB) {
        printf("%11sin mbytes/sec", " ");
    } else if (metric_info->unit == KB) {
        printf("%11sin kbytes/sec", " ");
    } else {
        printf("%12sin bytes/sec", " ");
    }

    printf("%16sin msgs/sec\n", " ");
}

static
void print_data_results(double bw, double mr, const perf_metrics_t * const data,
                            int len, double total_t) {
    static int atomic_type_index = 0;

    if (data->opstyle == STYLE_ATOMIC) {
        printf("%-10s", dt_names[atomic_type_index]);
        atomic_type_index = (atomic_type_index + 1) % ATOMICS_N_DTs;
    } else
        printf("%2s%10d", " ", len);

    if(data->unit == KB) {
        bw = bw * 1.0e3;
    } else if(data->unit == B) {
        bw = bw * 1.0e6;
    }

    if (data->opstyle == STYLE_ATOMIC) {
        printf("%13s%10.2f%15s%12.2f%12s%10.2f", " ", bw, " ",
                mr/1.0e6, " ", total_t/(data->trials * data->window_size));
    } else
        printf("%14s%10.2f%15s%12.2f", " ", bw, " ", mr);

    if(data->target_data) {
        if(data->my_node < data->szinitiator) {
            printf("%2sIniter", " ");
        } else  {
            printf("%2sTarget", " ");
        }
    }

    printf("\n");
}

static inline
void calc_and_print_results(double end_t, double start_t, int len,
                            perf_metrics_t * const metric_info) {
    int stride = 0, start_pe = 0, nPEs = 0;
    static double pe_bw_sum, bw = 0.0; /*must be symmetric for reduction*/
    double pe_bw_avg = 0.0, pe_mr_avg = 0.0;
    int nred_elements = 1;
    static double pe_time_start, pe_time_end,
                  end_time_max = 0.0, start_time_min = 0.0;
    double total_t = 0.0, total_t_max = 0.0;
    int multiplier = 1;

    PE_set_used_adjustments(&nPEs, &stride, &start_pe, metric_info);

    /* 2x as many messages at once for bi-directional */
    if(metric_info->b_type == BI_DIR)
        multiplier = 2;

    if (end_t > 0 && start_t > 0 && (end_t - start_t) > 0) {
        total_t = end_t - start_t;
#ifdef ENABLE_OPENMP
        bw = ((double) len * (double) metric_info->num_partners * (double) multiplier / 1.0e6 *
             metric_info->window_size * metric_info->trials *
             (double) metric_info->nthreads) / (total_t / 1.0e6);
#else
        bw = ((double) len * (double) metric_info->num_partners * (double) multiplier / 1.0e6 *
             metric_info->window_size * metric_info->trials) /
             (total_t / 1.0e6);
#endif
    } else {
        fprintf(stderr, "Incorrect time measured from bandwidth test: "
                        "start = %lf, end = %lf\n", start_t, end_t);
    }

    /* base case: will be overwritten by collective if num_pes > 2 */
    pe_bw_sum = bw;

    if (metric_info->individual_report == 1) {
        if (metric_info->my_node < metric_info->midpt) {
            printf("Individual bandwith for PE %6d (initer) is %10.2f\n",
                metric_info->my_node, pe_bw_sum);
        } else {
            printf("Individual bandwith for PE %6d (target) is %10.2f\n",
                metric_info->my_node, pe_bw_sum);
        }
    }

    pe_time_start = start_t;
    pe_time_end = end_t;
    shmem_barrier_all();
    if (metric_info->cstyle != COMM_INCAST) {
        if (nPEs >= 2) {
            shmem_double_min_reduce(SHMEM_TEAM_WORLD, &start_time_min, &pe_time_start, nred_elements);
            shmem_barrier_all();
            shmem_double_max_reduce(SHMEM_TEAM_WORLD, &end_time_max, &pe_time_end, nred_elements);
        } else if (nPEs == 1) {
            start_time_min = pe_time_start;
            end_time_max = pe_time_end;
        }

        /* calculating bandwidth based on the highest time duration across all PEs */
        if (end_time_max > 0 && start_time_min > 0 &&
           (end_time_max - start_time_min) > 0) {

            total_t_max = (end_time_max - start_time_min);
            int total_transfers = MAX(metric_info->szinitiator, metric_info->sztarget);
#ifdef ENABLE_OPENMP
            bw = ((double) len * (double) multiplier * (double) total_transfers /
                 1.0e6 * metric_info->window_size * metric_info->trials *
                 (double) metric_info->nthreads) / (total_t_max / 1.0e6);
#else
            bw = ((double) len * (double) multiplier * (double) total_transfers /
                 1.0e6 * metric_info->window_size * metric_info->trials) /
                 (total_t_max / 1.0e6);
#endif
        } else {
            fprintf(stderr, "Incorrect time measured from bandwidth test: "
                        "start_min = %lf, end_max = %lf\n",
                         start_time_min, end_time_max);
        }
        pe_bw_sum = bw;
    } else {
        if (nPEs >= 2) {
            shmem_double_sum_reduce(SHMEM_TEAM_WORLD, &pe_bw_sum, &bw, nred_elements);
        } else if (nPEs == 1) {
            pe_bw_sum = bw;
        }
    }

    /* aggregate bw since bw op pairs are communicating simultaneously */
    if(metric_info->my_node == start_pe) {
        pe_bw_avg = pe_bw_sum;
        pe_mr_avg = pe_bw_avg / (len / 1.0e6);
        print_data_results(pe_bw_avg, pe_mr_avg, metric_info, len, total_t);
    }
}

static int validate_atomics(perf_metrics_t * const m_info) {
    int snode = streaming_node(m_info);
    int * my_buf = (int *)m_info->dest;
    bw_type tbw = m_info->b_type;
    int expected_val = 0, errors = 0;
    unsigned int ppe_exp_val = ((m_info->trials + m_info->warmup) * m_info->window_size
                                * ATOMICS_N_DTs * ATOMICS_N_OPs) + m_info->my_node;

    if (m_info->cstyle == COMM_INCAST) {
        if (tbw == BI_DIR)
            printf("WARNING: This use-case is not currently well defined\n");

        if (m_info->my_node == 0) {
            expected_val = ppe_exp_val * m_info->num_pes;
        } else
            expected_val = m_info->my_node;
    } else {
        assert(m_info->cstyle == COMM_PAIRWISE);
        expected_val = ppe_exp_val;
    }

    if ((!snode && tbw == UNI_DIR) || tbw == BI_DIR) {
        if(my_buf[0] != expected_val) {
            printf("Validation error for PE %d: %d != %d \n", m_info->my_node, my_buf[0],
                    expected_val);
            errors++;
        }
    }

    return errors;
}

/**************************************************************/
/*                   Bi-Directional BW                        */
/**************************************************************/

/*have two symmetric char array metric_info->src/dest of max_len to
 * use for calculation initalized with my_node number
 * NOTE: post function validation assumptions, data isn't flushed pre/post */
extern void bi_dir_bw(int len, perf_metrics_t *metric_info);

static inline
void bi_dir_bw_test_and_output(perf_metrics_t * const metric_info) {
    int partner_pe = partner_node(metric_info);

    if(metric_info->my_node == 0) {
        if (metric_info->opstyle == STYLE_ATOMIC) {
            metric_info->max_len = sizeof(unsigned long long);
            print_atomic_header(metric_info);
        }
        else
            print_bw_header(metric_info);
    }

    if (metric_info->opstyle == STYLE_ATOMIC) {
        bi_dir_bw(0 /* ignored */, metric_info);
    } else {
        unsigned long int len = 0;
        for (len = metric_info->start_len; len <= metric_info->max_len;
             len *= metric_info->size_inc) {

            large_message_metric_chg(metric_info, len);

            bi_dir_bw(len, metric_info);
        }
    }

    shmem_barrier_all();

    if (metric_info->validate) {
        int errors = -1;
        if (metric_info->opstyle != STYLE_ATOMIC) {
            errors = validate_recv(metric_info->dest, metric_info->max_len, partner_pe);
        } else {
            errors = validate_atomics(metric_info);
        }
        if (errors >= 0)
            printf("Validation complete (%d errors)\n", errors);
    }
}

/**************************************************************/
/*                   UNI-Directional BW                       */
/**************************************************************/

/* have one symmetric char array metric_info->buf of max_len to use for
 * calculation initalized with my_node number
 * NOTE: post function validation assumptions, data isn't flushed pre/post */
extern void uni_dir_bw(int len, perf_metrics_t *metric_info);

static inline
void uni_dir_bw_test_and_output(perf_metrics_t * const metric_info) {
    int partner_pe = partner_node(metric_info);

    if(metric_info->my_node == 0) {
        if (metric_info->opstyle == STYLE_ATOMIC) {
            metric_info->max_len = sizeof(unsigned long long);
            print_atomic_header(metric_info);
        }
        else
            print_bw_header(metric_info);
    }

    if (metric_info->opstyle == STYLE_ATOMIC) {
        uni_dir_bw(0 /* ignored */, metric_info);
    } else {
        unsigned long int len = 0;
        for (len = metric_info->start_len; len <= metric_info->max_len;
             len *= metric_info->size_inc) {

            large_message_metric_chg(metric_info, len);

            uni_dir_bw(len, metric_info);
        }
    }

    shmem_barrier_all();

    if (metric_info->validate) {
        int errors = -1;
        if ((streaming_node(metric_info) && metric_info->opstyle == STYLE_GET) ||
            (target_node(metric_info) && metric_info->opstyle == STYLE_PUT)) {
            errors = validate_recv(metric_info->dest, metric_info->max_len, partner_pe);
        } else if (metric_info->opstyle == STYLE_ATOMIC) {
            errors = validate_atomics(metric_info);
        }
        if (errors >= 0)
            printf("Validation complete (%d errors)\n", errors);
    }
}

/**************************************************************/
/*                   INIT and teardown of resources           */
/**************************************************************/

/* create and init (with my_PE_num) two symmetric arrays on the heap */
static inline
int bw_init_data_stream(perf_metrics_t * const metric_info,
                        int argc, char *argv[]) {

    init_metrics(metric_info);
    int ret = command_line_arg_check(argc, argv, metric_info);

#ifndef VERSION_1_0
#if defined(ENABLE_THREADS)
    int tl;
    shmem_init_thread(metric_info->thread_safety, &tl);
    if(tl < metric_info->thread_safety) {
        fprintf(stderr,"Could not initialize with requested thread "
                "level %d: got %d\n", metric_info->thread_safety, tl);
        return -1;
    }
#else
    shmem_init();
#endif
#else
    start_pes(0);
#endif

    update_metrics(metric_info);

    if (ret) {
        if (metric_info->my_node == 0) {
            print_usage(ret);
        }
        return -1;
    } else {
        if (metric_info->num_pes < 2) {
            fprintf(stderr, "This test requires at least two processes.\n");
            print_usage(1);
            return -1;
        }
    }

    if (error_checking_init_target_usage(metric_info) == -1)
        return -1;
#if defined(ENABLE_THREADS)
    thread_safety_validation_check(metric_info);
#endif

    if(only_even_PEs_check(metric_info->my_node, metric_info->num_pes) != 0) {
        return -1;
    }

    metric_info->src = aligned_buffer_alloc(metric_info->max_len * metric_info->nthreads);
    init_array(metric_info->src, metric_info->max_len * metric_info->nthreads, metric_info->my_node);

    metric_info->dest = aligned_buffer_alloc(metric_info->max_len * metric_info->nthreads);
    init_array(metric_info->dest, metric_info->max_len * metric_info->nthreads, metric_info->my_node);

    return 0;
}


static inline
int bi_dir_init(perf_metrics_t * const metric_info, int argc,
                char *argv[], op_style opstyle) {
    int ret = bw_init_data_stream(metric_info, argc, argv);
    if (ret == 0) {
        metric_info->opstyle = opstyle;
        update_bw_type(metric_info, BI_DIR);
        return 0;
    } else
        return ret;
}

static inline
int uni_dir_init(perf_metrics_t * const metric_info, int argc,
                 char *argv[], op_style opstyle) {
    int ret = bw_init_data_stream(metric_info, argc, argv);
    if (ret == 0) {
        /* uni-dir validate needs to know if its a put or get */
        metric_info->opstyle = opstyle;
        update_bw_type(metric_info, UNI_DIR);
        return 0;
    } else
        return ret;
}

static inline
void bw_data_free(const perf_metrics_t * const metric_info) {
    shmem_barrier_all();

    aligned_buffer_free(metric_info->src);
    aligned_buffer_free(metric_info->dest);
}

static inline
void bw_finalize(void) {
#ifndef VERSION_1_0
    shmem_finalize();
#endif
}

static inline
void bi_dir_bw_main(int argc, char *argv[], op_style opstyle) {

    perf_metrics_t metric_info;

    int ret = bi_dir_init(&metric_info, argc, argv, opstyle);

    if (ret == 0) {
        bi_dir_bw_test_and_output(&metric_info);
        bw_data_free(&metric_info);
    }

    bw_finalize();
}

static inline
void uni_dir_bw_main(int argc, char *argv[], op_style opstyle) {

    perf_metrics_t metric_info;

    int ret = uni_dir_init(&metric_info, argc, argv, opstyle);

    if (ret == 0) {
        uni_dir_bw_test_and_output(&metric_info);
        bw_data_free(&metric_info);
    }

    bw_finalize();
}
