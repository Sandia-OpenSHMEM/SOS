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

#include <shmemx.h>
#include <common.h>
#ifdef ENABLE_OPENMP
#include <omp.h>
#endif

long red_psync[SHMEM_REDUCE_SYNC_SIZE];
long bar_psync[SHMEM_BARRIER_SYNC_SIZE];

static const char * dt_names [] = { "int", "long", "longlong" };

/*default settings if no input is provided */
static 
void init_metrics(perf_metrics_t *metric_info) {
    metric_info->t_type = BW;
    set_metric_defaults(metric_info);

    metric_info->unit = MB;
    metric_info->target_data = false;
    metric_info->cstyle = COMM_PAIRWISE;
    metric_info->bwstyle = STYLE_RMA;
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

static inline 
int streaming_node(perf_metrics_t my_info)
{
    if(my_info.cstyle == COMM_PAIRWISE) {
        return (my_info.my_node < my_info.szinitiator);
    } else {
        assert(my_info.cstyle == COMM_INCAST);
        return true;
    }
}

static inline 
int target_node(perf_metrics_t my_info)
{
    return (my_info.my_node >= my_info.midpt &&
        (my_info.my_node < (my_info.midpt + my_info.sztarget)));
}

/* put/get bw use opposite streaming/validate nodes */
static inline 
red_PE_set validation_set(perf_metrics_t my_info, int *nPEs)
{
    if(my_info.cstyle == COMM_PAIRWISE) {
        if(streaming_node(my_info)) {
            *nPEs = my_info.szinitiator;
            return FIRST_HALF;
        } else if(target_node(my_info)) {
            *nPEs = my_info.sztarget;
            return SECOND_HALF;
        } else {
            fprintf(stderr, "Warning: you are getting data from a node that "
                "wasn't a part of the perf set \n ");
	    return 0;
        }
    } else {
        assert(my_info.cstyle == COMM_INCAST);
        *nPEs = my_info.num_pes;
        return FULL_SET;
    }
}

/**************************************************************/
/*                   Input Checking                           */
/**************************************************************/

static 
int command_line_arg_check(int argc, char *argv[], perf_metrics_t *metric_info) {
    int ch, error = false;
    extern char *optarg;

    /* check command line args */
    while ((ch = getopt(argc, argv, "e:s:n:w:p:r:l:kbivtC:T:")) != EOF) {
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
        case 'p':
            metric_info->warmup = strtoul(optarg, (char **)NULL, 0);
            if(metric_info->warmup > (metric_info->trials/2)) {
                fprintf(stderr, "Error: warmup (%ld) must be <= trials/2 "
                        "(%ld)\n", metric_info->warmup, metric_info->trials/2);
                error = true;
            }
            break;
        case 'k':
            metric_info->unit = KB;
            break;
        case 'b':
            metric_info->unit = B;
            break;
        case 'v':
            metric_info->validate = true;
            if(metric_info->target_data) error = true;
            break;
        case 'w':
            metric_info->window_size = strtoul(optarg, (char **)NULL, 0);
            if(metric_info->target_data) error = true;
            break;
        case 't':
            metric_info->target_data = true;
            metric_info->window_size = 1;
            if(metric_info->validate) error = true;
            break;
        case 'r':
            metric_info->sztarget = strtoul(optarg, (char **)NULL, 0);
            break;
        case 'l':
            metric_info->szinitiator = strtoul(optarg, (char **)NULL, 0);
            break;
        case 'C':
            if (strcmp(optarg, "SINGLE") == 0) {
                metric_info->thread_safety = SHMEM_THREAD_SINGLE;
            } else if (strcmp(optarg, "FUNNELED") == 0) {
                metric_info->thread_safety = SHMEM_THREAD_FUNNELED;
            } else if (strcmp(optarg, "SERIALIZED") == 0) {
                metric_info->thread_safety = SHMEM_THREAD_SERIALIZED;
            } else if (strcmp(optarg, "MULTIPLE") == 0) {
                metric_info->thread_safety = SHMEM_THREAD_MULTIPLE;
            } else {
                fprintf(stderr, "Invalid threading level: \"%s\"\n", optarg);
                error = true;
            }
            break;
        case 'T':
            metric_info->nthreads = atoi(optarg);
            break;
        case 'i':
            metric_info->individual_report = 1;
            break;
        default:
            error = true;
            break;
        }
    }

    /* filling in 8/4KB chunks into array alloc'd to max_len */
    if(metric_info->target_data) {
        metric_info->start_len = TARGET_SZ_MIN;
        if((metric_info->max_len <
            ((metric_info->trials + metric_info->warmup) * TARGET_SZ_MIN)) ||
            (metric_info->max_len <
            ((metric_info->trials + metric_info->warmup) * TARGET_SZ_MAX))) {
                error = true;
            }
    }

    if (error) {
        if (metric_info->my_node == 0) {
            fprintf(stderr, "Usage: \n[-s start_length] [-e end_length] "
                    ": lengths should be a power of two \n"
                    "[-n trials (must be greater than 2*warmup (default: x => 100))] \n"
                    "[-p warm-up (see trials for value restriction)] \n"
                    "[-w window size - iterations between completion, cannot use with -t] \n"
                    "[-k (kilobytes/second)] [-b (bytes/second)] \n"
                    "[-v (validate data stream)] \n"
                    "[-i (turn on individual bandwidth reporting)] \n"
                    "[-t output data for target side (default is initiator,"
                    " only use with put_bw),\n cannot be used in conjunction "
                    "with validate, special sizes used, \ntrials"
                    " + warmup * sizes (8/4KB) <= max length \n"
                    "[-r number of nodes at target, use only with -t] \n"
                    "[-l number of nodes at initiator, use only with -t, "
                    "l/r cannot be used together] \n"
                    "[-C thread-safety-config: SINGLE, FUNNELED, SERIALIZED, or MULTIPLE] \n"
                    "[-T num-threads] \n");
        }
        return -1;
    }
    return 0;
}

/**************************************************************/
/*                   Result Printing and Calc                 */
/**************************************************************/

static 
void print_atomic_header(perf_metrics_t metric_info) {
    print_header(metric_info);
    printf("\n\nBandwidth test type:    %10s\n", metric_info.bw_type_str);

    if (metric_info.cstyle == COMM_INCAST) {
        printf("Communication style:        INCAST\n");
    } else {
        assert(metric_info.cstyle == COMM_PAIRWISE);
        printf("Communication style:      PAIRWISE\n");
    }

    printf("\nOperation%15sBandwidth%15sMessage Rate%15sLatency\n", 
            " ", " ", " ");

    if (metric_info.unit == MB) {
        printf("%19s in mbytes/sec"," ");
    } else if (metric_info.unit == KB) {
        printf("%19s in kbytes/sec", " ");
    } else {
        printf("%20s in bytes/sec", " ");
    }

    printf("%15s in Mops/sec%15s  in us\n", " ", " ");
}

static 
void print_bw_header(perf_metrics_t metric_info) {
    print_header(metric_info);
    printf("\n\nBandwidth test type:    %10s\n", metric_info.bw_type_str);

    printf("\nMessage Size%15sBandwidth%15sMessage Rate\n", " ", " ");

    printf("%4sin bytes", " ");
    if (metric_info.unit == MB) {
        printf("%11sin mbytes/sec", " ");
    } else if (metric_info.unit == KB) {
        printf("%11sin kbytes/sec", " ");
    } else {
        printf("%12sin bytes/sec", " ");
    }

    printf("%16sin msgs/sec\n", " ");
}

static 
void print_data_results(double bw, double mr, perf_metrics_t data,
                            int len, double total_t) {
    static int atomic_type_index = 0;

    if(data.target_data) {
        if(data.my_node < data.midpt) {
            printf("initiator:\n");
        } else  {
            printf("target:\n");
        }
    }

    if (data.bwstyle == STYLE_ATOMIC) {
        printf("%-10s", dt_names[atomic_type_index]);
        atomic_type_index = (atomic_type_index + 1) % ATOMICS_N_DTs;
    } else
        printf("%2s%10d", " ", len);

    if(data.unit == KB) {
        bw = bw * 1.0e3;
    } else if(data.unit == B) {
        bw = bw * 1.0e6;
    }

    if (data.bwstyle == STYLE_ATOMIC) {
        printf("%13s%10.2f%15s%12.2f%12s%10.2f\n", " ", bw, " ", 
                mr/1.0e6, " ", total_t/(data.trials * data.window_size));
    } else
        printf("%14s%10.2f%15s%12.2f\n", " ", bw, " ", mr);
}


/* reduction to collect performance results from PE set
    then start_pe will print results --- assumes num_pes is even */
static inline 
void PE_set_used_adjustments(int *nPEs, int *stride, int *start_pe,
                             perf_metrics_t my_info) {
    red_PE_set PE_set = validation_set(my_info, nPEs);

    if(PE_set == FIRST_HALF || PE_set == FULL_SET) {
        *start_pe = 0;
    }
    else {
        assert(PE_set == SECOND_HALF);
        *start_pe = my_info.midpt;
    }

    *stride = 0; /* back to back PEs */
}


static inline 
void calc_and_print_results(double end_t, double start_t, int len,
                            perf_metrics_t metric_info) {
    int stride = 0, start_pe = 0, nPEs = 0;
    static double pe_bw_sum, bw = 0.0; /*must be symmetric for reduction*/
    double pe_bw_avg = 0.0, pe_mr_avg = 0.0;
    int nred_elements = 1;
    static double pwrk[SHMEM_REDUCE_MIN_WRKDATA_SIZE];
    static double pe_time_start, pe_time_end, 
                  end_time_max = 0.0, start_time_min = 0.0;
    double total_t = 0.0, total_t_max = 0.0;
    int multiplier = 1;

    PE_set_used_adjustments(&nPEs, &stride, &start_pe, metric_info);

    /* 2x as many messages at once for bi-directional */
    if(metric_info.b_type == BI_DIR)
        multiplier = 2;

    if (end_t > 0 && start_t > 0 && (end_t - start_t) > 0) {
        total_t = end_t - start_t;
#ifdef ENABLE_OPENMP
        bw = ((double) len * (double) multiplier / 1.0e6 * 
             metric_info.window_size * metric_info.trials *
             (double) metric_info.nthreads) / (total_t / 1.0e6);
#else
        bw = ((double) len * (double) multiplier / 1.0e6 * 
             metric_info.window_size * metric_info.trials) /
             (total_t / 1.0e6);
#endif
    } else {
        fprintf(stderr, "Incorrect time measured from bandwidth test: "
                        "start = %lf, end = %lf\n", start_t, end_t);
    }

    /* base case: will be overwritten by collective if num_pes > 2 */
    pe_bw_sum = bw;

    if (metric_info.individual_report == 1) {
        printf("Individual bandwith for PE %6d is %10.2f\n", 
                metric_info.my_node, pe_bw_sum);
    }
    
    pe_time_start = start_t;
    pe_time_end = end_t;
    shmem_barrier(start_pe, stride, nPEs, bar_psync);
    if (nPEs >= 2) {
        shmem_double_min_to_all(&start_time_min, &pe_time_start, 
                                nred_elements, start_pe, stride, 
                                nPEs, pwrk, red_psync);
        shmem_barrier(start_pe, stride, nPEs, bar_psync);
        shmem_double_max_to_all(&end_time_max, &pe_time_end, 
                                nred_elements, start_pe, stride, 
                                nPEs, pwrk, red_psync);
    } else if (nPEs == 1) {
        start_time_min = pe_time_start;
        end_time_max = pe_time_end;
    }

    /* calculating bandwidth based on the highest time duration across all PEs */
    if (end_time_max > 0 && start_time_min > 0 && 
       (end_time_max - start_time_min) > 0) {

        total_t_max = (end_time_max - start_time_min);
#ifdef ENABLE_OPENMP
        bw = ((double) len * (double) multiplier * (double) metric_info.midpt / 
             1.0e6 * metric_info.window_size * metric_info.trials * 
             (double) metric_info.nthreads) / (total_t_max / 1.0e6);
#else
        bw = ((double) len * (double) multiplier * (double) metric_info.midpt / 
             1.0e6 * metric_info.window_size * metric_info.trials) / 
             (total_t_max / 1.0e6);
#endif
    } else {
        fprintf(stderr, "Incorrect time measured from bandwidth test: "
                        "start_min = %lf, end_max = %lf\n", 
                         start_time_min, end_time_max);
    } 

    pe_bw_sum = bw;

    /* aggregate bw since bw op pairs are communicating simultaneously */
    if(metric_info.my_node == start_pe) {
        pe_bw_avg = pe_bw_sum;
        pe_mr_avg = pe_bw_avg / (len / 1.0e6);
        print_data_results(pe_bw_avg, pe_mr_avg, metric_info, len, total_t);
    }
}

static void validate_atomics(perf_metrics_t m_info) {
    int snode = streaming_node(m_info);
    int * my_buf = (int *)m_info.dest;
    bw_type tbw = m_info.b_type;
    int expected_val = 0;
    unsigned int ppe_exp_val = ((m_info.trials + m_info.warmup) * m_info.window_size
                                * ATOMICS_N_DTs * ATOMICS_N_OPs) + m_info.my_node;

    if(m_info.cstyle == COMM_INCAST) {
        if(tbw == BI_DIR)
            printf("WARNING: This use-case is not currently well defined\n");

        if(m_info.my_node == 0) {
            expected_val = ppe_exp_val * m_info.num_pes;
        } else
            expected_val = m_info.my_node;
    } else {
        assert(m_info.cstyle == COMM_PAIRWISE);
        expected_val = ppe_exp_val;
    }

    if((!snode && tbw == UNI_DIR) || tbw == BI_DIR) {
        if(my_buf[0] != expected_val)
            printf("validation error for PE %d: %d != %d \n", m_info.my_node, my_buf[0],
                    expected_val);
    }
}

/**************************************************************/
/*                   Bi-Directional BW                        */
/**************************************************************/

/*have two symmetric char array metric_info->src/dest of max_len to
 * use for calculation initalized with my_node number
 * NOTE: post function validation assumptions, data isn't flushed pre/post */
extern void bi_dir_bw(int len, perf_metrics_t *metric_info);

static inline 
void bi_dir_bw_test_and_output(perf_metrics_t metric_info) {
    int partner_pe = partner_node(metric_info);
    unsigned long int len;

    if(metric_info.my_node == 0) {
        if (metric_info.bwstyle == STYLE_ATOMIC)
            print_atomic_header(metric_info);
        else
            print_bw_header(metric_info);
    }

    for (len = metric_info.start_len; len <= metric_info.max_len;
        len *= metric_info.size_inc) {

        large_message_metric_chg(&metric_info, len);

        bi_dir_bw(len, &metric_info);
    }

    shmem_barrier_all();

    if(metric_info.validate) {
        if(metric_info.bwstyle != STYLE_ATOMIC) {
            validate_recv(metric_info.dest, metric_info.max_len, partner_pe);
        } else {
            validate_atomics(metric_info);
        }
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
void uni_dir_bw_test_and_output(perf_metrics_t metric_info) {
    int partner_pe = partner_node(metric_info);
    unsigned long int len = 0;

    if(metric_info.my_node == 0) {
        if (metric_info.bwstyle == STYLE_ATOMIC)
            print_atomic_header(metric_info);
        else
            print_bw_header(metric_info);
    }

    for (len = metric_info.start_len; len <= metric_info.max_len;
        len *= metric_info.size_inc) {

        large_message_metric_chg(&metric_info, len);

        uni_dir_bw(len, &metric_info);
    }

    shmem_barrier_all();

    if(metric_info.validate) {
        if((streaming_node(metric_info) && metric_info.bwstyle == STYLE_GET) ||
            (target_node(metric_info) && metric_info.bwstyle == STYLE_PUT)) {
            validate_recv(metric_info.dest, metric_info.max_len, partner_pe);
        } else if(metric_info.bwstyle == STYLE_ATOMIC) {
            validate_atomics(metric_info);
        }
    }
}

/**************************************************************/
/*                   INIT and teardown of resources           */
/**************************************************************/

/* create and init (with my_PE_num) two symmetric arrays on the heap */
static inline 
int bw_init_data_stream(perf_metrics_t *metric_info,
                        int argc, char *argv[]) {

    int i = 0;
    init_metrics(metric_info);
    int ret = command_line_arg_check(argc, argv, metric_info);
    if (ret != 0) {
        return -1;
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
    metric_info->sztarget = metric_info->midpt;
    metric_info->szinitiator = metric_info->midpt;

    for(i = 0; i < SHMEM_REDUCE_SYNC_SIZE; i++)
        red_psync[i] = SHMEM_SYNC_VALUE;

    for(i = 0; i < SHMEM_BARRIER_SYNC_SIZE; i++)
        bar_psync[i] = SHMEM_SYNC_VALUE;

    if(only_even_PEs_check(metric_info->my_node, metric_info->num_pes) != 0) {
        return -2;
    }

    metric_info->src = aligned_buffer_alloc(metric_info->max_len);
    init_array(metric_info->src, metric_info->max_len, metric_info->my_node);

    metric_info->dest = aligned_buffer_alloc(metric_info->max_len);
    init_array(metric_info->dest, metric_info->max_len, metric_info->my_node);

    return 0;
}


static inline 
int bi_dir_init(perf_metrics_t *metric_info, int argc, char *argv[]) {
    int ret = bw_init_data_stream(metric_info, argc, argv);
    if (ret == 0) {
        update_bw_type(metric_info, BI_DIR);
        return 0;
    } else 
        return ret;
}

static inline 
int uni_dir_init(perf_metrics_t *metric_info, int argc,
                 char *argv[], bw_style bwstyl) {
    int ret = bw_init_data_stream(metric_info, argc, argv);
    if (ret == 0) {
        /* uni-dir validate needs to know if its a put or get */
        metric_info->bwstyle = bwstyl;
        update_bw_type(metric_info, UNI_DIR);
        return 0;
    } else 
        return ret;
}

static inline 
void bw_data_free(perf_metrics_t *metric_info) {
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
void bi_dir_bw_main(int argc, char *argv[]) {

    perf_metrics_t metric_info;

    int ret = bi_dir_init(&metric_info, argc, argv);

    if (ret == 0) {
        bi_dir_bw_test_and_output(metric_info);
        bw_data_free(&metric_info);
    }

    if (ret != -1)
        bw_finalize(); 
} 

static inline 
void uni_dir_bw_main(int argc, char *argv[], bw_style bwstyl) {

    perf_metrics_t metric_info;

    int ret = uni_dir_init(&metric_info, argc, argv, bwstyl);

    if (ret == 0) {
        uni_dir_bw_test_and_output(metric_info);
        bw_data_free(&metric_info);
    }

    if (ret != -1)
        bw_finalize();
} 
