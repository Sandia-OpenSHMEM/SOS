/*
*
*  Copyright (c) 2015 Intel Corporation. All rights reserved.
*  This software is available to you under the BSD license. For
*  license information, see the LICENSE file in the top level directory.
*
*/

#include <common.h>

#define MAX_MSG_SIZE (1<<22)
#define START_LEN 1

#define INC 2
#define TRIALS 500
#define WINDOW_SIZE 64
#define WARMUP 50

#define TRIALS_LARGE  100
#define WINDOW_SIZE_LARGE 64
#define WARMUP_LARGE  10
#define LARGE_MESSAGE_SIZE  8192

typedef enum {
    UNI_DIR,
    BI_DIR
} bw_type;

typedef enum {
    FIRST_HALF,
    SECOND_HALF
} red_PE_set;

typedef enum {
    B,
    KB,
    MB
} bw_units;

typedef struct perf_metrics {
    unsigned int start_len, max_len;
    unsigned int size_inc, trials;
    unsigned int window_size, warmup;
    int validate;
    int my_node, num_pes;
    bw_units unit;
    char *src, *dest;
    const char *bw_type;
    bw_type type;
} perf_metrics_t;

long red_psync[_SHMEM_REDUCE_SYNC_SIZE];

/*default settings if no input is provided */
void static data_init(perf_metrics_t * data) {
    data->start_len = START_LEN;
    data->max_len = MAX_MSG_SIZE;
    data->size_inc = INC;
    data->trials = TRIALS;
    data->window_size = WINDOW_SIZE; /*back-to-back msg stream*/
    data->warmup = WARMUP; /*number of initial iterations to skip*/
    data->unit = MB;
    data->validate = false;
    data->my_node = shmem_my_pe();
    data->num_pes = shmem_n_pes();
    data->src = NULL;
    data->dest = NULL;
}

void static bi_dir_data_init(perf_metrics_t * data) {
    data->bw_type = "Bi-directional Bandwidth";
    data->type = BI_DIR;
}

void static uni_dir_data_init(perf_metrics_t * data) {
    data->bw_type = "Uni-directional Bandwidth";
    data->type = UNI_DIR;
}


int static inline partner_node(perf_metrics_t my_info)
{
    int pairs = my_info.num_pes / 2;

    return (my_info.my_node < pairs ? my_info.my_node + pairs :
                    my_info.my_node - pairs);
}

int static inline streaming_node(perf_metrics_t my_info)
{
    return (my_info.my_node < (my_info.num_pes / 2));

}

/* put/get bw use opposite streaming/validate nodes */
red_PE_set static inline validation_set(perf_metrics_t my_info)
{
    return (streaming_node(my_info) ? FIRST_HALF : SECOND_HALF);

}

/**************************************************************/
/*                   Input Checking                           */
/**************************************************************/

void static command_line_arg_check(int argc, char *argv[],
                                   perf_metrics_t *metric_info) {
    int ch, error = false;
    extern char *optarg;

    /* check command line args */
    while ((ch = getopt(argc, argv, "e:s:n:kbv")) != EOF) {
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
        case 'k':
            metric_info->unit = KB;
            break;
        case 'b':
            metric_info->unit = B;
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
                    ": lengths should be a power of two \n " \
                    "[-n trials (must be greater than 20)] "\
                    "[-k (kilobytes/second)] [-b (bytes/second)] "\
                    "[-v (validate data stream)] \n");
        }
        shmem_finalize();
        exit (-1);
    }
}

void static inline only_even_PEs_check(int my_node, int num_pes) {
    if (num_pes % 2 != 0) {
        if (my_node == 0) {
            fprintf(stderr, "can only use an even number of nodes\n");
        }
        shmem_finalize();
        exit(77);
    }
}

/**************************************************************/
/*                   Result Printing and Calc                 */
/**************************************************************/

void static print_data_results(double bw, double mr, perf_metrics_t data, int len) {
    printf("%9d       ", len);

    if(data.unit == KB) {
        bw = bw * 1e3;
    } else if(data.unit == B){
        bw = bw * 1e6;
    }

    printf("%10.2f                          %10.2f\n", bw, mr);
}

void static print_results_header(perf_metrics_t metric_info) {
        printf("\nResults for %d PEs %d trials with window size %d "\
            "max message size %d with multiple of %d increments\n",
            metric_info.num_pes, metric_info.trials, metric_info.window_size,
            metric_info.max_len, metric_info.size_inc);

        printf("\nLength           %s           "\
            "Message Rate\n", metric_info.bw_type);

    printf("in bytes         ");
    if (metric_info.unit == MB) {
        printf("in megabytes per second");
    } else if (metric_info.unit == KB) {
        printf("in kilobytes per second");
    } else {
        printf("in bytes per second");
    }

    printf("         in messages/seconds\n");
}

/* reduction to collect performance results from PE set
    then start_pe will print results --- assumes num_pes is even */
void static inline PE_set_used_adjustments(int *stride, int *start_pe, perf_metrics_t my_info)
{
    red_PE_set PE_set = validation_set(my_info);

    if(PE_set == FIRST_HALF) {
        *start_pe = 0;
    }
    else {
        assert(PE_set == SECOND_HALF);
        *start_pe = my_info.num_pes / 2;
    }

    *stride = 0; /* back to back PEs */
}


void static inline calc_and_print_results(double total_t, int len,
                            perf_metrics_t metric_info)
{
    int half_of_nPEs = metric_info.num_pes/2;
    int stride = 0, start_pe = 0;
    static double pe_bw_sum, bw = 0.0; /*must be symmetric for reduction*/
    double pe_bw_avg = 0.0, pe_mr_avg = 0.0;
    int nred_elements = 1;
    static double pwrk[_SHMEM_REDUCE_MIN_WRKDATA_SIZE];

    PE_set_used_adjustments(&stride, &start_pe, metric_info);

    if (total_t > 0 ) {
        bw = (len / 1e6 * metric_info.window_size * metric_info.trials) /
                (total_t / 1e6);
    }

    /* 2x as many messages/bytes at once for bi-directional */
    if(metric_info.type == BI_DIR)
        bw *= 2;

    /* base case: will be overwritten by collective if num_pes > 2 */
    pe_bw_sum = bw;

    if(metric_info.num_pes > 2)
        shmem_double_sum_to_all(&pe_bw_sum, &bw, nred_elements, start_pe,
                                stride, half_of_nPEs, pwrk,
                                red_psync);

    /* aggregate bw since bw op pairs are communicating simultaneously */
    if(metric_info.my_node == start_pe) {
        pe_bw_avg = pe_bw_sum;
        pe_mr_avg = pe_bw_avg / (len / 1e6);
        print_data_results(pe_bw_avg, pe_mr_avg, metric_info, len);
    }
}

void static inline large_message_metric_chg(perf_metrics_t *metric_info, int len) {
    if(len > LARGE_MESSAGE_SIZE) {
        metric_info->window_size = WINDOW_SIZE_LARGE;
        metric_info->trials = TRIALS_LARGE;
        metric_info->warmup = WARMUP_LARGE;
    }
}

/**************************************************************/
/*                   Bi-Directional BW                        */
/**************************************************************/

/*have two symmetric char array metric_info->src/dest of max_len to
 * use for calculation initalized with my_node number
 * NOTE: post function validation assumptions, data isn't flushed pre/post */
extern void bi_dir_bw(int len, perf_metrics_t *metric_info);

void static inline bi_dir_bw_test_and_output(perf_metrics_t metric_info) {
    int len = 0, partner_pe = partner_node(metric_info);

    if (metric_info.my_node == 0)
        print_results_header(metric_info);

    for (len = metric_info.start_len; len <= metric_info.max_len;
        len *= metric_info.size_inc) {

        large_message_metric_chg(&metric_info, len);

        bi_dir_bw(len, &metric_info);
    }

    shmem_barrier_all();

    if(metric_info.validate) {
            validate_recv(metric_info.dest, metric_info.max_len, partner_pe);
    }
}

/**************************************************************/
/*                   UNI-Directional BW                       */
/**************************************************************/

/*have one symmetric char array metric_info->buf of max_len to use for
 * calculation initalized with my_node number
 * NOTE: post function validation assumptions, data isn't flushed pre/post */
extern void uni_dir_bw(int len, perf_metrics_t *metric_info);

void static inline uni_dir_bw_test_and_output(perf_metrics_t metric_info) {
    int len = 0, partner_pe = partner_node(metric_info);

    if (metric_info.my_node == 0)
        print_results_header(metric_info);

    for (len = metric_info.start_len; len <= metric_info.max_len;
        len *= metric_info.size_inc) {

        large_message_metric_chg(&metric_info, len);

        uni_dir_bw(len, &metric_info);
    }

    shmem_barrier_all();

    if(streaming_node(metric_info) && metric_info.validate)
        validate_recv(metric_info.dest, metric_info.max_len, partner_pe);

}

/**************************************************************/
/*                   INIT and teardown of resources           */
/**************************************************************/

/*create and init (with my_PE_num) two symmetric arrays on the heap */
void static inline bw_init_data_stream(perf_metrics_t *metric_info,
                                            int argc, char *argv[]) {

    int i = 0;

    data_init(metric_info);

    only_even_PEs_check(metric_info->my_node, metric_info->num_pes);

    for(i = 0; i < _SHMEM_REDUCE_MIN_WRKDATA_SIZE; i++)
        red_psync[i] = _SHMEM_SYNC_VALUE;

    command_line_arg_check(argc, argv, metric_info);

    metric_info->src = aligned_buffer_alloc(metric_info->max_len);
    init_array(metric_info->src, metric_info->max_len, metric_info->my_node);

    metric_info->dest = aligned_buffer_alloc(metric_info->max_len);
    init_array(metric_info->dest, metric_info->max_len, metric_info->my_node);
}


void static inline bi_dir_init(perf_metrics_t *metric_info, int argc,
                                char *argv[]) {
    bw_init_data_stream(metric_info, argc, argv);

    bi_dir_data_init(metric_info);

}

void static inline uni_dir_init(perf_metrics_t *metric_info, int argc,
                                char *argv[]) {
    bw_init_data_stream(metric_info, argc, argv);

    uni_dir_data_init(metric_info);
}

void static inline bw_data_free(perf_metrics_t *metric_info) {
    shmem_barrier_all();

    aligned_buffer_free(metric_info->src);
    aligned_buffer_free(metric_info->dest);
}

void static inline bi_dir_bw_main(int argc, char *argv[]) {

    perf_metrics_t metric_info;

    bi_dir_init(&metric_info, argc, argv);

    bi_dir_bw_test_and_output(metric_info);

    bw_data_free(&metric_info);

} /*main() */

void static inline uni_dir_bw_main(int argc, char *argv[]) {

    perf_metrics_t metric_info;

    uni_dir_init(&metric_info, argc, argv);

    uni_dir_bw_test_and_output(metric_info);

    bw_data_free(&metric_info);

} /*main() */
