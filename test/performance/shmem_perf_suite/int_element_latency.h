
void static inline
int_p_latency(perf_metrics_t data)
{
    double start = 0.0;
    double end = 0.0;
    int i = 0;

    if (data.my_node == 1) {
        printf("\nStream shmem_int_p results:\n");
        print_results_header();
    }

    /*puts to zero to match gets validation scheme*/
    if (data.my_node == 1) {

        for (i = 0; i < data.trials + data.warmup; i++) {
            if(i == data.warmup)
                start = perf_shmemx_wtime();

            shmem_int_p((int*) data.dest, data.my_node, 0);
            shmem_quiet();

        }
        end = perf_shmemx_wtime();

        calc_and_print_results(start, end, sizeof(int), data);
    }

    shmem_barrier_all();

    if((data.my_node == 0) && data.validate)
        validate_recv(data.dest, sizeof(int), partner_node(data.my_node));

} /* latency/bw for one-way trip */

void static inline
int_g_latency(perf_metrics_t data)
{
    double start = 0.0;
    double end = 0.0;
    int i = 0;
    int rtnd = -1;

    if (data.my_node == 0) {
        printf("\nStream shmem_int_g results:\n");
        print_results_header();
    }

    if (data.my_node == 0) {

        for (i = 0; i < data.trials + data.warmup; i++) {
            if(i == data.warmup)
                start = perf_shmemx_wtime();

            rtnd = shmem_int_g((int*) data.src, 1);
        }
        end = perf_shmemx_wtime();

        calc_and_print_results(start, end, sizeof(int), data);
    }

    shmem_barrier_all();

    if((data.my_node == 0) && data.validate)
        validate_recv((char*) &rtnd, sizeof(int), partner_node(data.my_node));
}
