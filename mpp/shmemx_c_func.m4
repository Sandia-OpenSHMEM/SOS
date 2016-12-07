void SHPRE()shmemx_init_thread(int tl_requested, int *tl_provided);
double SHPRE()shmemx_wtime(void);
char* SHPRE()shmemx_nodename(void);

void SHPRE()shmemx_getmem_ct(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe);
void SHPRE()shmemx_putmem_ct(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe);
void SHPRE()shmemx_ct_create(shmemx_ct_t *ct);
void SHPRE()shmemx_ct_free(shmemx_ct_t *ct);
long SHPRE()shmemx_ct_get(shmemx_ct_t ct);
void SHPRE()shmemx_ct_set(shmemx_ct_t ct, long value);
void SHPRE()shmemx_ct_wait(shmemx_ct_t ct, long wait_for);
