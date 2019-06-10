#include <stdio.h>
#include <stdlib.h>
#include <shmem.h>
#include <shmemx.h>

long recv_a_value(unsigned seed, int npes);
unsigned char is_valid(long value, int npes);

/* As if we receive some value from external source */
long recv_a_value(unsigned seed, int npes) {
  srand(seed);
  return rand() % npes;
}

/* Validate the value we recieved */
unsigned char is_valid(long value, int npes) {
  if (value == (npes-1))
    return 0;
  return 1;
}

int main(void)
{

  shmem_init();
  int me = shmem_my_pe();
  int npes = shmem_n_pes();
  size_t num = 32;

  long *values = shmem_malloc(num * sizeof(int));
  long *sums = shmem_malloc(num * sizeof(int));

  unsigned char *valid_me = shmem_malloc(num * sizeof(int));
  unsigned char *valid_all = shmem_malloc(num * sizeof(int));

  values[0] = recv_a_value((unsigned)me, npes);
  valid_me[0] = is_valid(values[0], npes);

  for (int i=1; i < num; i++) {
    values[i] = recv_a_value((unsigned)values[i-1], npes);
    valid_me[i] = is_valid(values[i], npes);
  }

  /* Wait for all PEs to initialize reductions arrays */
  shmemx_sync(SHMEMX_TEAM_WORLD);

//#if __STDC_VERSION__ >= 201112
//  /* C11 generic interface */
//  shmemx_and_reduce(SHMEMX_TEAM_WORLD, valid_all, valid_me, num);
//  shmemx_sum_reduce(SHMEMX_TEAM_WORLD, sums, values, num);
//#else
  /* C/C++ interface without generic support */
  shmemx_uchar_and_reduce(SHMEMX_TEAM_WORLD, valid_all, valid_me, num);
  shmemx_long_sum_reduce(SHMEMX_TEAM_WORLD, sums, values, num);
//#endif

  for (int i=0; i < num; i++) {
    if (valid_all[i]) {
      printf ("[%d] = %ld\n", i, sums[i]);
    }
    else {
      printf ("[%d] = invalid on one or more pe\n", i);
    }
  }
}
  
