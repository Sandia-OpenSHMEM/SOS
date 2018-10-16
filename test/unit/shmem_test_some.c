#include <shmem.h>
#include <shmemx.h>
#include <stdlib.h>

#define N 100

int main(void)
{
  int total_sum = 0;

  shmem_init();
  int mype = shmem_my_pe();
  int npes = shmem_n_pes();

  int *my_data = malloc(N * sizeof(int));
  int *all_data = shmem_malloc(N * npes * sizeof(int));

  int *flags = shmem_calloc(npes, sizeof(int));
  size_t *indices = calloc(npes, sizeof(size_t));
  int *status = calloc(npes, sizeof(int));

  for (int i = 0; i < N; i++)
      my_data[i] = mype*N + i;

  for (int i = 0; i < npes; i++)
      shmem_int_put_nbi(&all_data[mype*N], my_data, N, i);

  shmem_fence();

  for (int i = 0; i < npes; i++)
      shmem_int_p(&flags[mype], 1, i);
  
  int ncompleted = 0;

  while (ncompleted < npes) {
      int ntested = shmemx_int_test_some(flags, npes, indices, status, SHMEM_CMP_NE, 0); 
      if (ntested > 0) {
          for (int i = 0; i < ntested; i++) {
              for (int j = 0; j < N; j++)
                  total_sum += all_data[indices[i]*N + j];
          }
          ncompleted += ntested;
      } else {
          /* Overlap some computation here */
      }
  }

  shmem_finalize();
  return 0;
}
