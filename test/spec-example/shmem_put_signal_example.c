#include <shmem.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

int main(void) {
  int i, err_count = 0;

  shmem_init();

  size_t size = 2048;
  int mype = shmem_my_pe();
  int npes = shmem_n_pes();
  int pe = (mype + 1) % npes;
  uint64_t *message = malloc(size * sizeof(uint64_t));
  static uint64_t sig_addr = 0;

  for (i = 0; i < size; i++) {
    message[i] = mype;
  }

  uint64_t *data = shmem_calloc(size, sizeof(uint64_t));

  if (mype == 0) {
    shmem_put_signal(data, message, size, &sig_addr, 1, SHMEM_SIGNAL_SET, pe);
  }
  else {
    shmem_wait_until(&sig_addr, SHMEM_CMP_EQ, 1);
    shmem_put_signal(data, data, size, &sig_addr, 1, SHMEM_SIGNAL_SET, pe);
  }

  /* Verify data */
  for (i = 0; i < size; i++) {
    if (data[i] != 0) {
      printf("PE %d error, expected 0, received %"PRIu64"\n", mype, data[i]);
      shmem_global_exit(1);
    }
  }

  free(message);
  shmem_free(data);

  shmem_finalize();
  return 0;
}
