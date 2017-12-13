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
 *
 */

/* Each PE sends a message to every PE.  PEs wait for all messages to
 * arrive using shmem_test to poll the array. */

#include <stdio.h>
#include <shmem.h>

/* Wait for any entry in the given ivar array to match the wait criteria and
 * return the index of the entry that satisfied the test. */
static int wait_any(long *ivar, int count, int cmp, long value)
{
  int idx = 0;
  while (!shmem_long_test(&ivar[idx], cmp, value))
    idx = (idx + 1) % count;
  return idx;
}

int main(void)
{
  shmem_init();
  const int mype = shmem_my_pe();
  const int npes = shmem_n_pes();

  long *wait_vars = shmem_calloc(npes, sizeof(long));

  /* Put mype+1 to every PE */
  for (int i = 0; i < npes; i++)
      shmem_long_p(&wait_vars[mype], mype+1, i);

  int nrecv = 0, errors = 0;

  /* Wait for all messages to arrive */
  while (nrecv < npes) {
    int who = wait_any(wait_vars, npes, SHMEM_CMP_NE, 0);
    if (wait_vars[who] != who+1) {
        printf("%d: wait_vars[%d] = %ld, expected %d\n",
               mype, who, wait_vars[who], who+1);
        errors++;
    }
    wait_vars[who] = 0;
    nrecv++;
  }

  shmem_free(wait_vars);
  shmem_finalize();
  return errors;
}
