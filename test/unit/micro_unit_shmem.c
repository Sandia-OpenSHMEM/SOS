/*
 *  Copyright (c) 2015 Intel Corporation. All rights reserved.
 *  This software is available to you under the BSD license below:
 *
 * *	Redistribution and use in source and binary forms, with or
 *	without modification, are permitted provided that the following
 *	conditions are met:
 *
 *	- Redistributions of source code must retain the above
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


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>
#include <shmem.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>

typedef enum {
	NUM_WRITE = 5,
	NUM_READ =  8,
	NUM_SYNC = 3
} max_ops;

int target[NUM_WRITE];
int source[NUM_READ];
int psync[NUM_SYNC];

int verbose;
int debug;


static inline void wait(int *wait_var, int iterations, int pe)
{

	if (debug)
		printf("PE %d waiting...%d\n", pe, *wait_var);

	shmem_int_wait_until(wait_var, SHMEM_CMP_EQ, iterations);

	if (debug)
		printf("PE %d wait_until passed\n", pe);

}

static inline void pre_op_check(const char *op,
		int check_var, int iterations, int pe)
{
	if (verbose)
		printf("SHMEM %s, performing %d iterations\n",
				op, iterations);

	if (debug)
		printf("BEFORE operation PE %d target = %d\n",
				pe, check_var);
}

static inline void post_op_check(const char *op,
		int check_var, int iterations, int pe)
{

	if (check_var != iterations) {
		fprintf(stderr, "%s ERR: PE %d source = %d != %d\n",
				op, pe, check_var, iterations);
		exit(EXIT_FAILURE);
	}
}

static inline void putfence(int me, int iterations, int T)
{
	int i;

	if (me == 1)
		pre_op_check("int_p", target[T], iterations, 1);

	if (me == 0) {
		for (i = 1; i < iterations; i++) {
			shmem_int_p(&target[T], i, 1);
			shmem_fence();

	}
		shmem_int_p(&target[T], i, 1);

	} else
		wait(&target[T], iterations, 1);

	if (verbose)
		if (me == 0)
			printf("SHMEM int_p finished\n");

}


static inline void gettest(int me, int iterations, int T, int S, int P)
{

	int i;

	if (me == 0)
		source[S] = iterations;
	else
		source[S] = 0;

	target[T] = -1;

	if (me == 1) {
		pre_op_check("int_get", target[T], iterations, 1);

		for (i = 0; i < iterations; i++)
			target[T] = shmem_int_g(&source[S], 0);

		shmem_int_p(&psync[P], i, 0);

		post_op_check("get", target[T], iterations, 1);

	} else
		wait(&psync[P], iterations, 0);

	if (verbose) {
		if (me == 0)
			printf("SHMEM int_get finished\n");
	}
}

static inline void atomic_inc(int me, int iterations, int T)
{

	int i;

	if (me == 1)
		pre_op_check("int_inc", target[T], iterations, 1);

	if (me == 0) {
		for (i = 0; i < iterations; i++) {
			shmem_int_inc(&target[T], 1);
			shmem_fence();
		}
		shmem_int_inc(&target[T], 1);

		if (debug)
			printf("PE 0 done with operation\n");

	} else
		wait(&target[T], (iterations+1), 1);

	if (verbose) {
		if (me == 1)
			printf("SHMEM int_inc finished\n");
	}
}

static inline void atomic_add(int me, int iterations, int T)
{

	int i;

	if (me == 0)
		pre_op_check("int_add", target[T], iterations, 0);

	if (me == 1) {
		for (i = 0; i < iterations; i++) {
			shmem_int_add(&target[T], 1, 0);
			shmem_fence();
		}
		shmem_int_add(&target[T], 1, 0);

		if (debug)
			printf("PE 1 done with operation\n");

	} else
		wait(&target[T], (iterations+1), 0);

	if (verbose) {
		if (me == 1)
			printf("SHMEM int_add finished\n");
	}
}


static inline void swaptest(int me, int iterations, int T, int S, int P)
{

	int i;
	const int tswap = 5, sswap = 2, odd = 1, even = 0, is_even = 2;
	target[T] = tswap;
	source[S] = sswap;

	if (me == 0)
		pre_op_check("int_swap", source[S], iterations, 0);

	if (me == 0) {
		for (i = 0; i < iterations; i++)
			source[S] = shmem_int_swap(&target[T], source[S], 1);

		shmem_int_p(&psync[P], i, 1);

		if (debug)
			printf("AFTER flag PE 0 value of source is %d"
					" = 5?\n", source[S]);

		if (((iterations % is_even == odd) && (source[S] != tswap)) ||
			((iterations % is_even == even) &&
			 (source[S] != sswap))) {
			fprintf(stderr, "swap ERR: PE 0 source = %d\n",
					source[S]);
			exit(EXIT_FAILURE);
		}

	} else {
		wait(&psync[P], iterations, 1);

		if (((iterations % is_even == odd) && (target[T] != sswap)) ||
			((iterations % is_even == even) &&
			 (target[T] != tswap))) {
			fprintf(stderr, "swap ERR: PE 0 target = %d \\n",
					target[T]);
			exit(EXIT_FAILURE);
		}

	}

	if (verbose) {
		if (me == 0)
			printf("SHMEM int_swap finished\n");
	}
}

static inline void cswaptest(int me, int iterations, int T, int S, int P)
{

	int i;
	source[S] = -100;

	if (me == 1) {
		pre_op_check("int_cswap", source[S], iterations, 1);

		for (i = 0; i < iterations; i++)
			source[S] = shmem_int_cswap(&(target[T]), i, (i+1), 0);

		shmem_int_p(&psync[P], i, 0);

		post_op_check("cswap", source[S], (iterations-1), 1);

	} else {
		wait(&psync[P], iterations, 0);

		if (target[T] != iterations) {
			fprintf(stderr, "cswap ERR: PE 1 target = %d != %d\n",
					target[T], iterations);
			exit(EXIT_FAILURE);
		}
	}

	if (verbose) {
		if (me == 1)
			printf("SHMEM int_cswap finished\n");
	}
}

static inline void fetchatomic_add(int me, int iterations, int T, int S)
{

	int i;

	if (me == 1)
		pre_op_check("int_fadd", target[T], iterations, 1);

	if (me == 0) {
		if (debug) {
			printf("BEFORE flag PE 0 value of source is"
					" %d = 0?\n", source[S]);
		}

		for (i = 0; i < iterations; i++) {
			source[S] = shmem_int_fadd(&target[T], 1, 1);
			shmem_fence();
		}
		source[S] = shmem_int_fadd(&target[T], 1, 1);

		post_op_check("fadd", source[S], iterations, 0);

	} else
		wait(&target[T], (iterations+1), 1);

	if (verbose) {
		if (me == 0)
			printf("SHMEM int_fadd finished\n");
	}
}

static inline void fetchatomic_inc(int me, int iterations, int T, int S)
{

	int i;

	if (me == 0)
		pre_op_check("int_finc", target[T], iterations, 0);

	if (me == 1) {
		if (debug) {
			printf("BEFORE flag PE 1 value of source is %d\n",
					source[S]);
		}

		for (i = 0; i < iterations; i++) {
			source[S] = shmem_int_finc(&target[T], 0);
			shmem_fence();
		}

		post_op_check("finc", source[S], (iterations-1), 1);
	} else
		wait(&target[T], iterations, 0);

	if (verbose) {
		if (me == 1)
			printf("SHMEM int_finc finished\n");
	}

}

int main(int argc, char **argv)
{
	int        me, nproc;
	int        T = 0, S = 0, P = 0;
	const int  DEFAULT_ITR = 7;
	int	   iterations = DEFAULT_ITR;

	start_pes(0);

	me = _my_pe();
	nproc = _num_pes();

	if (argc == 2) {
		if (strncmp(argv[1], "v", 1) == 0) {
			verbose = 1;
		} else if ((strncmp(argv[1], "h", 1) == 0) ||
				(strncmp(argv[1], "-h", 2) == 0)) {
			if (me == 0) {
				fprintf(stderr, "input options:\n 1) single"
					" argument options: < v (verbose) | "
					"number of interations >\n");
				fprintf(stderr, " 2) two argument options:"
					" must provide  <iterations> and "
					"test choice <pf|gt|aa|ai|st|cs|fa|fi"
				" + (v (for verbose) | vd (verbose debug))>\n");
			}
				return 0;

		} else {
			if (!isdigit(argv[1][0])) {
				if (me == 0) {
					fprintf(stderr, "single argument "
						"error: options: < v (verbose) | "
						"number of interations >\n");
				}
				return 0;
			}
			iterations = atoi(argv[1]);
			assert(iterations > 0);
		}
	}

	if (nproc != 2) {
		if (me == 0) {
			fprintf(stderr, "This is a micro test and is only "
				"intended to run on exactly two processes you"
				" are using %d\n", nproc);
		}
		return 0;
	}

	if (argc == 3) {
		if (strncmp(&argv[2][2], "v", 1) == 0)
			verbose = 1;

		if (strncmp(&argv[2][3], "d", 1) == 0)
			debug = 1;

		if (strncmp(argv[2], "pf", 2) == 0) {
			putfence(me, iterations, T++);
		}  else if (strncmp(argv[2], "gt", 2) == 0) {
			gettest(me, iterations, T++, S++, P++);
		}  else if (strncmp(argv[2], "aa", 2) == 0) {
			atomic_add(me, iterations, T++);
		}  else if (strncmp(argv[2], "ai", 2) == 0) {
			atomic_inc(me, iterations, T++);
		}  else if (strncmp(argv[2], "st", 2) == 0) {
			swaptest(me, iterations, T++, S++, P++);
		}  else if (strncmp(argv[2], "cs", 2) == 0) {
			cswaptest(me, iterations, T++, S++, P++);
		}  else if (strncmp(argv[2], "fa", 2) == 0) {
			fetchatomic_add(me, iterations, T++, S++);
		}  else if (strncmp(argv[2], "fi", 2) == 0) {
			fetchatomic_inc(me, iterations, T++, S++);
		} else {
			if (me == 0) {
				fprintf(stderr, "two argument input error:"
				" must provide <iterations> and test choice"
				" <pf|gt|aa|ai|st|cs|fa|fi + (v (for verbose)"
				" | vd (verbose debug))>\n");
			}
			return 0;
		}
	} else {
		putfence(me, iterations,  T++);
		gettest(me, iterations, T++, S++, P++);
		atomic_add(me, iterations, T++);
		atomic_inc(me, iterations, T++);
		swaptest(me, iterations, T++, S++, P++);
		cswaptest(me, iterations, T++, S++, P++);
		fetchatomic_add(me, iterations, T++, S++);
		fetchatomic_inc(me, iterations, T++, S++);
	}

	if (verbose) {
		if (me == 1)
			printf("PE 1: PASS: %8d iterations\n", iterations);
		else
			printf("PE 0 Successful exit\n");
	}

	return 0;
}
