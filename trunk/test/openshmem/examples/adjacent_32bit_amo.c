/*
 *
 * Copyright (c) 2011, 2012 
 *   University of Houston System and Oak Ridge National Laboratory.
 * 
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * o Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * 
 * o Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * 
 * o Neither the name of the University of Houston System, Oak Ridge
 *   National Laboratory nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

/* Program contributed by SGI on the OpenSHMEM mailing list*/

#include <shmem.h>
#include <stdio.h>
#include <stdlib.h>

const int tries = 1000000;

#ifdef TEST64BIT
typedef long locktype;
#else
typedef int locktype;
#endif
 

int
main()
{
    int tpe, other;
    long i;
    struct {
	locktype a;
	locktype b;
    } *twovars;  
    int numfail = 0;

    start_pes(0);
    tpe = 0;
    other = _num_pes() - 1;

    twovars = shmalloc(sizeof(*twovars));
    if (_my_pe() == 0) {
	printf("Element size: %ld bytes\n", sizeof(locktype));
	printf("Addresses: 1st element %p\n", &twovars->a);
	printf("           2nd element %p\n", &twovars->b);
	printf("Iterations: %d   target PE: %d   other active PE: %d\n",
		tries, tpe, other);
    }
    twovars->a = 0;
    twovars->b = 0;

    shmem_barrier_all();


    if (_my_pe() == 0) {
	// put two values alternately to the 1st 32 bit word
	long expect, check;

	for (i=0; i<tries; i++) {
	    expect =  2 + i%2;
	    if (sizeof(locktype) == sizeof(int)) {
	      shmem_int_p((void*)&twovars->a, expect, tpe);
	      check = shmem_int_g((void*)&twovars->a, tpe);
	    } else if (sizeof(locktype) == sizeof(long)) {
	      shmem_long_p((void*)&twovars->a, expect, tpe);
	      check = shmem_long_g((void*)&twovars->a, tpe);
	    }
	    if (check != expect) {
		printf("error: iter %ld get returned %ld expected %ld\n", i, check, expect);
		numfail++;
		if (numfail > 10) {
		    printf("FAIL\n");
		    abort();
		}
	    }
	}
	printf("PE %d done doing puts and gets\n",_my_pe());


    } else if (_my_pe() == other) {
	// keep on atomically incrementing the 2nd 32 bit word
	long oldval;

	for (i=0; i<tries; i++) {
	    if (sizeof(locktype) == sizeof(int)) {
	      oldval = shmem_int_finc((void*)&twovars->b, tpe);
	    } else if (sizeof(locktype) == sizeof(long)) {
	      oldval = shmem_long_finc((void*)&twovars->b, tpe);
	    }
	    if (oldval != i) {
		printf("error: iter %ld finc got %ld expect %ld\n", i, oldval, i);
		numfail++;
		if (numfail > 10) {
		    printf("FAIL\n");
		    abort();
		}
	    }
	}
	printf("PE %d done doing fincs\n",_my_pe());
    }
    shmem_barrier_all();
    if (numfail) {
        printf("FAIL\n");
    }
    shmem_barrier_all();
    if (_my_pe() == 0) {
        printf("test complete\n");
    }
    return 0;
}
