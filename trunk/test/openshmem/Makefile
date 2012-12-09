 #
 #
 # Copyright (c) 2011, 2012 
 #   University of Houston System and Oak Ridge National Laboratory.
 # 
 # All rights reserved.
 # 
 # Redistribution and use in source and binary forms, with or without
 # modification, are permitted provided that the following conditions
 # are met:
 # 
 # o Redistributions of source code must retain the above copyright notice,
 #   this list of conditions and the following disclaimer.
 # 
 # o Redistributions in binary form must reproduce the above copyright
 #   notice, this list of conditions and the following disclaimer in the
 #   documentation and/or other materials provided with the distribution.
 # 
 # o Neither the name of the University of Houston System, Oak Ridge
 #   National Laboratory nor the names of its contributors may be used to
 #   endorse or promote products derived from this software without specific
 #   prior written permission.
 # 
 # THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 # "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 # LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 # A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 # HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 # SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 # TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 # PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 # LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 # NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 # SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 #


SHELL = /bin/sh

help: 
	@echo
	@echo "Please use one of the following targets:"
	@echo
	@echo "    * For Examples, C feature tests, Fortran feature tests and Microbenchmarks : make compile, make run, make clean"
	@echo "    * For Examples : make examples, make examples-run"
	@echo "    * For C feature tests : make C_feature_tests, make C_feature_tests-run "
	@echo "    * For  Fortran feature tests : make F_feature_tests, make F_feature_tests-run"
	@echo "    * For  Microbenchmarks : make micro_bench, make micro_bench-run"
	@echo
	@echo "    clean: removes object files"
	@echo

.PHONY : examples

compile: C_feature_tests F_feature_tests micro_bench examples 

run: examples-run C_feature_tests-run F_feature_tests-run micro_bench-run

C_feature_tests: 
	cd feature_tests/C; $(MAKE) all

examples:
	cd examples; $(MAKE) all

examples-run:
	cd examples; $(MAKE) run  


C_feature_tests-run:
	cd feature_tests/C; $(MAKE) run

F_feature_tests: 
	cd feature_tests/Fortran; $(MAKE) all

F_feature_tests-run:
	cd feature_tests/Fortran; $(MAKE) run

micro_bench:
	cd performance_tests/micro_benchmarks; $(MAKE) 

micro_bench-run:
	cd performance_tests/micro_benchmarks; $(MAKE) run 

clean:
	cd examples; $(MAKE) $@
	cd feature_tests/C; $(MAKE) $@
	cd feature_tests/Fortran; $(MAKE) $@
	cd performance_tests/micro_benchmarks; $(MAKE) $@
	rm -f *~ */*~ *.x *.o

