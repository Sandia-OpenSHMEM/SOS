include ../Makefile.in

F90_TESTS        = $(wildcard *.f90)

F90_EXES         = $(F90_TESTS:.f90=.x)

EXES             = $(F90_EXES)

.PHONY: clean all default

all default:    $(EXES)

.SUFFIXES: .x

%.x:    %.f90
	$(SHMEM_F90) $(SHMEM_F90CFLAGS) $(SHMEM_F90LFLAGS) -o $@ $^

clean:
	rm -f $(EXES)

run: $(EXES)
	@../${TEST_RUNNER}
