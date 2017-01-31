#!/bin/bash

#export CUSTOM_SHMEM_DIR="<SOS_INSTALL_DIRECTORY>"
#export CRAY_TESTS_DIR="<CRAY_TESTS_SRC_DIRECTORY"
export CC=oshcc
export CXX=oshCC
export FTN=oshfort
export LAUNCHER=oshrun
export CFLAGS="-DOPENSHMEM"
export FFLAGS="-DOPENSHMEM -DOPENSHMEM_FORT_SHORT_HEADER -fcray-pointer"
export NPES=4

cd $CRAY_TESTS_DIR
source configure.sh
make $TRAVIS_PAR_MAKE sma1
make $TRAVIS_PAR_MAKE sma2
source $CRAY_TESTS_DIR/sma1/sma1_run 2>&1 | tee cray-tests-sma1.log
source $CRAY_TESTS_DIR/sma2/sma2_run 2>&1 | tee cray-tests-sma2.log

# Increment by 1 when a set of test fails (max is 3 for C/C++/Fortran)
tests_failed=0

#Check for failures in the C/C++ tests
if grep "FAIL" cray-tests-sma1.log; then ((tests_failed+=1)); fi
if grep "FAIL" cray-tests-sma2.log; then ((tests_failed+=1)); fi
if [ ! $SOS_DISABLE_FORTRAN ]; then
    make $TRAVIS_PAR_MAKE smaf
    if [ $? -eq 0 ]; then
        source $CRAY_TESTS_DIR/smaf/smaf_run 2>&1 | tee cray-tests-smaf.log;
        # Check for failures in the Fortran tests
        if grep "FAIL" cray-tests-smaf.log; then ((tests_failed+=1)); fi
    fi;
fi

make clean

if [ $tests_failed -eq 0 ]; then
  echo $'\n********************************\n'
  echo    "  All Cray SHMEM tests passed!  "
  echo $'\n********************************\n'
else
  echo $'\n\nCray SHMEM tests failed... exiting with code: ' $tests_failed $'\n\n'
fi
exit $tests_failed
