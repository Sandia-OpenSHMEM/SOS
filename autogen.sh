#! /bin/bash

test -d ./config || mkdir ./config

if [ ! -f test/Makefile.am ] || [ ! -f test/apps/Makefile.am ] ||
   [ ! -f test/include/Makefile.am ] || [ ! -f test/performance/Makefile.am ] ||
   [ ! -f test/shmemx/Makefile.am ]  || [ ! -f test/spec-example/Makefile.am ] ||
   [ ! -f test/unit/Makefile.am ];
then
  test -f modules/tests-sos/autogen.sh || { echo "ERROR: The test directory is empty. Please run the following command to download the SOS tests:"; echo "git submodule update --init"; exit 1; }
fi

FILES=./man/*.1
echo -n "dist_man1_MANS =" > ./man/Makefile.am
for f in $FILES
do
  echo -n " $(basename $f)" >> ./man/Makefile.am
done

FILES=./man/*.3
echo -e -n "\ndist_man3_MANS =" >> ./man/Makefile.am
for f in $FILES
do
  echo -n " $(basename $f)" >> ./man/Makefile.am
done

echo -e "\n" >> ./man/Makefile.am

autoreconf -vif
