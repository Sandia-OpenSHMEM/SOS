#! /bin/bash

test -d ./config || mkdir ./config

test -f modules/tests-sos/configure.ac || { echo "ERROR: The test directory is empty. Please run the following command to download the SOS tests:"; echo "git submodule update --init"; exit 1; }

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
