#! /bin/bash

test -d ./config || mkdir ./config

test -f modules/test-sos/configure.ac || { cd modules/test-sos; git submodule update --init; cd ../..; }

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
