#! /bin/bash

test -d ./config || mkdir ./config

FILES=./man/*.1
printf "dist_man1_MANS = " > ./man/Makefile.am
for f in $FILES
do
  printf "%s " $(basename $f) >> ./man/Makefile.am
done

FILES=./man/*.3
printf "\ndist_man3_MANS = " >> ./man/Makefile.am
for f in $FILES
do
  printf "%s " $(basename $f) >> ./man/Makefile.am
done

autoreconf -vif
