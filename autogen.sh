#! /bin/sh

set -x
test -d ./config || mkdir ./config
aclocal -I config
libtoolize --force --copy
autoheader
automake --foreign --add-missing --copy
autoconf

cd shmem_pmi
./autogen.sh
