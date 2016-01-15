#! /bin/sh

set -x
test -d ./config || mkdir ./config
aclocal -I config
case `uname` in
    Darwin*) glibtoolize --force --copy ;;
    *)        libtoolize --force --copy ;;
esac
autoheader
automake --foreign --add-missing --copy
autoconf

cd shmem_pmi
./autogen.sh
