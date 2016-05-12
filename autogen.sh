#! /bin/sh

test -d ./config || mkdir ./config
test -d ./shmem_pmi/config || mkdir ./shmem_pmi/config

autoreconf -vif
