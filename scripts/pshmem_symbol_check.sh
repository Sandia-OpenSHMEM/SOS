#!/bin/sh

# Simple script to check for missing profiling symbols in 
# SHMEM library. The readelf tool requires the library to 
# be a shared object.
#
# Example usage: pshmem_symbol_check.sh <path to SHMEM library>
#
# Exit if any command fails
set -e

if [ $# != 1 ] ; then
    echo "Usage: $0 SHMEM_LIBRARY"
    exit 1
fi

SHMEM_LIB=$1

readelf --syms --wide $SHMEM_LIB | grep 'shmem_' | awk '{print $8}' > symbols
cat symbols | sort | uniq -d > shmem_symbols

if grep -q -R "pshmem_" shmem_symbols
then
  grep -R "pshmem_" shmem_symbols > pshmem_symbols_uniq
  echo "Number of pshmem symbols found: `wc -l pshmem_symbols_uniq | awk '{print $1}'`"
else
  echo "No PSHMEM symbols found"
  exit 0
fi

sed -n '/^pshmem_/d' shmem_symbols

for i in $(cat ./pshmem_symbols_uniq)
do
  search_str=$(echo $i | cut -c 2-)
  sed "/${search_str}/d" -i ./shmem_symbols
done

rm symbols
rm pshmem_symbols_uniq

echo "APIs that dont have profiling symbols:"
echo "--------------------------------------"
cat shmem_symbols

rm shmem_symbols
