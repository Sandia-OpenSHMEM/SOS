#!/bin/bash

# Simple script to generate an SOS build using the OFI transport and Hydra
# process manager.
#
# Example usage: build-for-testing.sh ./sos-build ./sos-install
#
# Configure options can be set for OFI, Hydra, and SOS through the
# OFI_CONFIG_OPTS, HYDRA_CONFIG_OPTS, and SOS_CONFIG_OPTS environment
# variables.

# Exit if any command fails
set -e

if [ -z "$SOS_VERSION" ] ; then
    SOS_VERSION="v1.5.1"
fi
if [ -z "$OFI_VERSION" ] ; then
    OFI_VERSION="v1.14.x"
fi

HYDRA_URL="http://www.mpich.org/static/downloads/3.2.1/hydra-3.2.1.tar.gz"

PAR_MAKE="-j 2"
CLONE_DEPTH="10"
SOS_DEFAULT_OPTS="--enable-pmi-simple --enable-error-checking"

if [ $# != 2 ] ; then
    echo "Usage: $0 BUILD-DIR INSTALL-DIR"
    exit 1
fi

BUILD_DIR=$1
INSTALL_DIR=$2

if [ ! -d $BUILD_DIR ] ; then
    echo "Can't access build directory '$BUILD_DIR'"
    exit 1
fi

if [ ! -d $INSTALL_DIR ] ; then
    echo "Can't access install directory '$INSTALL_DIR'"
    exit 1
fi

if [[ $INSTALL_DIR != "/"* ]] ; then
    pushd $INSTALL_DIR > /dev/null
    INSTALL_DIR=$PWD
    popd > /dev/null
fi

if [ -z "$SOS_CONFIG_OPTS" ] ; then
    SOS_CONFIG_OPTS=$SOS_DEFAULT_OPTS
fi

echo ========================================================================
echo = BUILDING LIBFABRIC
echo ========================================================================

cd $BUILD_DIR
git clone -b $OFI_VERSION --depth $CLONE_DEPTH https://github.com/ofiwg/libfabric.git libfabric
cd libfabric
./autogen.sh
./configure --prefix=$INSTALL_DIR $OFI_CONFIG_OPTS
make $PAR_MAKE
make install
cd ..

echo ========================================================================
echo = BUILDING HYDRA
echo ========================================================================

wget $HYDRA_URL
tar xvzf `basename $HYDRA_URL`
cd `basename $HYDRA_URL | sed -e 's/.tar.gz//'`
./configure --prefix=$INSTALL_DIR $HYDRA_CONFIG_OPTS
make $PAR_MAKE
make install
cd ..

echo ========================================================================
echo = BUILDING SANDIA OPENSHMEM
echo ========================================================================

git clone -b $SOS_VERSION --depth $CLONE_DEPTH --recurse-submodules https://github.com/Sandia-OpenSHMEM/SOS.git sos
cd sos
./autogen.sh
./configure --prefix=$INSTALL_DIR --with-ofi=$INSTALL_DIR $SOS_CONFIG_OPTS
make $PAR_MAKE
make install
cd ..

echo
echo Successfully installed in $INSTALL_DIR
echo
