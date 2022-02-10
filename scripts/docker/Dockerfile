FROM ubuntu:20.04
#FROM intel/oneapi-basekit:devel-ubuntu18.04

ENV SOS_INSTALL=/home/sos
RUN mkdir /home/sos
ENV DEBIAN_FRONTEND=noninteractive
ENV CC=gcc
ENV CXX=g++
# For OneAPI Compilers use clang and clang++
  #ENV CC=clang
  #ENV CXX=clang++

RUN apt-get update -y && apt-get install -y \
  git                                       \
  vim                                       \
  build-essential                           \
  wget                                      \
  automake                                  \
  libtool                                   \
  flex                                      \
  python                                    \
  libhwloc-dev

#Packages added on Travis
RUN apt-get install -y libev-libevent-dev   \
  libev-dev                                 \
  mpich

# Build Libevent
WORKDIR $SOS_INSTALL
RUN wget -c https://github.com/libevent/libevent/releases/download/release-2.1.10-stable/libevent-2.1.10-stable.tar.gz
RUN tar -xzvf libevent-2.1.10-stable.tar.gz
WORKDIR libevent-2.1.10-stable
RUN ./autogen.sh
RUN ./configure --prefix=$SOS_INSTALL/libevent-2.1.10-stable/install
RUN make clean all install

# Build Hydra
WORKDIR $SOS_INSTALL
RUN wget -c https://www.mpich.org/static/downloads/4.0/hydra-4.0.tar.gz
RUN tar -xzvf hydra-4.0.tar.gz
WORKDIR hydra-4.0
RUN ./autogen.sh
RUN ./configure --prefix=$SOS_INSTALL/hydra-4.0/install
RUN make all install

# Build PMIx
WORKDIR $SOS_INSTALL
RUN git clone -b v4.1.1rc6 --depth 10 https://github.com/pmix/pmix pmix
WORKDIR pmix
RUN ./autogen.pl
RUN ./configure --prefix=$SOS_INSTALL/pmix/install --with-libevent=$SOS_INSTALL/libevent-2.1.10-stable/install --without-libev --disable-debug CFLAGS=-O3 --disable-shared --enable-static
RUN make
RUN make install

# Build PRRTE
WORKDIR $SOS_INSTALL
RUN git clone -b v2.0.0 --depth 10 https://github.com/pmix/prrte prrte
WORKDIR prrte
RUN ./autogen.pl
RUN ./configure --prefix=$SOS_INSTALL/prrte/install --with-pmix=$SOS_INSTALL/pmix/install --without-slurm --with-libevent=$SOS_INSTALL/libevent-2.1.10-stable/install --without-libev --disable-shared --enable-static
RUN make
RUN make install

# Build Libfabric
WORKDIR $SOS_INSTALL
RUN git clone -b v1.7.x --depth 10 https://github.com/ofiwg/libfabric.git libfabric
WORKDIR libfabric
RUN ./autogen.sh
# To build basic Libfabric
RUN ./configure --prefix=$SOS_INSTALL/libfabric/install
# If running with GNI provider:
  #RUN ./configure --prefix=$SOS_INSTALL/libfabric/install-gni --enable-gni --disable-verbs --disable-sockets --disable-udp --disable-psm --disable-tcp
RUN make
RUN make install

# Build Portals 4
WORKDIR $SOS_INSTALL
RUN git clone --depth 10 https://github.com/regrant/portals4.git portals4
WORKDIR portals4
RUN ./autogen.sh
RUN ./configure --prefix=$SOS_INSTALL/portals4/install --enable-zero-mrs --enable-reliable-udp --disable-pmi-from-portals
RUN make
RUN make install

# Build UCX
WORKDIR $SOS_INSTALL
RUN git clone -b v1.9.0 --depth 10 https://github.com/openucx/ucx.git
WORKDIR ucx
RUN ./autogen.sh
RUN ./configure --prefix=$SOS_INSTALL/ucx/install --enable-mt --disable-numa --without-java
RUN make
RUN make install

# Build SOS
WORKDIR $SOS_INSTALL
RUN git clone https://github.com/Sandia-OpenSHMEM/SOS.git
WORKDIR SOS
RUN ./autogen.sh
# To Build SOS w/ Basic Libfabric
RUN ./configure --prefix=$SOS_INSTALL/SOS/install --with-ofi=$SOS_INSTALL/libfabric/install --without-ucx --without-portals4 --enable-pmi-simple --disable-fortran
# To Build SOS w/ GNI provider
  #RUN ./configure --prefix=$SOS_INSTALL/SOS/install-gni --with-ofi=$SOS_INSTALL/libfabric/install-gni --without-ucx --without-portals4 --enable-pmi-simple --enable-ofi-mr=basic --enable-completion-polling --disable-fortran
# To Build SOS w/ Libfabric & PMIx
  #RUN ./configure --prefix=$SOS_INSTALL/SOS/install --with-ofi=$SOS_INSTALL/libfabric/install --without-ucx --without-portals4 --with-pmix=$SOS_INSTALL/pmix/install --disable-shared --enable-static
# To Build SOS w/ Portals 4
  #RUN ./configure --prefix=$SOS_INSTALL/SOS/install-portals --with-ofi=$SOS_INSTALL/portals4/install --without-ucx --without-ofi --enable-pmi-simple
# To Build SOS w/ UCX
  #RUN ./configure --prefix=$SOS_INSTALL/SOS/install-ucx --with-ucx=$SOS_INSTALL/ucx/install --without-ofi --without-portals4 --enable-pmi-simple
RUN make
RUN make install
RUN make check TESTS= -j

#Start PRRTE Server and Use prun to run tests
#RUN make check TESTS= -j
#ENV PATH=$SOS_INSTALL/prrte/install/bin:$PATH
#RUN prte --daemonize --host localhost:4 --allow-run-as-root
#RUN SHMEM_INFO=1 make VERBOSE=1 TEST_RUNNER="prun -np 2" check
#RUN pterm
