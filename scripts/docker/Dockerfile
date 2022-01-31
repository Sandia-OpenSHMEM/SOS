FROM ubuntu:20.04
#FROM intel/oneapi-basekit:devel-ubuntu18.04


ENV DK_INSTALL=/usr/sos
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y
RUN apt-get install -y git
RUN apt-get install -y vim
RUN apt-get install -y build-essential
RUN apt-get install -y wget
RUN apt-get install -y automake # installs autoconf
RUN apt-get install -y libtool
RUN apt-get install -y flex
RUN apt-get install -y python
RUN apt-get install -y libhwloc-dev

#Packages added on Travis
RUN apt-get install -y libev-libevent-dev
RUN apt-get install -y libev-dev
RUN apt-get install -y mpich

# Build Libevent
WORKDIR $DK_INSTALL
RUN wget https://github.com/libevent/libevent/releases/download/release-2.1.10-stable/libevent-2.1.10-stable.tar.gz
RUN tar -xzvf libevent-2.1.10-stable.tar.gz
WORKDIR libevent-2.1.10-stable
RUN ./autogen.sh
RUN ./configure --prefix=$DK_INSTALL/libevent-2.1.10-stable/install
RUN make clean all install

# Build PMIx
WORKDIR $DK_INSTALL
RUN git clone -b v4.1.1rc6 --depth 10 https://github.com/pmix/pmix pmix
WORKDIR pmix
RUN ./autogen.pl
RUN ./configure --prefix=$DK_INSTALL/pmix/install --with-libevent=$DK_INSTALL/libevent-2.1.10-stable/install --without-libev --disable-debug CFLAGS=-O3 --disable-shared --enable-static
RUN make
RUN make install

# Build PRRTE
WORKDIR $DK_INSTALL
RUN git clone -b v2.0.0 --depth 10 https://github.com/pmix/prrte prrte
WORKDIR prrte
RUN ./autogen.pl
RUN ./configure --prefix=$DK_INSTALL/prrte/install --with-pmix=$DK_INSTALL/pmix/install --without-slurm --with-libevent=$DK_INSTALL/libevent-2.1.10-stable/install --without-libev --disable-shared --enable-static
RUN make
RUN make install

# Build Libfabric
WORKDIR $DK_INSTALL
RUN git clone -b v1.7.x --depth 10 https://github.com/ofiwg/libfabric.git libfabric
WORKDIR libfabric
RUN ./autogen.sh
# To build basic Libfabric
RUN ./configure --prefix=$DK_INSTALL/libfabric/install
# If running with oneapi clang compilers:
  #RUN ./configure --prefix=$DK_INSTALL/libfabric/install-oneapi CC=clang CXX=clang++
# If running with GNI provider:
  #RUN ./configure --prefix=$DK_INSTALL/libfabric/install-gni --enable-gni --disable-verbs --disable-sockets --disable-udp --disable-psm --disable-tcp
RUN make
RUN make install

# Build Portals 4
WORKDIR $DK_INSTALL
RUN git clone --depth 10 https://github.com/regrant/portals4.git portals4
WORKDIR portals4
RUN ./autogen.sh
RUN ./configure --prefix=$DK_INSTALL/portals4/install --enable-zero-mrs --enable-reliable-udp --disable-pmi-from-portals
RUN make
RUN make install

# Build UCX
WORKDIR $DK_INSTALL
RUN git clone -b v1.9.0 --depth 10 https://github.com/openucx/ucx.git
WORKDIR ucx
RUN ./autogen.sh
RUN ./configure --prefix=$DK_INSTALL/ucx/install --enable-mt --disable-numa --without-java
RUN make
RUN make install

# Build SOS
WORKDIR $DK_INSTALL
RUN git clone https://github.com/Sandia-OpenSHMEM/SOS.git
WORKDIR SOS
RUN ./autogen.sh
# To Build SOS w/ Basic Libfabric
RUN ./configure --prefix=$DK_INSTALL/SOS/install --with-ofi=$DK_INSTALL/libfabric/install --without-ucx --without-portals4 --enable-pmi-simple --disable-fortran
# To Build SOS w/ oneapi compilers
  #RUN ./configure --prefix=$DK_INSTALL/SOS/install-oneapi --with-ofi=$DK_INSTALL/libfabric/install-oneapi --without-ucx --without-portals4 --enable-pmi-simple CC=clang CXX=clang++ --disable-fortran
# To Build SOS w/ GNI provider
  #RUN ./configure --prefix=$DK_INSTALL/SOS/install-gni --with-ofi=$DK_INSTALL/libfabric/install-gni --without-ucx --without-portals4 --enable-pmi-simple --enable-ofi-mr=basic --enable-completion-polling --disable-fortran
# To Build SOS w/ Libfabric & PMIx
  #RUN ./configure --prefix=$DK_INSTALL/SOS/install --with-ofi=$DK_INSTALL/libfabric/install --without-ucx --without-portals4 --with-pmix=$DK_INSTALL/pmix/install --disable-shared --enable-static
# To Build SOS w/ Portals 4
  #RUN ./configure --prefix=$DK_INSTALL/SOS/install-portals --with-ofi=$DK_INSTALL/portals4/install --without-ucx --without-ofi --enable-pmi-simple
# To Build SOS w/ UCX
  #RUN ./configure --prefix=$DK_INSTALL/SOS/install-ucx --with-ucx=$DK_INSTALL/ucx/install --without-ofi --without-portals4 --enable-pmi-simple
RUN make
RUN make install
RUN make check TESTS= -j

#Start PRRTE Server and Use prun to run tests
#RUN make check TESTS= -j
#ENV PATH=$DK_INSTALL/prrte/install/bin:$PATH
#RUN prte --daemonize --host localhost:4 --allow-run-as-root
#RUN SHMEM_INFO=1 make VERBOSE=1 TEST_RUNNER="prun -np 2" check
#RUN pterm
