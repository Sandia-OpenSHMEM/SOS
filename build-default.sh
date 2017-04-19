#!/bin/bash

function confirm {
  def=$1;
  shift 1
  read -p "$* (default: $def) " ret
  [[ -z $ret ]] && ret=$def;
  echo $ret
}

function Yn  {
  echo -n $* "[Y/n] ";
  read ret;
  echo $ret | grep -v '^n' >/dev/null;
}

prefixdir=$(confirm "${HOME}/sos" "Installation directory?")
fetchhydra=""; Yn "Fetch hydra?" && fetchhydra="y";
libfabric_build=""; Yn "Rebuild libfabric?" && libfabric_build="y";
reconfigure=""; Yn "Reconfigure SOS?" && reconfigure="y";
rebuild=""; Yn "Rebuild SOS?" && rebuild="y";
rebuild_tests=""; Yn "Rebuild Tests?" && rebuild_tests="y";
run_tests=""; Yn "Run tests?" && run_tests="y";

mkdir -p $prefixdir

if [[ -n $fetchhydra ]]; then
  d=$(mktemp -d) &&
  pushd $d &&
  ((wget http://www.mpich.org/static/downloads/3.2/hydra-3.2.tar.gz &&
   tar xzf hydra-3.2.tar.gz &&
   cd hydra-3.2 &&
   ./configure --prefix="$prefixdir" && make -j && make install;) \
   || exit -1) &&
  popd
  rm -rf $d
fi

if [[ -n $libfabric_build ]]; then
  d=$(mktemp -d) &&
  pushd $d &&
  ((git clone --depth=1 https://github.com/ofiwg/libfabric &&
    cd libfabric &&
    ./autogen.sh && ./configure --prefix="$prefixdir" && make -j &&
    make install) ||
   exit -1) &&
  popd
  rm -rf $d
fi

if [[ -n $reconfigure ]]; then
  (./autogen.sh && \
  ./configure --prefix=${HOME}/sos --with-ofi=${HOME}/sos \
  --enable-pmi-simple) || exit -1
fi

if [[ -n $rebuild ]]; then
  make -j && make install
fi

if [[ -n $rebuild_tests ]]; then
  make check -j TESTS=
fi

if [[ -n $run_tests ]]; then
  make check -j
fi

