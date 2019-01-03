# ![SOS logo](https://raw.githubusercontent.com/davidozog/sandia-shmem/pr/readme_markdown/extra/SOS.png) Sandia OpenSHMEM ![SOS logo](https://raw.githubusercontent.com/davidozog/sandia-shmem/pr/readme_markdown/extra/SOS.png)
<!-- TODO: update davidozog/pr/readme_markdown -> Sandia-OpenSHMEM/SOS -->
[![GitHub version](https://badge.fury.io/gh/Sandia-OpenSHMEM%2FSOS.svg)](https://badge.fury.io/gh/Sandia-OpenSHMEM%2FSOS)
[![Build Status](https://travis-ci.org/Sandia-OpenSHMEM/SOS.svg?branch=master)](https://travis-ci.org/Sandia-OpenSHMEM/SOS)
[![Coverity Scan Build Status](https://scan.coverity.com/projects/9375/badge.svg)](https://scan.coverity.com/projects/9375)

## About

Sandia OpenSHMEM is an implementation of the OpenSHMEM specification over
Portals 4.0, the Open Fabrics Interface (OFI), and XPMEM.

Please refer to the [`tests-sos` repository](https://github.com/openshmem-org/tests-sos)
to download only the unit tests and the performance test suite that are
included with Sandia OpenSHMEM.

This README file is formatted with Markdown.  The [human-readable README file is located
here](https://github.com/Sandia-OpenSHMEM/SOS/blob/master/README).

The following sections briefly describe how to build Sandia OpenSHMEM and the
available configuration parameters and runtime environment variables.  More
detailed information can be found in the [SOS Wiki pages](https://github.com/Sandia-OpenSHMEM/SOS/wiki).

## Building

The Sandia OpenSHMEM implementation utilizes the GNU Autoconf/Automake/Libtool
tools to generate a configure script.  If the `configure` file is not present
(e.g. after downloading the repository for the first time), generate it
by running:
```
  $ ./autogen.sh
```
Once the configure file exists, run:
```
  $ ./configure <options>
  $ make
  $ make check
  $ make install
```
The `make check` step is not strictly necessary, but is a good idea.  Make
check utilizes the `TEST_RUNNER` and `NPROCS` make variables, which can be used to
override defaults, e.g. `make check NPROCS=4` or `make check
TEST_RUNNER="mpiexec -n 2 -ppn 1 -hosts compute1,compute2"`.

Sandia OpenSHMEM must be configured to use either the Portals 4 or OFI network
transport, but not both.  It can optionally be configured to use XPMEM or CMA
to optimize communication between PEs within the same shared memory domain.

## Customizing the build
Please refer to the full [README file located here](https://github.com/Sandia-OpenSHMEM/SOS/blob/master/README)
to see the available configure options, runtime environment variables, OFI
transport options, and debugging options.
