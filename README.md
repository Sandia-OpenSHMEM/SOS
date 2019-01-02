# ![SOS logo](https://raw.githubusercontent.com/davidozog/sandia-shmem/pr/readme_markdown/extra/SOS.png) Sandia OpenSHMEM ![SOS logo](https://raw.githubusercontent.com/davidozog/sandia-shmem/pr/readme_markdown/extra/SOS.png)
<!-- TODO: update davidozog/pr/readme_markdown -> Sandia-OpenSHMEM/SOS -->
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

The Sandia OpenSHMEM implementation utilizes the Autoconf/Automake/Libtool
build system.  The standard GNU configure script and make system is used, as
follows:
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

Options to configure include:

<!-- Note: This markdown doesn't yet work with Github because you cannot specify a column width
| Flag                      | Description |
| --------------            | ----------- |
| `--prefix=<DIR>`          | Install implementation in `<DIR>`, default: /usr/local |
| `--with-portals4=<DIR>`   | Find the Portals 4 library in `<DIR>`. |
...
-->

<table>
 <thead>
   <tr>
    <th>Flag</th>
    <th>Description</th>
   </tr>
 </thead>
<tbody>
 <tr>
  <td><code>--prefix=&lt;DIR&gt;</code></td>
  <td>Install implementation in <code>&lt;DIR&gt;</code>, default: /usr/local</td>
 </tr>
 <tr>
  <td><code>--with-portals4=&lt;DIR&gt;</code></td>
  <td>Find the Portals 4 library in <code>&lt;DIR&gt;</code>.</td>
 </tr>
 <tr>
  <td><code>--with-ofi=&lt;DIR&gt;</code></td>
  <td>Find the libfabric library in <code>&lt;DIR&gt;</code>.</td>
 </tr>
 <tr>
  <td><code>--with-xpmem=&lt;DIR&gt;</code></td>
  <td>Find the XPMEM library in <code>&lt;DIR&gt;</code>.</td>
 </tr>
 <tr>
  <td><code>--with-cma</code></td>
  <td>Use cross-memory attach for on-node communication.</td>
 </tr>
 <tr>
  <td><code>--with-pmi=&lt;DIR&gt;</code></td>
  <td>Location of PMI installation.  Configure will automatically look for the PMI runtime provided by the Portals 4 reference implementation.</td>
 </tr>
 <tr>
  <td><code>--enable-pmi-simple</code></td>
  <td>Include support for interfacing with a PMI 1.0 launcher.  The launcher must be provided by a separate package, such as MPICH, Hydra, or SLURM.</td>
 </tr>
 <tr>
  <td><code>--enable-error-checking</code></td>
  <td>Enable error checking in SHMEM calls. This will increase the overhead of communication operations.</td>
 </tr>
 <tr>
  <td><code>--enable-hard-polling</code></td>
  <td>When using only the network transport, the implementation will use counting events to block the implementation when waiting for local memory changes.  On some implementations, enabling hard polling may increase target side message rate.</td>
 </tr>
 <tr>
  <td><code>--enable-remote-virtual-addressing</code></td>
  <td>Enable optimizations assuming the symmetric heap is always symmetric with regards to virtual address. This may cause applications to abort during <code>shmem_init()</code> if such a symmetric heap can not be created, but will reduce the instruction count for some operations. This optimization also requires that the Portals 4 implementation support <code>BIND_INACCESSIBLE</code> on LEs.  This optimization will reduce the overhead of communication calls.</td>
 </tr>
 <tr>
  <td><code>--disable-fortran</code></td>
  <td>Disable the Fortran bindings.  This may be useful if the machine has a Fortran compiler which does not support ISO_C_BINDING.</td>
 </tr>
 <tr>
  <td><code>--enable-nonblocking-fence</code></td>
  <td>By default, shmem_fence() is equivalent to shmem_quiet(), which can be a lengthy operation. Enabling this feature results in the ordering point being moved from the shmem_fence() to the next put-like call, which can help improve overlap in some cases.</td>
 </tr>
 <tr>
  <td width="40%"><code>--enable-total-data-ordering=&lt;yes|no|check&gt;</code></td>
  <td>If a network supports total data ordering (that is, ordering guarantees to two different addresses on the same target node), this option can remove the shmem_quiet() from shmem_fence() calls when sending short messages.  The option does, however, force ordering requirements on the network, so experimentation may be necessary to determine the best configuration.  Yes means always assume total data ordering is available and abort a job if that's not the case.  No means never use total data ordering optimizations.  Check will result in slightly higher overhead than "yes", but will provide a fallback if the network doesn't provide total data ordering.</td>
 </tr>
 </tbody>
</table>

There are many other options to configure to influence performance and
behavior.  See `configure --help` for documentation on available
options.

## SHMEM Runtime Support

### Environment variables:
<table>
 <thead>
   <tr>
    <th>Environment Variable</th>
    <th>Description</th>
   </tr>
 </thead>
<tbody>
 <tr>
  <td><code>SHMEM_VERSION</code></td>
  <td>If defined, print SHMEM version during start_pes().</td>
 </tr>
 <tr>
  <td width="45%"><code>SHMEM_INFO</code></td>
  <td>If defined, print (stdout) SHMEM environment variables.</td>
 </tr>
 <tr>
  <td><code>SHMEM_SYMMETRIC_SIZE</code> (default: 64 MiB) </td>
  <td>
      The allocated size of the symmetric heap which shmalloc() and shfree() operates on. The size value can be scaled with a suffix of </br>
      'K' for kilobytes (B * 1024), </br>
      'M' for Megabytes (KiB * 1024) </br>
      'G' for Gigabytes (MiB * 1024) </br>
  </td>
 </tr>
 <tr>
  <td><code>SHMEM_BOUNCE_SIZE</code> (default: 2 KiB)</td>
  <td>The maximum size of a bounce buffer for put messages. Messages greater
      than the immediate send value for the underlying network but greater than
      this threshold will be copied into a bounce buffer and then sent. </td>
 </tr>
 <tr>
  <td><code>SHMEM_MAX_BOUNCE_BUFFERS</code> (default: 128)</td>
  <td>The maximum number of bounce buffers that can be created per context.</td>
 </tr>
 <tr>
  <td><code>SHMEM_COLL_CROSSOVER</code> (default: 4)</td>
  <td>For num_pes &lt; <code>SHMEM_COLL_CROSSOVER</code>, collective algorithms are serial instead of tree based.</td>
 </tr>
 <tr>
  <td><code>SHMEM_COLL_RADIX</code> (default: 4)</td>
  <td>Controls the width of the n-ary tree for collectives, such that each node will fanout-send to a max of approximately <code>SHMEM_COLL_RADIX</code></td>
 </tr>
 <tr>
  <td><code>SHMEM_SYMMETRIC_HEAP_USE_MALLOC</code> (default: 0)</td>
  <td>If set to a non-zero integer, will use `malloc()` instead of
      `mmap()` to allocate the symmetric heap.  This option may result in
      incorrect behavior when remote virtual addressing is enabled.</td>
 </tr>
 <tr>
  <td><code>SHMEM_BARRIER_ALGORITHM</code> (default: auto)</td>
  <td>Algorithm to use for barriers.  Default is to auto-select (which
      may result in different algorithms being used for different 
      PE sets).  Options are: auto, linear, tree, dissem.</td>
 </tr>
 <tr>
  <td><code>SHMEM_BCAST_ALGORITHM</code> (default: auto)</td>
  <td>Algorithm to use for broadcasts.  Default is to auto-select (which
      may result in different algorithms being used for different 
      PE sets).  Options are: auto, linear, tree.</td>
 </tr>
 <tr>
  <td><code>SHMEM_REDUCE_ALGORITHM</code> (default: auto)</td>
  <td>Algorithm to use for reductions.  Default is to auto-select (which
      may result in different algorithms being used for different 
      PE sets).  Options are: auto, linear, tree, recdbl.</td>
 </tr>
 <tr>
  <td><code>SHMEM_COLLECT_ALGORITHM</code> (default: auto)</td>
  <td>Algorithm to use for allgathers.  Default is to auto-select (which
      may result in different algorithms being used for different 
      PE sets).  Options are: auto, linear.</td>
 </tr>
 <tr>
  <td><code>SHMEM_FCOLLECT_ALGORITHM</code> (default: auto)</td>
  <td>Algorithm to use for allgathers with fixed contribution amounts.
      Default is to auto-select (which may result in different 
      algorithms being used for different PE sets).  
      Options are: auto, linear, ring, recdbl.  Note that recursive
      doubling (recdbl) will fall back to ring if the PE set is not a
      power of two in size.</td>
 </tr>
 <tr>
  <td><code>SHMEM_BARRIERS_FLUSH</code> (default: off)</td>
  <td>If defined, standard output (stdout) and error (stderr) streams 
      will be flushed at the beginning of each barrier operation.</td>
 </tr>
 <tr>
  <td><code>SHMEM_CMA_PUT_MAX</code> (default: 8192)</td>
  <td>`--with-cma`, shmem put lengths &lt;= <code>CMA_PUT_MAX</code> use <code>process_vm_writev();</code>
      otherwise use Portals4 transport put.</td>
 </tr>
 <tr>
  <td><code>SHMEM_CMA_GET_MAX</code> (default: 16384)</td>
  <td>`--with-cma`, shmem get lengths &lt;= <code>CMA_GET_MAX</code> use <code>process_vm_readv();</code>
      otherwise use Portals4 transport get.
  </td>
 </tr>
 <tr>
  <td><code>SHMEM_SYMMETRIC_HEAP_USE_HUGE_PAGES</code></br>(default: off)</td>
  <td>If defined, large pages will be used to back the symmetric heap.  This
      feature is only available on Linux.</td>
 </tr>
 <tr>
  <td><code>SHMEM_SYMMETRIC_HEAP_PAGE_SIZE</code> (default: 2MB)</td>
  <td>Used to specify a large page size when using large pages to back the
      symmetric heap.  Ignored if <code>SHMEM_SYMMETRIC_HEAP_USE_HUGE_PAGES</code> is not
      set.  Refer to <code>SHMEM_SYMMETRIC_SIZE</code> for input syntax.</td>
 </tr>
 <tr>
  <td><code>SHMEM_DISABLE_ASLR_CHECK</code> (default: on)</td>
  <td>Disable runtime checks for address space layout randomization (ASLR).</td>
 </tr>
 </tbody>
</table>

### OFI Transport Environment variables:
<table>
 <thead>
   <tr>
    <th>Environment Variable</th>
    <th>Description</th>
   </tr>
 </thead>
<tbody>
 <tr>
  <td width="45%"><code>SHMEM_OFI_PROVIDER</code> (default: auto)</td>
  <td>The name of the provider that should be used by the OFI transport.
      Shell-style wildcards, including * and ?, are allowed.  The fi_info
      utility included with libfabric can be used for assistance with
      identifying the desired provider.</td>
 </tr>
 <tr>
  <td><code>SHMEM_OFI_FABRIC</code> (default: auto)</td>
  <td>The name of the fabric that should be used by the OFI transport.
      Shell-style wildcards, including * and ?, are allowed.  The `fi_info`
      utility included with libfabric can be used for assistance with
      identifying the desired fabric.</td>
 </tr>
 <tr>
  <td><code>SHMEM_OFI_DOMAIN</code> (default: auto)</td>
  <td>The name of the fabric domain that should be used by the OFI transport.
      Shell-style wildcards, including * and ?, are allowed.  The `fi_info`
      utility included with libfabric can be used for assistance with
      identifying the desired fabric domain.</td>
 </tr>
 <tr>
  <td><code>SHMEM_OFI_ATOMIC_CHECKS_WARN</code> (default: off)</td>
  <td>If defined, OFI will not abort if fabric provider doesn't support every
      data type x op combination, instead it will print a warning.</td>
 </tr>
 <tr>
  <td><code>SHMEM_OFI_TX_POLL_LIMIT</code> (default: 0)</td>
  <td>Sets the maximum number of iterations for the transmit polling loop
      (for put/quiet operations).  Setting this to -1 enables continuous
      completion polling (i.e. there is no polling limit).  The default
      behavior is to call `fi_cntr_wait` without polling.</td>
 </tr>
 <tr>
  <td><code>SHMEM_OFI_RX_POLL_LIMIT</code> (default: 0)</td>
  <td>Sets the maximum number of iterations for the receive polling loop (for
      get/wait operations).  Setting this to -1 enables continuous completion
      polling (i.e. there is no polling limit).  The default behavior is to
      call `fi_cntr_wait` without polling.</td>
 </tr>
 <tr>
  <td><code>SHMEM_OFI_STX_MAX</code> (default: 1)</td>
  <td>Sets the maximum number of sharable transmit contexts (STXs) per PE.
      STXs are the underlying transmit resources that are allocated to
      OpenSHMEM contexts and they are allocated using the algorithm specified
      by the `SHMEM_OFI_STX_ALLOCATOR` parameter.</td>
 </tr>
 <tr>
  <td><code>SHMEM_OFI_STX_ALLOCATOR</code> (default: round-robin)</td>
  <td>Algorithm for allocating STX resources to OpenSHMEM contexts.  In
      particular, the algorithm determines how resources are shared by
      contexts once all STXs have been allocated.  Options are: round-robin,
      random.</td>
 </tr>
 <tr>
  <td><code>SHMEM_OFI_STX_THRESHOLD</code> (default: 1)</td>
  <td>Number of contexts that must be allocated to all shared STXs before
      another shared STX can be allocated.  This threshold can be increased to
      reduce the number of shared STXs and increase the number of STXs
      available for private use (i.e., with contexts that enable the
      `SHMEM_CTX_PRIVATE` option).</td>
 </tr>
 <tr>
  <td><code>SHMEM_OFI_STX_DISABLE_PRIVATE</code> (default: off)</td>
  <td>Disable STX privatization. Enabling this may improve load balance across
      transmit resources, especially in scenarios where the number of contexts
      exceeds the number of STXs.</td>
 </tr>
 </tbody>
</table>
       
### Debugging Environment variables:
<table>
 <thead>
   <tr>
    <th>Environment Variable</th>
    <th>Description</th>
   </tr>
 </thead>
<tbody>
 <tr>
  <td width="40%"><code>SHMEM_DEBUG</code> (default: off)</td>
  <td>If defined enables debugging messages from OpenSHMEM runtime.</td>
 </tr>
 <tr>
  <td><code>SHMEM_TRAP_ON_ABORT</code> (default: off)</td>
  <td>If defined, generate a trap when aborting an OpenSHMEM program.  This
      can be used to interface with a debugger or generate core files.</td>
 </tr>
 <tr>
  <td><code>SHMEM_BACKTRACE</code> (default: &lt;empty&gt;)</td>
  <td>Can be used to choose the backtracing mechanism. Default value is NULL 
      for which no backtrace information is provided upon failure. User can set 
      this with any one of these available options: execinfo, gdb, auto.</td>
 </tr>
 </tbody>
</table>
