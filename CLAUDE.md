# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## About

Sandia OpenSHMEM (SOS) is a C implementation of the [OpenSHMEM specification](http://openshmem.org/) for Partitioned Global Address Space (PGAS) parallel programming. It runs over three network transports — **OFI (libfabric)**, **Portals 4**, and **UCX** — with optional on-node acceleration via **XPMEM** or **CMA**. Exactly one network transport must be selected at configure time.

Target deployment scale: PPN >= 104, thousands of nodes. Evaluate all algorithm and data structure tradeoffs at this scale.

## Build

```bash
./autogen.sh                          # Only needed after fresh clone or configure.ac changes
./configure --with-ofi=<DIR> --enable-pmi-simple
make -j$(nproc)
make install
```

Key configure flags:

| Flag | Purpose |
|------|---------|
| `--with-ofi=<DIR>` | OFI/libfabric transport (most common) |
| `--with-portals4=<DIR>` | Portals 4 transport |
| `--with-ucx=<DIR>` | UCX transport |
| `--with-xpmem=<DIR>` | XPMEM on-node optimization |
| `--with-cma` | CMA on-node optimization |
| `--enable-pmi-simple` | Bundled PMI 1.0 client (works with MPICH/Hydra/SLURM) |
| `--enable-pmi-mpi CC=mpicc` | Use MPI for process management |
| `--enable-error-checking` | Runtime argument validation |
| `--enable-remote-virtual-addressing` | Performance optimization (assumes symmetric virtual addresses) |
| `--enable-mr-endpoint` | Use FI_MR_ENDPOINT (required for CXI provider) |
| `--enable-mr-local` | Use FI_MR_LOCAL to require local MR descriptors in RMA operations |
| `--enable-debug` | Debug symbols |
| `--enable-picky` | Strict compiler warnings (developer mode; replaces a dedicated lint target) |
| `--disable-fortran` | Skip Fortran bindings |

## Tests

```bash
make check                             # Full test suite
make check NPROCS=4                    # Override number of PEs
make check TEST_RUNNER="mpiexec -n 4 -ppn 2 -hosts node1,node2"
```

Run a single test after `make install`:
```bash
oshrun -n 4 ./modules/tests-sos/test/unit/shmem_put_get
```

Tests live in the `modules/tests-sos/` git submodule under `test/unit/`, `test/apps/`, `test/performance/`, and `test/shmemx/`.

## Architecture

```
Public API (shmem.h / pshmem.h)         ← generated from mpp/*.h4 via M4
        ↓
API Implementations (src/init_c.c, src/data_c.c4, src/atomic_c.c4, …)
        ↓
Transport Abstraction (src/transport.h)  ← compile-time dispatch
        ↓                                    (USE_OFI / USE_PORTALS4 / USE_UCX)
Network Transport                        Runtime/PMI Layer
  transport_ofi.c                          runtime-pmi.c
  transport_portals4.c                     runtime-pmi2.c
  transport_ucx.c                          runtime-pmix.c
                                           runtime-mpi.c
        ↓
On-node Transport (optional, layered on top)
  transport_xpmem.c / transport_cma.c
```

### Key directories

| Directory | Role |
|-----------|------|
| `src/` | Core library (`libsma`): transport, runtime, collectives, init, memory |
| `mpp/` | Public header templates (`.h4`) — `shmem.h`, `shmemx.h`, `pshmem.h` |
| `bindings/` | M4 macros for type-generic binding generation |
| `pmi-simple/` | Bundled PMI 1.0 client for `--enable-pmi-simple` builds |
| `modules/tests-sos/` | Test suite (git submodule) |
| `examples/` | Minimal usage examples (`hello.c`, `pi.c`) |
| `scripts/` | CI helpers, man page generation, build convenience scripts |

### Transport abstraction

`src/transport.h` selects the active transport at compile time via `#ifdef USE_OFI / USE_PORTALS4 / USE_UCX`. All transports implement the same inline function interface declared in their respective `transport_*.h` headers. When adding transport-level features, apply changes consistently across all three transports (and `transport_none.c` for the no-transport stub).

### Runtime/PMI layer

`src/runtime.h` declares the runtime interface. Exactly one of `runtime-pmi.c`, `runtime-pmi2.c`, `runtime-pmix.c`, or `runtime-mpi.c` is compiled in, selected by configure flags. Handles PE discovery, KVS exchange, and barriers during `shmem_init()`.

### Collectives

`src/collectives.c` and `src/shmem_collectives.h` implement barrier, broadcast, reduction, scan, collect, and fcollect. Each operation has a `coll_type_t` (AUTO, LINEAR, TREE, DISSEM, RING, RECDBL) selectable at runtime via `SHMEM_` env vars. AUTO selects based on `SHMEM_COLL_CROSSOVER` (default 4 PEs) and `SHMEM_COLL_SIZE_CROSSOVER` (default 16 KB). The k-ary tree builder (`shmem_internal_build_kary_tree`) is shared across tree-based algorithms; radix is controlled by `SHMEM_COLL_RADIX` (default 4).

## Code Generation (M4 Templates)

Many `src/` files and all `mpp/` headers are **generated from M4 templates** — do not edit generated files directly.

| Template | Generated output |
|----------|-----------------|
| `src/*.c4` | `src/*.c` |
| `mpp/*.h4`, `src/shr_transport.h4` | `*.h` |

Templates `include(shmem_bind_c.m4)` (and Fortran/C11 variants) from `bindings/` to expand type-generic macros. Running `make` regenerates them automatically. Generated files carry `/* This is a generated file, do not edit directly. */`.

## Key Conventions

### Naming prefixes

| Prefix | Scope |
|--------|-------|
| `shmem_internal_` | Internal implementation; not public API |
| `shmem_transport_` | Transport layer |
| `shmem_runtime_` | Runtime/PMI layer |
| `shmemx_` | Extended/experimental (non-standard) API |
| `pshmem_` | Profiling API wrappers |

### Internal header guard

Internal source files must define `SHMEM_INTERNAL_INCLUDE` before including `shmem.h`:
```c
#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
```

### Error handling

Use macros from `shmem_internal.h` rather than `abort()`/`exit()`:
```c
RAISE_ERROR(ret)             // non-zero return code
RAISE_ERROR_STR("message")   // string message
RAISE_ERROR_MSG("fmt", ...)  // printf-style message
```

### Symmetric heap globals

`shmem_internal_heap_base` / `shmem_internal_heap_length` and `shmem_internal_data_base` / `shmem_internal_data_length` are set during `shmem_init()`. Pointer validity checks use macros in `shmem_internal.h`.

### Runtime environment variables

Controlled via `SHMEM_`-prefixed env vars defined in `src/shmem_env_defs.h` using the `SHMEM_INTERNAL_ENV_DEF` macro. Key ones for collective tuning: `SHMEM_BARRIER_ALGORITHM`, `SHMEM_BCAST_ALGORITHM`, `SHMEM_REDUCE_ALGORITHM`, `SHMEM_FCOLLECT_ALGORITHM`, `SHMEM_COLL_RADIX`, `SHMEM_COLL_CROSSOVER`, `SHMEM_SYMMETRIC_SIZE`.

### File mode lines

C source files begin with `/* -*- C -*- */`. M4 templates begin with `dnl vi: set ft=m4`.

### Compiler wrappers

After install: `oshcc` / `oshc++` / `oshfort` to compile; `oshrun` to launch jobs.

## CI

GitHub Actions (`.github/workflows/ci.yml`) tests a matrix of OFI versions (v1.7.x through v2.1.x) and feature combinations: PMI simple, error checking, RVA, XPMEM, UCX, and various collective algorithm settings.
