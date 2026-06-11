# Sandia OpenSHMEM (SOS) - Performance Branch Development Guide

## Overview
This is Bryan Morgan's fork of Sandia OpenSHMEM focused on performance optimizations for large-scale HPC systems (100K+ PEs, 1000+ nodes). The primary target is Cray EX systems with HPE Slingshot interconnect using the CXI (Cassini) NIC provider.

## Branch Structure

### Main Branches
- **performance**: Integration branch combining all performance work
- **bcm_test-FI_MR_LOCAL**: CXI hybrid MR descriptor mode + fence optimization
- **huge-page-support-clean**: Huge page allocation improvements for symmetric heap

### Upstream
- **upstream_mainline/main**: Official Sandia OpenSHMEM main branch
- **origin/performance**: Remote tracking for performance work

## Critical Scale Considerations

**Target Scale**: PPN (processes per node) ≥ 104, thousands of nodes, 100K+ total PEs

At this scale:
- Stack allocations via `alloca()` can cause stack overflow (typical limit: 1-8MB)
- Use `malloc()` for any variable-sized arrays that scale with PE count
- Always validate at 100K+ PE scale, not just small tests

## Key Technical Areas

### 1. CXI Provider Optimizations

#### Hybrid MR Descriptor Mode
- **What**: CXI-specific optimization that skips internal MR registration when desc is non-NULL
- **Where**: `src/transport_ofi.c:allocate_fabric_resources()`
- **Env Var**: `SHMEM_OFI_CXI_HYBRID_MR_DESC=1` (default: enabled)
- **API**: Uses `cxi_dom_ops_v3.enable_hybrid_mr_desc()` via `fi_open_ops()`
- **Impact**: Eliminates per-call MR cache lookups in fi_write/fi_writemsg
- **Critical**: Must be called BEFORE endpoint creation (provider only propagates to child EPs at creation time)
- **Gating**: Now properly gated on `shmem_transport_ofi_check_provider("cxi")` to avoid spurious warnings on non-CXI providers

#### Fence Fast Path
- **What**: Skip put_quiet in fence for CXI provider
- **Why**: CXI maintains per-EP FIFO ordering with FI_DELIVERY_COMPLETE, so explicit put_quiet poll is redundant
- **Impact**: ~1-2µs latency improvement per fence call
- **Code**: `src/transport_ofi.h:shmem_transport_fence()`
- **Gating**: `if (!shmem_transport_ofi_check_provider("cxi")) { shmem_transport_put_quiet(ctx); }`

#### Provider Name Handling
- **Storage**: `char *shmem_transport_ofi_prov_name` (global, non-static)
- **Initialization**: `strdup(info->p_info->fabric_attr->prov_name)` to survive fi_freeinfo()
- **Cleanup**: `free(shmem_transport_ofi_prov_name)` in shmem_transport_fini()
- **Check Function**: `shmem_transport_ofi_check_provider(const char *name)` - static inline in transport_ofi.h
- **Match Type**: Uses `strcmp()` (exact match) to avoid layered providers (e.g., "cxi;ofi_rxm") incorrectly inheriting CXI fast paths

### 2. Hierarchical Barrier

Three-phase algorithm for multi-node jobs:
1. **Phase 1 (intranode gather)**: XPMEM CPU stores/loads, dissemination barrier among local PEs
2. **Phase 2 (internode)**: NIC puts among root PEs only
3. **Phase 3 (intranode fanout)**: XPMEM dissemination in reverse sense

**Critical Implementation Details**:
- **PE Arrays**: Must use `malloc()` not `alloca()` - at 100K+ PEs, array size exceeds stack limit
- **Memory Management**: Single allocation for both local_pes and root_pes arrays: `malloc(2 * sizeof(int) * PE_size)`
- **Exit Paths**: All 3 exit paths must call `free(pe_bufs)` (degenerate case, after local sync, after internode sync)
- **Variables**:
  - `my_up_raw`: USED for atomic stores, must keep
  - `my_up_slot`: Unused volatile pointer, removed
  - `tree_parent_shr`: Unused in current implementation, removed

**Auto-Selection**: Enabled when `local_count >= HIER_BARRIER_THRESHOLD` (default: 2)

### 3. Heterogeneous Memory Ordering (FI_HMEM)

#### copy_self Implementation
**Location**: `src/shmem_comm.h:shmem_internal_copy_self()`

**Context**: This is **disabled by default**. Only applies when built with `--enable-ofi-hmem` to support device-attached memory (GPUs, accelerators) via libfabric's FI_HMEM support.

**Problem**: Self-copies with device memory must guarantee visibility at target device before returning.

**Solution**: Always use `put_quiet` (not `put_wait`) because:
- `put_nb` routes through 3 code paths:
  1. **inject**: Small copies (<= max_buffered_send), no counter event, completion=0
  2. **bounce-buffer**: Medium copies, sets completion watermark
  3. **put_large**: Large copies, sets completion watermark
- `put_wait` has early exit: `if ((*completion) == 0) return;`
- Inject path leaves completion=0, so `put_wait` returns immediately without ordering fence
- This creates a data race where device write is unordered

**Correct Implementation** (when `USE_FI_HMEM` defined):
```c
long completion = 0;
shmem_internal_put_nb(SHMEM_CTX_DEFAULT, dest, source, nelems,
                      shmem_internal_my_pe, &completion);
shmem_transport_put_quiet((shmem_transport_ctx_t *)SHMEM_CTX_DEFAULT);
```

`put_quiet` drains all pending puts and provides the NIC-level ordering fence needed for correctness across all code paths.

**Note**: Without `--enable-ofi-hmem`, `copy_self` simply calls `memcpy()` - this complexity only exists for device memory support.

### 4. Huge Page Support

**Location**: `src/symmetric_heap_c.c:mmap_alloc()`

**Allocation Strategy** (Linux only):
1. Try hugetlbfs file mapping (if SHMEM_SYMMETRIC_HEAP_USE_HUGE_PAGES=1)
2. Fall back to anonymous MAP_HUGETLB with explicit 2MB page size: `MAP_HUGE_SHIFT (21 << MAP_HUGE_SHIFT)`
3. Fall back to THP via `madvise(MADV_HUGEPAGE)`
4. Final fallback: regular pages

**Virtual Address Symmetry**:
- Prefer `requested_base` hint to preserve virtual address symmetry (required for RVA)
- Only use NULL hint as last resort if requested_base fails
- `requested_base = (data_base + data_length + 2GB) & ~(1GB - 1)` (aligned to 1GB boundary)

**Size Tracking**:
- hugetlbfs path rounds up to page boundary: `mapped_bytes = CEILING(bytes, page_size)`
- Return actual mapped size via `*mapped_bytes` for correct munmap/transport registration

### 5. Put Pipeline Depth

**Problem**: At high PPN (128+), concurrent puts can exhaust NIC TRS (transaction resources), causing `-FI_EAGAIN` storms and deadlock.

**Solution**: Pipeline depth limit via counter watermark
- **Env Var**: `SHMEM_OFI_PUT_PIPELINE_DEPTH` (default: 0 = unlimited)
- **Mechanism**: Throttle when `pending_put_cntr - retired_put_cntr >= depth`
- **Placement**: Throttle check BEFORE each fragment in put_large (not just once before loop)
- **Critical**: Per-fragment throttling prevents N-fragment put from bypassing the gate

**Location**: `src/transport_ofi.h:shmem_transport_ofi_put_large()`

### 6. Runtime Initialization

**NULL Pointer Checks**: `src/runtime-pmi.c` and `src/runtime-pmi2.c`
```c
if (NULL == location_array) {
    return 0;  // Safe early exit when !enable_node_ranks
}
```

Added to prevent segfault when dereferencing location_array in node rank computation.

## Build Warnings Fixed

### Unused Variables
- **collectives.c**: Removed `tree_parent_shr`, `my_up_slot` (kept `my_up_raw` - it's used!)
- **shmem_comm.h**: Removed `len` variable, compute inline: `shmem_internal_assert(type_size * count > 0);`

### Unused Functions
- **shmem_atomic.c**: Added `__attribute__((unused))` to `shmem_internal_cpu_atomic_load_long` (utility for future use)

### Static Inline Function Linkage
**Problem**: check_provider was static in .c but extern in .h, causing linkage conflict.

**Solution**: Move function definition to header as static inline:
```c
/* src/transport_ofi.h */
extern char *shmem_transport_ofi_prov_name;
static inline int
shmem_transport_ofi_check_provider(const char *name)
{
    return (shmem_transport_ofi_prov_name &&
            strcmp(shmem_transport_ofi_prov_name, name) == 0);
}
```

Remove function definition from .c file. Each compilation unit gets its own copy which the compiler can inline.

## Testing Infrastructure

**Location**: `modules/tests-sos` (submodule)
- Updated separately from main SOS changes
- User manages this submodule independently

## Perlmutter Configuration

**System**: NERSC Perlmutter (Cray EX, HPE Slingshot, CXI NICs)

**Configure Flags**:
```bash
./configure \
  --enable-ofi \
  --with-ofi-mr=scalable \
  --enable-mr-endpoint \
  --disable-enable-remote-virtual-addressing \
  --disable-nonfetch-amo \
  --enable-cxi-hybrid-mr-desc
```

**Key Settings**:
- Scalable MR mode (not basic)
- MR endpoint mode (not domain)
- No RVA (virtual address symmetry not guaranteed on Perlmutter)
- Disable non-fetch AMO (CXI provider workaround)
- Enable CXI hybrid MR descriptor optimization

## Recent Fixes (June 2026)

### Stack Overflow Fix (commit 6e3fbd4c)
- **Problem**: `alloca(sizeof(int) * PE_size)` in hierarchical barrier
- **Impact**: At 100K+ PEs, allocates 400KB+ on stack (typical limit: 1-8MB)
- **Solution**: Use `malloc(2 * sizeof(int) * PE_size)` for both PE arrays

### Compiler Warnings (commits e3bd0838, 6c77db46)
- Removed unused variables: tree_parent_shr, my_up_slot
- Kept my_up_raw (used for atomic stores!)
- Fixed shmem_comm.h unused len variable
- Added __attribute__((unused)) to cpu_atomic_load_long

### Build Errors (commit 3166280e)
- Fixed prov_name linkage: removed static from .c file
- Fixed check_provider: moved to static inline in header
- Fixed unused len variable in shmem_atomicv

### FI_HMEM Device Memory Ordering (commits 6e3fbd4c, a8368943)
- **Context**: Latent bug fix in `--enable-ofi-hmem` code path (disabled by default)
- **Initial fix**: Used put_quiet (correct)
- **Copilot refinement bug**: Tried to optimize with put_wait (incorrect - data race!)
- **Final fix**: Back to put_quiet with excellent comment explaining all 3 code paths
- **Lesson**: Review Copilot changes carefully - caught the bug before push
- **Note**: Does not affect standard builds; only relevant if FI_HMEM support is enabled

### Provider Name Handling (commit 38549f6a)
- strdup provider name to survive fi_freeinfo()
- Free in shmem_transport_fini()
- Gate hybrid MR setup on CXI provider check
- Change strncmp to strcmp for exact match

## Code Style & Conventions

- Single space after periods (not double space)
- No emojis unless explicitly requested
- Comments: only write when WHY is non-obvious (hidden constraint, subtle invariant, workaround)
- Don't explain WHAT the code does (well-named identifiers do that)
- Don't reference current task/PR in comments ("added for X flow" rots over time)
- **Git commits**: Do NOT add Co-Authored-By lines (organizational policy)

## Common Pitfalls

1. **Don't use alloca() for arrays that scale with PE count** - use malloc()
2. **Always check completion paths in put operations** - inject path has no counter event
3. **Gate CXI optimizations on provider check** - other providers need conservative paths
4. **strdup provider name** - pointer becomes invalid after fi_freeinfo()
5. **Verify my_up_raw is used before removing** - atomic stores need it!
6. **Check ALL exit paths for free()** - hierarchical barrier has 3 exit paths

## Branch Status (Current)

All branches have latest fixes:
- **performance**: 16 commits ahead of upstream_mainline/performance
- **bcm_test-FI_MR_LOCAL**: 2 commits ahead of origin (97ec5ec0, 23c16af4)
- **huge-page-support-clean**: 1 commit ahead of origin (16a23069)

Ready for testing on Perlmutter.
