# SOS Performance Branch - Architecture Documentation

## Architectural Changes (June 2026 Session)

This document captures architectural decisions and patterns established during performance optimization work.

---

## 1. Provider Abstraction Layer

### Design Pattern: Static Inline Provider Checks

**Decision**: Centralize provider-specific optimizations behind a static inline check function.

**Implementation**:
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

**Rationale**:
- **Performance**: Static inline allows compiler to optimize away the check entirely in hot paths
- **Maintainability**: Single point of truth for provider checks, used consistently across codebase
- **Correctness**: `strcmp()` (exact match) prevents layered providers like "cxi;ofi_rxm" from incorrectly inheriting CXI-specific fast paths
- **Linkage**: Static inline in header means each compilation unit gets its own copy, avoiding ODR violations

**Usage Sites**:
1. `shmem_transport_fence()`: Skip put_quiet for CXI (per-EP FIFO ordering with FI_DELIVERY_COMPLETE)
2. `allocate_fabric_resources()`: Gate CXI hybrid MR descriptor setup
3. Future: Any provider-specific optimization paths

**Lifetime Management**:
- Provider name is `strdup()`'d at init to survive `fi_freeinfo()`
- Freed in `shmem_transport_fini()`
- Global visibility (non-static) allows access from inline functions in headers

---

## 2. Memory Management Strategy at Scale

### Design Pattern: Heap Allocation for Scale-Dependent Arrays

**Problem**: Stack allocations via `alloca()` cause stack overflow at large PE counts.

**Architectural Rule**: Any array whose size scales with PE count MUST use heap allocation.

**Example - Hierarchical Barrier**:
```c
/* BAD - stack overflow at 100K+ PEs */
int *local_pes = alloca(sizeof(int) * PE_size);
int *root_pes = alloca(sizeof(int) * PE_size);

/* GOOD - scales to millions of PEs */
int *pe_bufs = malloc(2 * sizeof(int) * PE_size);
if (!pe_bufs)
    RAISE_ERROR_STR("malloc failed for hierarchical barrier PE arrays");
int *local_pes = pe_bufs;
int *root_pes = pe_bufs + PE_size;
/* ... */
free(pe_bufs);
```

**Scale Analysis**:
- Typical stack limit: 1-8 MB (ulimit -s)
- At 100K PEs: 2 arrays × 4 bytes × 100,000 = 800 KB
- At 200K PEs: 1.6 MB
- At 500K PEs: 4 MB (exceeds many systems' default stack size)

**Design Rules**:
1. **Single allocation**: Allocate one contiguous buffer, partition with pointer arithmetic
2. **Error handling**: Check malloc() return value, abort with clear error message
3. **Cleanup**: Ensure ALL exit paths call `free()` (use goto cleanup pattern for complex functions)
4. **Documentation**: Comment WHY heap allocation is used ("PE_size can reach num_pes (100K+) at scale")

**Impact on Hierarchical Barrier**:
- 3 exit paths: degenerate case, post-local-sync, post-internode-sync
- All 3 must call `free(pe_bufs)` before return
- Prevents stack overflow that would manifest only at production scale

---

## 3. Heterogeneous Memory Ordering Architecture (FI_HMEM)

### Design Pattern: Conservative Ordering for Device Memory

**Context**: `USE_FI_HMEM` enables device-attached memory support (GPUs, accelerators) via libfabric HMEM. **Disabled by default** - only applies when built with `--enable-ofi-hmem`.

**Problem**: Self-copies (same PE) with device memory must guarantee visibility at target device.

**Architecture Decision**: Always use strongest ordering primitive (`put_quiet`) rather than per-operation ordering (`put_wait`).

**Code Path Analysis**:
```
shmem_internal_put_nb() routes through 3 implementations:

1. inject path (size <= max_buffered_send):
   - fi_inject_write() - no completion counter event
   - *completion remains 0
   - put_wait() early-exits immediately: if ((*completion) == 0) return;
   - Device write is UNORDERED → data race

2. bounce-buffer path (size <= bounce_buffer_size):
   - fi_writemsg() with bounce buffer
   - Sets *completion watermark
   - put_wait() polls until retired ≥ watermark
   - Correct ordering

3. put_large path (size > bounce_buffer_size):
   - Fragmented fi_write() loop
   - Sets *completion watermark
   - put_wait() polls until retired ≥ watermark
   - Correct ordering
```

**Architectural Choice**: Use `put_quiet` for correctness across all paths:
```c
static inline void
shmem_internal_copy_self(void *dest, const void *source, size_t nelems)
{
#ifdef USE_FI_HMEM
    long completion = 0;
    shmem_internal_put_nb(SHMEM_CTX_DEFAULT, dest, source, nelems,
                          shmem_internal_my_pe, &completion);
    shmem_transport_put_quiet((shmem_transport_ctx_t *)SHMEM_CTX_DEFAULT);
#else
    memcpy(dest, source, nelems);
#endif
}
```

**Rationale**:
- **Correctness over performance**: `put_quiet` is ~100-500ns slower but guarantees correctness
- **Path-independent**: Works correctly regardless of which internal path put_nb chooses
- **NIC-level fence**: Drains all pending puts and provides memory barrier at NIC
- **Future-proof**: Remains correct if put_nb routing logic changes

**Lesson Learned**: 
- Initial implementation used `put_quiet` (correct)
- Optimization attempt used `put_wait` (incorrect - subtle data race)
- Final implementation reverted to `put_quiet` with comprehensive comment
- **Architectural principle**: For correctness-critical operations (device memory ordering, cross-device coherency), use strongest available ordering primitive

**Impact**: This fix only affects builds with `--enable-ofi-hmem`. Standard builds (including Perlmutter configuration) use simple `memcpy()` path and are unaffected.

---

## 4. CXI Fast Path Architecture

### Design Pattern: Provider-Specific Fast Paths with Conservative Fallback

**Architectural Principle**: Optimize for known-good providers; remain correct for unknown/generic providers.

**Example - Fence Operation**:
```c
static inline int
shmem_transport_fence(shmem_transport_ctx_t* ctx)
{
#if WANT_TOTAL_DATA_ORDERING == 0
    if (!shmem_transport_ofi_check_provider("cxi")) {
        shmem_transport_put_quiet(ctx);  // Conservative path
    }
    // CXI: Skip put_quiet (FI_DELIVERY_COMPLETE + per-EP FIFO = implicit ordering)
#endif
    shmem_transport_get_wait(ctx);
    return 0;
}
```

**Provider Semantics**:
- **CXI**: FI_DELIVERY_COMPLETE + per-EP FIFO ordering guarantees remote visibility
- **Generic OFI**: No such guarantee; must explicitly `put_quiet` to ensure visibility

**Performance Impact**:
- CXI fast path: ~1-2µs latency improvement per fence
- Generic path: Correct but slower (polls NIC completion queue)

**Extensibility Pattern**:
```c
if (shmem_transport_ofi_check_provider("cxi")) {
    /* CXI-specific fast path */
} else if (shmem_transport_ofi_check_provider("verbs")) {
    /* Verbs-specific optimization */
} else {
    /* Conservative fallback */
}
```

---

## 5. CXI Hybrid MR Descriptor Mode

### Design Pattern: Vendor-Specific API Extensions

**Feature**: CXI provider extension to skip internal MR registration when descriptor is provided.

**Architecture**:
```c
if (shmem_transport_ofi_check_provider("cxi")) {
    if (shmem_internal_params.OFI_CXI_HYBRID_MR_DESC) {
        struct cxi_dom_ops_v3_local {
            int (*cntr_read)(...);
            int (*topology)(...);
            int (*enable_hybrid_mr_desc)(struct fid *, bool);
        } *cxi_dom_ops = NULL;
        
        int ret = fi_open_ops(&shmem_transport_ofi_domainfd->fid,
                             "dom_ops_v3", 0, (void **)&cxi_dom_ops, NULL);
        if (ret == 0 && cxi_dom_ops && cxi_dom_ops->enable_hybrid_mr_desc) {
            ret = cxi_dom_ops->enable_hybrid_mr_desc(&domain->fid, true);
        }
    }
}
```

**Architectural Constraints**:
1. **Provider Gating**: Only attempt `fi_open_ops()` for CXI provider (avoids spurious warnings)
2. **Timing**: Must be called BEFORE endpoint creation (setting propagates to child EPs only at creation)
3. **Graceful Degradation**: If API unavailable, fall back to standard MR path
4. **User Control**: Env var `SHMEM_OFI_CXI_HYBRID_MR_DESC` allows opt-out

**Performance Impact**:
- Eliminates per-call MR cache lookup in fi_write/fi_writemsg
- Critical for high message rate workloads (millions of ops/sec)

**Future Extensions**:
- CXI topology API via `cxi_dom_ops->topology()` for NUMA-aware placement
- CXI counter read API via `cxi_dom_ops->cntr_read()` for low-latency polling

---

## 6. Pipeline Depth Control Architecture

### Design Pattern: Backpressure via Counter Watermarks

**Problem**: At high PPN (128+), concurrent puts can exhaust NIC transaction resources (TRS), causing `-FI_EAGAIN` and potential deadlock.

**Architectural Solution**: Throttle before each operation when pending operations exceed threshold.

**Implementation**:
```c
static inline void
shmem_transport_ofi_put_pipeline_throttle(shmem_transport_ctx_t *ctx)
{
    if (shmem_transport_ofi_put_pipeline_depth > 0) {
        uint64_t pending = SHMEM_TRANSPORT_OFI_CNTR_READ(&ctx->pending_put_cntr);
        uint64_t retired = fi_cntr_read(ctx->put_cntr);
        
        if (pending - retired >= (uint64_t)shmem_transport_ofi_put_pipeline_depth) {
            /* Backpressure: wait for some puts to retire */
            shmem_transport_put_wait(ctx, &pending);
        }
    }
}
```

**Critical Design Detail - Per-Fragment Throttling**:
```c
void shmem_transport_ofi_put_large(...)
{
    SHMEM_TRANSPORT_OFI_CTX_LOCK(ctx);
    while (frag_source < end) {
        /* THROTTLE BEFORE EACH FRAGMENT - not just once before loop */
        shmem_transport_ofi_put_pipeline_throttle(ctx);
        
        SHMEM_TRANSPORT_OFI_CNTR_INC(&ctx->pending_put_cntr);
        fi_write(...);
        
        frag_source += frag_len;
    }
    /* Capture watermark after all fragments issued */
    if (completion)
        *completion = SHMEM_TRANSPORT_OFI_CNTR_READ(&ctx->pending_put_cntr);
    SHMEM_TRANSPORT_OFI_CTX_UNLOCK(ctx);
}
```

**Why Per-Fragment Matters**:
- **Wrong**: Throttle once before loop → N-fragment put issues all N fi_write() calls, bypassing TRS limit entirely
- **Right**: Throttle before each fragment → each fragment individually passes through the gate
- **Scale Impact**: At 128 PPN with 8 MB puts (256 fragments), wrong placement allows 256× TRS oversubscription

**Tuning Guidance**:
- Default: 0 (unlimited) for backward compatibility
- Recommended for high PPN: 512 (Cray SHMEM default)
- Symptom: `-FI_EAGAIN` storms in traces → enable pipeline depth limit
- Too low: Unnecessary serialization, reduced bandwidth
- Too high: Still hit TRS exhaustion

---

## 7. Huge Page Allocation Strategy

### Design Pattern: Graduated Fallback with Virtual Address Hints

**Architecture**: Multi-tier allocation strategy prioritizing huge pages while maintaining correctness.

**Allocation Tiers**:
```
1. hugetlbfs file (if SHMEM_SYMMETRIC_HEAP_USE_HUGE_PAGES)
   ├─ Find mounted hugetlbfs with requested page size
   ├─ Create file, ftruncate to rounded size
   ├─ mmap(requested_base, ..., MAP_SHARED | MAP_HUGETLB, fd, 0)
   └─ On failure → tier 2

2. Anonymous MAP_HUGETLB with explicit page size
   ├─ mmap(requested_base, ..., MAP_ANON | MAP_HUGETLB | (21 << MAP_HUGE_SHIFT), -1, 0)
   ├─ 21 << MAP_HUGE_SHIFT forces 2MB pages (2^21 bytes)
   └─ On failure → tier 3

3. Transparent Huge Pages (THP) via madvise
   ├─ mmap(requested_base, ..., MAP_ANON | MAP_PRIVATE, -1, 0)
   ├─ madvise(ptr, size, MADV_HUGEPAGE)
   └─ Kernel promotes to huge pages opportunistically

4. Regular pages (final fallback)
   └─ mmap(NULL, ..., MAP_ANON | MAP_PRIVATE, -1, 0)
```

**Virtual Address Symmetry Constraints**:
```c
void *requested_base = 
    (void*) (((unsigned long) shmem_internal_data_base + 
              shmem_internal_data_length + 2 * ONEGIG) & ~(ONEGIG - 1));
```

**Design Rationale**:
- **Why requested_base**: RVA (Remote Virtual Addressing) requires symmetric virtual addresses across PEs
- **Fallback to NULL**: Only as last resort; breaks RVA but allows job to proceed
- **Alignment**: 1GB boundary prevents TLB thrashing and improves performance
- **Rounding**: hugetlbfs requires size aligned to page boundary; track actual mapped size for munmap

**Size Tracking Architecture**:
```c
size_t mapped_length = 0;
void *base = mmap_alloc(requested_size, &mapped_length);
shmem_internal_heap_length = mapped_length;  // May be > requested_size
```

This ensures `munmap(base, shmem_internal_heap_length)` uses the correct size.

---

## Summary of Architectural Principles

1. **Provider Abstraction**: Centralize provider checks; optimize for known providers, remain correct for generic
2. **Scale-Aware Memory**: Use heap allocation for arrays that scale with PE count
3. **Conservative Ordering**: Use strongest ordering primitive for heterogeneous memory (GPU)
4. **Backpressure Control**: Throttle before each operation, not just at function entry
5. **Graduated Fallback**: Prefer optimal path, but degrade gracefully through multiple tiers
6. **Virtual Address Symmetry**: Preserve when possible (RVA), but allow fallback for robustness
7. **Vendor Extensions**: Gate on provider check; handle unavailability gracefully

These principles emerged from production-scale testing at 100K+ PEs on Cray EX systems with HPE Slingshot interconnect.
