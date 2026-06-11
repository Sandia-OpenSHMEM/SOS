# Open Issues

## Active Issues

### 1. Perlmutter Testing Required
**Status**: Not yet tested  
**Priority**: HIGH  
**Description**: All recent fixes need validation on Perlmutter (Cray EX, HPE Slingshot, CXI NICs)

**Changes to Validate**:
- Stack overflow fix (malloc vs alloca in hierarchical barrier)
- Compiler warning fixes (removed unused variables)
- Provider name handling (strdup, strcmp)
- FI_HMEM device memory ordering fix (put_quiet in copy_self, disabled by default)
- CXI hybrid MR descriptor mode improvements

**Test Plan**:
1. Configure with Perlmutter flags (see CLAUDE.md)
2. Build and verify no warnings
3. Run at multiple scales:
   - Small (4 nodes, 104 PPN)
   - Medium (64 nodes, 104 PPN)
   - Large (1000+ nodes, 104 PPN) if possible
4. Check for:
   - No stack overflow errors
   - Correct barrier functionality
   - No device memory ordering issues (if testing with --enable-ofi-hmem, not in standard config)
   - CXI hybrid MR mode enabled (check startup message)

**Branches to Test**:
- performance (primary integration branch)
- bcm_test-FI_MR_LOCAL (CXI-specific features)
- huge-page-support-clean (huge page support only)

---

### 2. Mark's Commits - Isolation Testing
**Status**: Unknown if conflicts exist  
**Priority**: MEDIUM  
**Description**: Need to verify if Mark's performance commits break on clean performance branch or only conflict with hybrid MR work.

**Context**: From memory note:
> Test Mark's commits in isolation - verify if Mark's perf commits break on clean performance branch or only conflict with hybrid MR work

**Action Items**:
1. Identify which commits are "Mark's commits"
2. Apply them to clean performance branch (without hybrid MR)
3. Test for breakage
4. If they work in isolation, investigate interaction with hybrid MR work
5. If they break independently, report upstream

**Investigation Questions**:
- Which specific commits are Mark's?
- What performance optimization do they implement?
- Do they touch the same code paths as hybrid MR work?

---

### 3. Branch Sync Status
**Status**: Local changes ahead of remotes  
**Priority**: MEDIUM  
**Description**: Parent branches have unpushed commits with critical fixes

**Current State**:
- **bcm_test-FI_MR_LOCAL**: 2 commits ahead of origin
  - 97ec5ec0: Fix compiler warnings
  - 23c16af4: Improve provider name handling and hybrid MR gating
- **huge-page-support-clean**: 1 commit ahead of origin
  - 16a23069: Fix compiler warnings
- **performance**: 16 commits ahead of upstream_mainline/performance

**Risk**: If not pushed soon, potential conflicts if others modify these branches.

**Action**: Push after Perlmutter validation confirms fixes work correctly.

---

## Resolved Issues

### ✅ Stack Overflow in Hierarchical Barrier (June 11, 2026)
**Resolution**: Replaced alloca() with malloc() for PE arrays  
**Commits**: 6e3fbd4c, 6c77db46  
**Impact**: Prevents stack overflow at 100K+ PEs

### ✅ FI_HMEM Device Memory Ordering Data Race (June 11, 2026)
**Resolution**: Use put_quiet (not put_wait) in copy_self  
**Commits**: 6e3fbd4c, a8368943 (now 78bba900, 8b6afe3f after rebase)  
**Context**: Latent bug fix in --enable-ofi-hmem code path (disabled by default)  
**Impact**: Guarantees visibility for device self-copies across all code paths (only when FI_HMEM enabled)  
**Lesson**: Copilot initially got it right, tried to optimize incorrectly, then corrected. Always review carefully.  
**Note**: Does not affect standard Perlmutter builds which use memcpy() path.

### ✅ Build Warnings and Errors (June 11, 2026)
**Resolution**: Removed unused variables, fixed static inline linkage  
**Commits**: 3166280e, e3bd0838, 6c77db46  
**Impact**: Clean build with no warnings

### ✅ Provider Name Lifecycle Bug (June 11, 2026)
**Resolution**: strdup at init, free at fini  
**Commit**: 38549f6a  
**Impact**: Pointer remains valid after fi_freeinfo()

### ✅ CXI Hybrid MR Spurious Warnings (June 11, 2026)
**Resolution**: Gate fi_open_ops() call on CXI provider check  
**Commit**: 38549f6a  
**Impact**: Non-CXI providers don't see confusing startup warnings

---

## Known Limitations

### 1. RVA Disabled on Perlmutter
**Status**: By design (not an issue)  
**Reason**: Perlmutter doesn't guarantee virtual address symmetry  
**Impact**: Must use mr-endpoint mode instead of mr-scalable-rva  
**Configuration**: `--disable-enable-remote-virtual-addressing`

### 2. Non-Fetch AMO Disabled
**Status**: By design (workaround for CXI provider)  
**Reason**: Known issue with CXI provider and non-fetching atomics  
**Impact**: All AMOs converted to fetch-AMO internally  
**Configuration**: `--disable-nonfetch-amo`  
**Future**: May be fixed in later libfabric/CXI versions

### 3. Single Space After Period
**Status**: User style preference  
**Reason**: User feedback - "double space makes me look old"  
**Impact**: All generated text and comments use single space after periods

---

## Monitoring

### Metrics to Watch
1. **Stack usage at scale**: Should remain under 1MB even at 500K PEs
2. **TRS exhaustion**: Monitor for `-FI_EAGAIN` with pipeline depth limit enabled
3. **ATU cache hit rate**: Should be >80% (check via telemetry)
4. **Fence latency**: CXI fast path should save 1-2µs vs generic path
5. **Huge page allocation**: Check startup messages for successful huge page allocation

### Telemetry Counters (CXI-specific)
- `ixe_disp_lpe_amos_ok`: Should match total AMO count (no failed AMOs)
- `pct_retry_trs_put`: Should be low/zero (indicates resource pressure)
- `atu_cache_hit` / `atu_cache_miss`: Hit rate >80% desired
- `hni_sts_tx_ok_octets`: Total TX traffic
- `hni_sts_rx_ok_octets`: Total RX traffic

Use `/analyze-telemetry` skill with delta files for detailed analysis.
