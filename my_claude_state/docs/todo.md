# TODO List

## Immediate (This Week)

### 1. Test on Perlmutter ⚠️ HIGH PRIORITY
**Why**: Validate all recent fixes work at scale on production hardware

**Steps**:
```bash
# 1. Push performance branch (or test locally first)
cd /mnt/scratch/bcmorgan/SOS_MyFork
git checkout performance
# Optionally: git push origin performance

# 2. SSH to Perlmutter
ssh perlmutter

# 3. Clone/pull latest
cd $SCRATCH
git clone <fork-url> SOS_MyFork  # or: cd SOS_MyFork && git pull

# 4. Load modules
module load PrgEnv-gnu
module load libfabric

# 5. Configure
./autogen.sh
./configure \
  --enable-ofi \
  --with-ofi-mr=scalable \
  --enable-mr-endpoint \
  --disable-enable-remote-virtual-addressing \
  --disable-nonfetch-amo \
  --enable-cxi-hybrid-mr-desc \
  --prefix=$SCRATCH/sos-install

# 6. Build
make -j16 2>&1 | tee build.log
# Check for warnings - should be none!

# 7. Install
make install

# 8. Run tests
# TODO: Define specific test suite to run
```

**What to Look For**:
- ✅ Clean build (no warnings)
- ✅ CXI hybrid MR mode enabled (check startup message)
- ✅ No stack overflow at high PE counts
- ✅ Barrier correctness at all scales
- ✅ Performance improvements vs baseline

---

### 2. Push Parent Branches ⚠️ AFTER Perlmutter validation
**Why**: Share fixes with upstream, prevent conflicts

**Branches to Push**:
```bash
# bcm_test-FI_MR_LOCAL (2 commits ahead)
git checkout bcm_test-FI_MR_LOCAL
git log origin/bcm_test-FI_MR_LOCAL..HEAD --oneline  # Verify commits
git push origin bcm_test-FI_MR_LOCAL

# huge-page-support-clean (1 commit ahead)
git checkout huge-page-support-clean
git log origin/huge-page-support-clean..HEAD --oneline  # Verify commits
git push origin huge-page-support-clean

# performance (16 commits ahead - coordinate with team)
git checkout performance
# May want to discuss with team before pushing
git push origin performance
```

**Pre-push Checklist**:
- [ ] Perlmutter tests pass
- [ ] All builds clean (no warnings)
- [ ] Commit messages accurate
- [ ] No sensitive data in commits
- [ ] Co-author tags correct (GitHub Copilot credits)

---

### 3. Test Mark's Commits in Isolation
**Why**: Determine if they conflict with hybrid MR work or break independently

**Method**:
1. Create test branch from clean performance base (before hybrid MR)
2. Cherry-pick Mark's commits
3. Build and test
4. Compare results with hybrid MR + Mark's commits

**If They Break**:
- Document the failure mode
- Check if it's a known issue
- Consider reporting upstream

**If They Work**:
- Issue is interaction between Mark's work and hybrid MR
- Need deeper investigation of overlapping code paths

---

## Short Term (This Month)

### 4. Performance Characterization
**Goal**: Quantify performance improvements from recent work

**Benchmarks to Run**:
- OSU microbenchmarks (latency, bandwidth, barrier)
- Application proxies (if available)
- Scaling studies: 4, 16, 64, 256, 1024+ nodes

**Metrics**:
- Barrier latency (hierarchical vs dissemination vs linear)
- Put/get bandwidth and latency
- Fence overhead (CXI fast path vs generic)
- Memory footprint (huge pages vs regular)

**Compare**:
- performance branch vs upstream main
- CXI optimizations enabled vs disabled
- Huge pages vs regular pages
- Pipeline depth limit impact at high PPN

---

### 5. Documentation Updates
**Status**: In progress (this session)

**Completed**:
- [x] CLAUDE.md - comprehensive technical guide
- [x] docs/architecture.md - architectural decisions
- [x] docs/open-issues.md - current issues and status
- [x] docs/todo.md - this file

**Still Needed**:
- [ ] Update top-level README.md if substantial features added
- [ ] Document environment variables (collect all SHMEM_* vars)
- [ ] Create troubleshooting guide (common errors and fixes)
- [ ] Performance tuning guide (when to use each optimization)

---

### 6. Code Review with Team
**Why**: Get second opinion on architectural changes

**Topics to Review**:
1. Provider abstraction pattern (static inline check function)
2. Heap allocation strategy for scale-dependent arrays
3. GPU memory ordering approach (put_quiet always)
4. CXI fast path gating
5. Pipeline depth control implementation

**Format**: Could be PR review, design doc, or technical presentation

---

## Medium Term (Next Quarter)

### 7. Explore Additional CXI Optimizations
**Ideas**:
- CXI topology API for NUMA-aware PE placement
- CXI direct counter read for lower latency
- CXI-specific collective algorithms
- Investigate CXI triggered operations

**Research**:
- Review CXI provider documentation
- Consult with HPE Slingshot team
- Profile to identify hotspots
- Prototype optimizations in feature branch

---

### 8. Hierarchical Barrier Tuning
**Current**: Auto-select when local_count >= HIER_BARRIER_THRESHOLD (default: 2)

**Tuning Opportunities**:
- Optimize threshold for different node counts
- Experiment with different dissemination patterns
- Consider hybrid algorithms (tree + dissemination)
- Cache line padding analysis (HIER_SLOT_STRIDE)

**Measurement**:
- Barrier latency vs node count
- Scalability beyond 1000 nodes
- Impact of local PE count on performance
- Comparison with Cray SHMEM barrier

---

### 9. Memory Registration Improvements
**Context**: MR (Memory Registration) cache is critical for performance

**Investigate**:
- Optimal MR cache size for 100K+ PEs
- Hybrid MR mode impact on cache hit rate
- Registration overhead for GPU memory
- Scalable MR mode vs endpoint mode tradeoffs

**Metrics**:
- ATU cache hit rate (from telemetry)
- MR cache hit rate (libfabric stats)
- Registration latency for hot paths
- Memory footprint of MR cache

---

### 10. Upstream Contributions
**Goal**: Contribute improvements back to Sandia OpenSHMEM

**Candidates for Upstream**:
- Stack overflow fix (universally applicable)
- NULL pointer checks (safety improvement)
- Provider abstraction pattern (enables vendor optimizations)
- Huge page allocation strategy (benefits all Linux systems)

**Process**:
1. Verify fixes work across multiple systems
2. Ensure no Perlmutter-specific assumptions
3. Write comprehensive commit messages
4. Submit PR to Sandia OpenSHMEM GitHub
5. Address review feedback

**Blockers**:
- Some changes are CXI-specific (may not be upstream interest)
- Need to verify license compatibility
- Must ensure changes don't regress other platforms

---

## Long Term (Future)

### 11. GPU Direct Support
**Status**: FI_HMEM path exists but needs optimization

**Investigate**:
- GPU memory registration strategy
- Optimal buffer sizes for GPU transfers
- Kernel launch latency impact
- Multi-GPU per node scenarios

---

### 12. Fault Tolerance
**Context**: At 100K+ PEs, node failures are expected

**Research**:
- Checkpoint/restart integration
- User-Level Fault Mitigation (ULFM) for MPI
- Resilient collectives
- PE failure detection and recovery

---

### 13. Alternative Interconnects
**Current**: Optimized for HPE Slingshot (CXI)

**Expand to**:
- InfiniBand (verbs provider)
- Ethernet (tcp/udp providers)
- Omni-Path (psm2 provider)

**Goal**: Portable performance across HPC systems

---

## Maintenance

### Ongoing Tasks
- [ ] Monitor upstream SOS for relevant changes
- [ ] Track libfabric releases for CXI improvements
- [ ] Update documentation as code evolves
- [ ] Keep test suite current
- [ ] Review and clean up stale branches
- [ ] Update memory notes with new learnings

### Technical Debt
- None currently identified (clean slate after recent fixes)

### Future Deprecations to Watch
- Older libfabric APIs
- PMI-1 vs PMI-2 vs PMIx
- Legacy collective algorithms

---

## Completed ✅

### June 11, 2026 Session
- [x] Fix stack overflow in hierarchical barrier (malloc vs alloca)
- [x] Fix GPU memory ordering (put_quiet in copy_self)
- [x] Fix all compiler warnings (unused variables, unused functions)
- [x] Fix build errors (static inline linkage, prov_name lifecycle)
- [x] Improve provider name handling (strdup, strcmp)
- [x] Gate CXI hybrid MR on provider check
- [x] Update parent branches (bcm_test-FI_MR_LOCAL, huge-page-support-clean)
- [x] Document all architectural changes
- [x] Create comprehensive CLAUDE.md
- [x] Document open issues and TODOs

---

## Notes

**Priority Legend**:
- ⚠️ HIGH: Blocking or time-sensitive
- MEDIUM: Important but not urgent
- LOW: Nice to have

**Branch Status** (as of June 11, 2026):
- performance: Ready for Perlmutter testing
- bcm_test-FI_MR_LOCAL: Ready for push after validation
- huge-page-support-clean: Ready for push after validation

**Next Session Goals**:
1. Report Perlmutter test results
2. Push validated branches
3. Begin performance characterization work
