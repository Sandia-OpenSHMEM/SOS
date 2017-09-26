/* SOS_ENV( name, kind, default, category, short description )
 *
 * Kinds: long, size, bool, string
 * Categories: openshmem, other, collectives, intranode, transport
 */
SHMEM_INTERNAL_ENV_DEF(INFO, bool, false, SHMEM_INTERNAL_ENV_CAT_OPENSHMEM,
                       "Print library information message at startup")
SHMEM_INTERNAL_ENV_DEF(VERSION, bool, false, SHMEM_INTERNAL_ENV_CAT_OPENSHMEM,
                       "Print library version at startup")
SHMEM_INTERNAL_ENV_DEF(DEBUG, bool, false, SHMEM_INTERNAL_ENV_CAT_OPENSHMEM,
                       "Enable debugging messages")
SHMEM_INTERNAL_ENV_DEF(SYMMETRIC_SIZE, size, 512*1024*1024, SHMEM_INTERNAL_ENV_CAT_OPENSHMEM,
                       "Symmetric heap size")

#ifdef __linux__
SHMEM_INTERNAL_ENV_DEF(SYMMETRIC_HEAP_USE_HUGE_PAGES, bool, false, SHMEM_INTERNAL_ENV_CAT_OTHER,
                       "Use Linux huge pages for symmetric heap")
SHMEM_INTERNAL_ENV_DEF(SYMMETRIC_HEAP_PAGE_SIZE, size, 2*1024*1024, SHMEM_INTERNAL_ENV_CAT_OTHER,
                       "Page size to use for huge pages")
#endif

SHMEM_INTERNAL_ENV_DEF(SYMMETRIC_HEAP_USE_MALLOC, bool, false, SHMEM_INTERNAL_ENV_CAT_OTHER,
                        "Allocate the symmetric heap using malloc")
SHMEM_INTERNAL_ENV_DEF(BOUNCE_SIZE, size, DEFAULT_BOUNCE_SIZE, SHMEM_INTERNAL_ENV_CAT_OTHER,
                       "Maximum message size to bounce buffer")
SHMEM_INTERNAL_ENV_DEF(TRAP_ON_ABORT, bool, false, SHMEM_INTERNAL_ENV_CAT_OTHER,
                       "Generate trap if the program aborts or calls shmem_global_exit")

SHMEM_INTERNAL_ENV_DEF(COLL_CROSSOVER, long, 4, SHMEM_INTERNAL_ENV_CAT_COLLECTIVES,
                       "Crossover between linear and tree collectives")
SHMEM_INTERNAL_ENV_DEF(COLL_RADIX, long, 4, SHMEM_INTERNAL_ENV_CAT_COLLECTIVES,
                       "Radix for tree-based collectives")
SHMEM_INTERNAL_ENV_DEF(BARRIER_ALGORITHM, string, "auto", SHMEM_INTERNAL_ENV_CAT_COLLECTIVES,
                       "Algorithm for barrier.  Options are auto, linear, tree, dissem")
SHMEM_INTERNAL_ENV_DEF(BCAST_ALGORITHM, string, "auto", SHMEM_INTERNAL_ENV_CAT_COLLECTIVES,
                       "Algorithm for broadcast.  Options are auto, linear, tree")
SHMEM_INTERNAL_ENV_DEF(REDUCE_ALGORITHM, string, "auto", SHMEM_INTERNAL_ENV_CAT_COLLECTIVES,
                       "Algorithm for reductions.  Options are auto, linear, tree, recdbl")
SHMEM_INTERNAL_ENV_DEF(COLLECT_ALGORITHM, string, "auto", SHMEM_INTERNAL_ENV_CAT_COLLECTIVES,
                       "Algorithm for collect.  Options are auto, linear")
SHMEM_INTERNAL_ENV_DEF(FCOLLECT_ALGORITHM, string, "auto", SHMEM_INTERNAL_ENV_CAT_COLLECTIVES,
                       "Algorithm for fcollect.  Options are auto, linear, ring, recdbl")

#ifdef USE_CMA
SHMEM_INTERNAL_ENV_DEF(CMA_PUT_MAX, size, 8*1024, SHMEM_INTERNAL_ENV_CAT_INTRANODE,
                       "Size below which to use CMA for puts")
SHMEM_INTERNAL_ENV_DEF(CMA_GET_MAX, size, 16*1024, SHMEM_INTERNAL_ENV_CAT_INTRANODE,
                       "Size below which to use CMA for gets")
#endif /* USE_CMA */

#ifdef USE_OFI
SHMEM_INTERNAL_ENV_DEF(OFI_ATOMIC_CHECKS_WARN, bool, false, SHMEM_INTERNAL_ENV_CAT_TRANSPORT,
                       "Display warnings about unsupported atomic operations")
SHMEM_INTERNAL_ENV_DEF(OFI_PROVIDER, string, "auto", SHMEM_INTERNAL_ENV_CAT_TRANSPORT,
                       "Provider that should be used by the OFI transport")
SHMEM_INTERNAL_ENV_DEF(OFI_USE_PROVIDER, string, "auto", SHMEM_INTERNAL_ENV_CAT_TRANSPORT,
                       "Deprecated, replaced by SHMEM_OFI_PROVIDER")
SHMEM_INTERNAL_ENV_DEF(OFI_FABRIC, string, "auto", SHMEM_INTERNAL_ENV_CAT_TRANSPORT,
                       "Fabric that should be used by the OFI transport")
SHMEM_INTERNAL_ENV_DEF(OFI_DOMAIN, string, "auto", SHMEM_INTERNAL_ENV_CAT_TRANSPORT,
                       "Fabric domain that should be used by the OFI transport")
#endif
