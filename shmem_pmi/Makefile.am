
AM_CPPFLAGS= -I$(top_srcdir)/shmem_pmi

lib_LTLIBRARIES = libshmem_pmi.la
libshmem_pmi_la_SOURCES = \
	simple_pmi.c \
	simple_pmiutil.c \
	simple_pmiutil.h \
	pmi.h

install-exec-hook:
	$(MKDIR_P) ../install/lib
	$(LN_S) $(DESTDIR)$(libdir)/libshmem_pmi.so ../install/lib/libshmem_pmi.so
	$(LN_S) $(DESTDIR)$(libdir)/libshmem_pmi.so.0 ../install/lib/libshmem_pmi.so.0
