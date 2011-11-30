LISPFLAV = clisp

bin_PROGRAMS =
libexec_PROGRAMS =
BUILT_SOURCES =
AM_DEFAULT_SOURCE_EXT = .lisp
CL_COMPILE = $(top_srcdir)/cl_compile.sh $(LISPFLAV)
CL_LINK = $(top_srcdir)/cl_link.sh $(LISPFLAV)
AM_CPPFLAGS = -D_GNU_SOURCE -D_POSIX_C_SOURCE=201001L -D__USE_XOPEN2K8
AM_LDFLAGS =

libexec_PROGRAMS += thhcc.bin
thhcc_bin_SOURCES =
thhcc_bin_SOURCES += package.lisp
thhcc_bin_SOURCES += util.lisp
thhcc_bin_SOURCES += time.lisp
thhcc_bin_SOURCES += predef.lisp
thhcc_bin_SOURCES += timezone.lisp
thhcc_bin_SOURCES += copy-instance.lisp
thhcc_bin_SOURCES += thhrule.lisp
thhcc_bin_SOURCES += thhcc.lisp
thhcc_bin_LINK = $(CL_LINK)

bin_PROGRAMS += thhcc
thhcc_SOURCES = thhcc-wrap.c thhcc-clo.ggo
thhcc_CPPFLAGS = $(AM_CPPFLAGS) -DLIBEXECDIR=\"$(libexecdir)\"
thhcc_LDFLAGS = $(AM_LDFLAGS)
BUILT_SOURCES += thhcc-clo.c thhcc-clo.h

util.fas: package.lisp
time.fas: package.lisp
timezone.fas: package.lisp
copy-instance.fas: package.lisp
predef.fas: package.lisp util.lisp
thhrule.fas: package.lisp util.lisp time.lisp timezone.lisp copy-instance.lisp
thhcc.fas: package.lisp thhrule.lisp predef.lisp

.lisp.o:
	$(CL_COMPILE) $< $@

## ggo rule
%.c %.h: %.ggo
	cat $< | gengetopt -l -F $*
