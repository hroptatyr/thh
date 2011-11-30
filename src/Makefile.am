LISPFLAV = clisp

bin_PROGRAMS =
BUILT_SOURCES =
AM_DEFAULT_SOURCE_EXT = .lisp
CL_COMPILE = $(top_srcdir)/cl_compile.sh $(LISPFLAV)
CL_LINK = $(top_srcdir)/cl_link.sh $(LISPFLAV)

bin_PROGRAMS += thhcc
thhcc_SOURCES =
thhcc_SOURCES += package.lisp
thhcc_SOURCES += util.lisp
thhcc_SOURCES += time.lisp
thhcc_SOURCES += predef.lisp
thhcc_SOURCES += timezone.lisp
thhcc_SOURCES += copy-instance.lisp
thhcc_SOURCES += thhrule.lisp
thhcc_SOURCES += thhcc.lisp
thhcc_LINK = $(CL_LINK) thhcc

util.fas: package.lisp
time.fas: package.lisp
timezone.fas: package.lisp
copy-instance.fas: package.lisp
predef.fas: package.lisp util.lisp
thhrule.fas: package.lisp util.lisp time.lisp timezone.lisp copy-instance.lisp
thhcc.fas: package.lisp thhrule.lisp predef.lisp

.lisp.o:
	$(CL_COMPILE) $< $@

