LISPFLAV = clisp

bin_PROGRAMS =
AM_DEFAULT_SOURCE_EXT = .lisp
OBJEXT = fas
CL_COMPILE = $(top_srcdir)/cl_compile.sh $(LISPFLAV)
CL_LINK = $(top_srcdir)/cl_link.sh $(LISPFLAV)

bin_PROGRAMS += thhcc

thhcc_SOURCES =
thhcc_SOURCES += package.lisp
thhcc_SOURCES += util.lisp
thhcc_SOURCES += time.lisp
thhcc_SOURCES += predef.lisp
thhcc_SOURCES += copy-instance.lisp
thhcc_SOURCES += thhrule.lisp
thhcc_SOURCES += thhcc.lisp
thhcc_LINK = $(CL_LINK) thhcc

.lisp.o:
	$(MAKE) $*.fas

.lisp.fas:
	$(CL_COMPILE) $< $@

