LISPFLAV = clisp

noinst_PROGRAMS =
AM_DEFAULT_SOURCE_EXT = .lisp
OBJEXT = fas

noinst_PROGRAMS += thhcc

thhcc_SOURCES =
thhcc_SOURCES += package.lisp
thhcc_SOURCES += util.lisp
thhcc_SOURCES += time.lisp
thhcc_SOURCES += predef.lisp
thhcc_SOURCES += thhrule.lisp
thhcc_SOURCES += thhcc.lisp
thhcc_LINK = clisp thhcc

.lisp.o:
	$(MAKE) $*.fas

.lisp.fas:
	$(LISPFLAV) -c $< -o $@
