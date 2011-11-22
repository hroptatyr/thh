LISPFLAV = clisp

noinst_PROGRAMS =
AM_DEFAULT_SOURCE_EXT = .lisp

noinst_PROGRAMS += thhcc

thhcc_SOURCES =
thhcc_SOURCES += thhcc.lisp
thhcc_LINK = echo

.lisp.o:
	srcdir=$(srcdir) $(LISPFLAV) < $<
