LISPFLAV = clisp

noinst_PROGRAMS =
AM_DEFAULT_SOURCE_EXT = .lisp

noinst_PROGRAMS += test
noinst_PROGRAMS += thhcc

test_LINK = echo

thhcc_SOURCES =
thhcc_SOURCES += thhcc.lisp
thhcc_LINK = echo

.lisp.o:
	$(LISPFLAV) < $<
