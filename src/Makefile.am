LISPFLAV = clisp

noinst_PROGRAMS =
AM_DEFAULT_SOURCE_EXT = .lisp
LINK = echo

noinst_PROGRAMS += test

.lisp.o:
	$(LISPFLAV) < $<
