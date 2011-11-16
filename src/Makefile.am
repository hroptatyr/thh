SBCL = sbcl
CLISP = clisp
CMUCL = lisp

SBCL_DUMP = (sb-ext:save-lisp-and-die ".bin" :executable t)
CLISP_DUMP = (ext:saveinitmem ".bin" :executable t :norc t :quiet t)
CMUCL_DUMP = (save-lisp ".bin" :executable t :load-init-file nil)

noinst_PROGRAMS =

noinst_PROGRAMS += test


%: %.lisp
#	$(SBCL) --load $< --eval '$(SBCL_DUMP)' && mv ".bin" $@
	(cat $<; echo '$(CLISP_DUMP)') | $(CLISP) && mv ".bin" $@
#	$(CMUCL) -load $< -eval '$(CMUCL_DUMP)' && mv ".bin" $@
