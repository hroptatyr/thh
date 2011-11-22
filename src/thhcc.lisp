;; (require "asdf")
;; (asdf:load-system :thhrule)

(setq *load-pathname*
      (or
       #+sbcl (posix-getenv "srcdir")
       #+clisp (getenv "srcdir")
       nil))

(load (concatenate 'string *load-pathname* "/package.lisp"))
(load (concatenate 'string *load-pathname* "/thhrule.lisp"))
(load (concatenate 'string *load-pathname* "/predef.lisp"))

(use-package :thhrule)


(defun my-command-line ()
  (or 
   #+clisp *args*
   #+sbcl (cdr *posix-argv*)
   #+cmu extensions:*command-line-words*
   #+gcl si:*command-args*
   nil))

(defun main ()
  ;; ./thhcc FILE RULESET
  (let* ((cmd-line (my-command-line))
	 (rule-file (probe-file (car cmd-line)))
	 (ruleset/str (cadr cmd-line))
	 ruleset/sym)

    (unless rule-file
      (error "cannot read file ~s" (car cmd-line)))
    (load rule-file)

    (unless (and (stringp ruleset/str)
		 (nstring-upcase ruleset/str)
		 (setq ruleset/sym (find-symbol ruleset/str)))
      (error "ruleset ~a not defined" ruleset/str))
    (dotimes (i 64)
      (print (multiple-value-list (next-event (symbol-value ruleset/sym))))))
  (quit))

#+sbcl
(sb-ext:save-lisp-and-die "thhcc.o" :executable t :toplevel #'main)
#+clisp
(ext:saveinitmem "thhcc.o" :executable t :norc t :quiet t :init-function #'main)
#+cmu
(save-lisp "thhcc.o" :executable t :load-init-file nil :init-function #'main)
