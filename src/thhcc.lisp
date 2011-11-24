(require "package")
(require "thhrule")
(require "predef")
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
  (let ((cmd-line (my-command-line)))
    (unless cmd-line
      (error "Usage: thhcc RULE-FILE RULESET-SYMBOL"))

    (let* ((rule-file (car cmd-line))
	   (rule-file/real (probe-file rule-file))
	   (ruleset/str (cadr cmd-line))
	   ruleset/sym)

      (unless (and rule-file rule-file/real)
	(error "cannot read file ~s" rule-file)
	(quit))
      (load rule-file/real)

      (unless (and (stringp ruleset/str)
		   (nstring-upcase ruleset/str)
		   (setq ruleset/sym (find-symbol ruleset/str)))
	(error "ruleset ~a not defined" ruleset/str)
	(quit))

      ;; real work now
      (loop
	with rs = (symbol-value ruleset/sym)
	and cutoff = (thhrule::make-stamp :unix 4294967295)
	and d and s and r
	while (and (multiple-value-setq (d s r) (thhrule::next-event rs))
		   (thhrule::dt< (thhrule::metronome-of rs) cutoff))
	do (format t "~a	~a	~a~%" d s r))))
  (quit))

#+sbcl
(sb-ext:save-lisp-and-die "thhcc" :executable t :toplevel #'main)
#+clisp
(ext:saveinitmem "thhcc" :executable t :norc t :quiet t :init-function #'main)
#+cmu
(save-lisp "thhcc" :executable t :load-init-file nil :init-function #'main)
