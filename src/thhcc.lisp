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
	   (year/str (caddr cmd-line))
	   (year/num (and year/str (parse-integer year/str)))
	   (metro-sta (when year/num
			(make-date :year year/num :mon 1 :dom 1)))
	   (metro-end (when year/num
			  (make-datetime :year year/num :mon 12 :dom 31
					 :hour 23 :min 59 :sec 59)))
	   ruleset/sym
	   ruleset)

      (unless (and rule-file rule-file/real)
	(error "cannot read file ~s" rule-file)
	(quit))
      (load rule-file/real)

      (unless (and (stringp ruleset/str)
		   (nstring-upcase ruleset/str)
		   (setq ruleset/sym (find-symbol ruleset/str)))
	(error "ruleset ~a not defined" ruleset/str)
	(quit))

      ;; assign ruleset
      (and (setq ruleset (symbol-value ruleset/sym))
	   metro-sta
	   (setf (metronome-of ruleset) metro-sta))

      ;; real work now
      (unless (and ruleset (eql (type-of ruleset) 'thhrule::ruleset))
	(error "ruleset ~a is not a ruleset instance" ruleset/str)
	(quit))

      (loop
	with rs = ruleset
	and cutoff = (or metro-end (make-stamp :unix 4294967295))
	and d and s and r and e
	while (and (multiple-value-setq (d s r e) (next-event rs))
		   (thhrule::dt< (metronome-of rs) cutoff))
	do (format t "~a	~a	~a	~a~%" d s r e))))
  (quit))

#+sbcl
(sb-ext:save-lisp-and-die "thhcc.bin" :executable t :toplevel #'main)
#+clisp
(ext:saveinitmem "thhcc.bin"
		 :executable t :norc t :quiet t :init-function #'main)
#+cmu
(save-lisp "thhcc.bin" :executable t :load-init-file nil :init-function #'main)
