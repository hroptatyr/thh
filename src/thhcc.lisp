(require "package")
(require "thhrule")
(require "predef")
(require "util")
(use-package '(:thhrule :stamp))

;; old stuff
(import 'thhrule::deftrading-hours)
(import 'thhrule::defholiday/yearly)
(import 'thhrule::defholiday/monthly)
(import 'thhrule::defholiday/weekly)
(import 'thhrule::defholiday/once)
(import 'thhrule::defholiday)

;; family stuff that isn't exported
(import 'thhrule::family)
(import 'thhrule::make-famiter)
(import 'thhrule::state-of)
(import 'thhrule::%tuplify-state)


(defun my-command-line ()
  (or 
   #+clisp *args*
   #+sbcl (cdr *posix-argv*)
   #+cmu extensions:*command-line-words*
   #+gcl si:*command-args*
   nil))

(defgeneric real-work (thing &key &allow-other-keys))

(defmethod real-work ((f family) &key metro-sta metro-end)
  ;; turn into a famiter
  (let ((fi (make-famiter :family f :metronome metro-sta))
	(cutoff (or metro-end (make-stamp :unix 4294967295)))
	d s)
    (loop
      while (and (multiple-value-setq (d s) (next-event fi))
		 (dt< (metronome-of fi) cutoff))
      do (format t "~a	~a~%" d s))))

(defmethod real-work ((thing t) &key &allow-other-keys)
  (error "family ~a is not iterable" thing))

(defun main ()
  ;; ./thhcc FILE RULESET
  (block nil
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

	(cond
	 ((and rule-file rule-file/real))
	 (t
	  (error "cannot read file ~s" rule-file)))
	(load rule-file/real)

	(cond
	 ((and (stringp ruleset/str)
	       (nstring-upcase ruleset/str)
	       (setq ruleset/sym (find-symbol ruleset/str))))
	 ((null ruleset/str)
	  ;; interactive mode
	  (return))
	 (t
	  (error "ruleset ~a not defined" ruleset/str)))

	;; assign ruleset
	(setq ruleset (symbol-value ruleset/sym))

	;; real work now
	(real-work ruleset :metro-sta metro-sta :metro-end metro-end)
	(quit)))))

#+sbcl
(sb-ext:save-lisp-and-die "thhcc.bin" :executable t :toplevel #'main)
#+clisp
(ext:saveinitmem "thhcc.bin"
		 :executable t :norc t :quiet t :init-function #'main)
#+cmu
(save-lisp "thhcc.bin" :executable t :load-init-file nil :init-function #'main)
