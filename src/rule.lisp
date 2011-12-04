;;; rule -- notion of rules, market, product or state rules
;;
;; Copyright (C) 2011 Sebastian Freundt
;;
;; Author:  Sebastian Freundt <freundt@ga-group.nl>
;;
;; This file is part of thh and dateutils.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the author nor the names of any contributors
;;    may be used to endorse or promote products derived from this
;;    software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(require "package")
(require "stamp")
(require "time")
(require "util")
(require "timezone")
(in-package :thhrule)


;; rule class
(defclass rule ()
  (
   ;; validity forms first
   (from
    :initarg :from
    :accessor valid-from-of
    :initform +dawn-of-time+)
   (till
    :initarg :till
    :accessor valid-till-of
    :initform +dusk-of-time+)
   ;; stream closure, takes stamp and returns the next occurrence
   (next-lambda
    :initarg :next-lambda
    :type function)
   (next
    :initarg :next
    :accessor next-of
    :type stamp)
   (state
    :initarg :state
    :initform nil
    :accessor state-of
    :type state)
   (state-start
    :initarg :state-start
    :reader get-start-state
    :type state)
   (state-end
    :initarg :state-end
    :reader get-end-state
    :type state)
   (timezone
    :initarg :timezone
    :accessor timezone-of)
   (in-lieu
    :initform nil
    :reader in-lieu-of
    :initarg :in-lieu)
   (name
    :initarg :name
    :accessor name-of
    :reader get-name)))

(defun make-rule (&rest v+k)
  (multiple-value-bind (vals keys) (split-vals+keys v+k)
    (apply #'make-instance 'rule keys)))

(defmethod print-object ((r rule) out)
  (print-unreadable-object (r out :type t)
    (format out "~a" (name-of r))))

(defmacro defrule (name &rest v+k)
  `(let ((r (make-rule ,@v+k :name ',name)))

     ;; stuff that needs to close over R

     ;; and finally inject to environ
     (defvar ,name r)))


;; actual functionality
(defmacro defrule/once (name &key from till on (for 1)
			     in-year function
			     (start-state '+market-last+)
			     (end-state '+market-last+)
			     state
			     &allow-other-keys)
  "Define a one-off event."
  (let ((from/stamp (or (parse-dtall from) +dawn-of-time+))
	(till/stamp (or (parse-dtall till) +dusk-of-time+))
	(on/stamp
	 (cond
	  ((and (eql (car function) 'function)
		(numberp (eval in-year)))
	   (funcall (eval function) (eval in-year)))
	  (t
	   (parse-date on)))))
    `(defrule ,name
       :from ,from/stamp
       :till ,till/stamp
       :state-start ',start-state
       :state-end ',end-state
       :state ,state
       :name ',name
       :next
       ,(if (and (d>= on/stamp from/stamp) (d<= on/stamp till/stamp))
	    (make-interval :start on/stamp :length for)
	  ;; otherwise the user is obviously confused
	  nil))))

(defmacro defrule/daily (name &key from till
			      start starts end ends
			      timezone
			      (start-state '+market-last+)
			      (end-state '+market-last+)
			      state
			      &allow-other-keys)
  (let* ((sta/stamp (parse-time (or start starts "00:00:00")))
	 (end/stamp (parse-time (or end ends "23:59:59")))
	 (from/stamp (or (parse-dtall from) +dawn-of-time+))
	 (till/stamp (or (parse-dtall till) +dusk-of-time+))
	 (zone (var-or-sym-value timezone))
	 (zone (cond
		((stringp zone)
		 (make-timezone :path timezone))
		((timezonep zone)
		 zone)))
	 (next-lambda (gensym (symbol-name name))))

    `(let ((rule
	    (make-rule
	     :from ,from/stamp
	     :till ,till/stamp
	     :timezone ,zone
	     :state-start ',start-state
	     :state-end ',end-state
	     :state ,state
	     :name ',name))
	   (ou (mod ,(get-unix sta/stamp) 86400))
	   (cu (mod ,(get-unix end/stamp) 86400))
	   (zone ,zone))
       ;; close over the rule, and stuff like the open and close times
       (defun ,next-lambda (stamp)
	 (with-slots (from till timezone) rule
	   (flet ((probe (day timeofday)
		    (let ((s (+ day timeofday)))
		      (make-datetime :unix (local-stamp->utc s timezone)))))
	     (let* ((fu (get-unix from))
		    (su (utc-stamp->local (get-unix stamp) timezone))
		    (stamp/midnight (midnight (max su fu) ou))
		    (probe/o (probe stamp/midnight ou))
		    (probe/c (probe stamp/midnight cu)))
	       (when (dt<= probe/o till)
		 (make-interval :start probe/o :end probe/c))))))
       ;; assign the closure as next-lambda
       (setf (slot-value rule 'next-lambda) #',next-lambda)
       (defvar ,name rule))))

(defmacro defrule/weekly (name &key from till on (for 1)
			       (start-state '+market-last+)
			       (end-state '+market-last+)
			       state
			       &allow-other-keys)
  (let ((from/stamp (or (parse-dtall from) +dawn-of-time+))
	(till/stamp (or (parse-dtall till) +dusk-of-time+))
	(on/sym (get-dow/sym on)))
    `(defrule ,name
       :from ,from/stamp
       :till ,till/stamp
       :state-start ',start-state
       :state-end ',end-state
       :state ,state
       :name ',name
       :next-lambda
       (lambda (stamp)
	 (do* ((sf (get-unix ,from/stamp))
	       (ss (get-unix stamp))
	       (s (midnight (max sf ss) 0) (+ 86400 s))
	       (probe))
	     ((and (eql (get-dow (setq probe (make-date :unix s))) ',on/sym)
		   (dt>= probe stamp))
	      (if (d<= probe ,till/stamp)
		  (make-interval :start probe :length ,for))))))))

(defmacro defrule/monthly (name &key from till on which
				;; by-year+month
				function
				in-lieu
				(for 1)
				(start-state '+market-last+)
				(end-state '+market-last+)
				state
				&allow-other-keys)
  (let ((from/stamp (or (parse-dtall from) +dawn-of-time+))
	(till/stamp (or (parse-dtall till) +dusk-of-time+)))
    (let ((probe-fun
	   (cond
	    ((and (null function) (null which))
	     (lambda (year month)
	       (make-date :year year :mon month :dom on)))
	    ((null function)
	     (lambda (year month)
	       (make-ymcw :year year :mon month :dow on :which which)))
	    ((eql (car function) 'function)
	     (eval function))
	    (t
	     (error "~a is not a function" function)))))
      `(defrule ,name
	 :from ,from/stamp
	 :till ,till/stamp
	 :state-start ',start-state
	 :state-end ',end-state
	 :state ,state
	 :name ',name
	 :in-lieu ,in-lieu
	 :next-lambda
	 (lambda (stamp)
	   (do* ((ym (max-stamp ,from/stamp stamp))
		 (m (get-mon ym) (if (> (1+ m) 12) 1 (1+ m)))
		 (y (get-year ym) (if (= m 1) (1+ y) y))
		 (probe))
	       ((dt>= (setq probe (funcall ,probe-fun y m)) stamp)
		(if (d<= probe ,till/stamp)
		    (make-interval :start probe :length ,for)))))))))

(defmacro defrule/yearly (name &key from till in on which
			       ;; by-year
			       function
			       in-lieu
			       (for 1)
			       (start-state '+market-last+)
			       (end-state '+market-last+)
			       state
			       &allow-other-keys)
  (let ((from/stamp (or (parse-dtall from) +dawn-of-time+))
	(till/stamp (or (parse-dtall till) +dusk-of-time+))
	(in/num (get-mon/num in)))
    (let ((probe-fun
	   (cond
	    ((and (null function) (null which))
	     (lambda (year)
	       (make-date :year year :mon in/num :dom on)))
	    ((null function)
	     (lambda (year)
	       (make-ymcw :year year :mon in/num :dow on :which which)))
	    ((eql (car function) 'function)
	     (eval function))
	    (t
	     (error "~a is not a function" function)))))
      `(defrule ,name
	 :from ,from/stamp
	 :till ,till/stamp
	 :state-start ',start-state
	 :state-end ',end-state
	 :state ,state
	 :name ',name
	 :in-lieu ,in-lieu
	 :next-lambda
	 (lambda (stamp)
	   (do* ((ys (get-year stamp))
		 (yf ,(get-year from/stamp))
		 (y (max ys yf) (1+ y))
		 (probe))
	       ((dt>= (setq probe (funcall ,probe-fun y)) stamp)
		(if (d<= probe ,till/stamp)
		    (make-interval :start probe :length ,for)))))))))


;; super macros and funs
(defgeneric push-rule (thing rule))
(defmethod push-rule (thing (r rule))
  ;; do nothing
  (format t "NO-OP #'push-rule called with ~a pusing onto ~a~%" r thing))

(defmacro defrule-macros (name &key state push-obj)
  ;; convenience macros
  (let* ((defname (sym-conc 'def name))
	 (defname-rule (if (eql name 'rule)
			   defname
			 (sym-conc defname '-rule)))
	 (defname/once (sym-conc defname '/once))
	 (defname/daily (sym-conc defname '/daily))
	 (defname/weekly (sym-conc defname '/weekly))
	 (defname/monthly (sym-conc defname '/monthly))
	 (defname/quarterly (sym-conc defname '/quarterly))
	 (defname/yearly (sym-conc defname '/yearly))
	 (state/key (and state (list :state state))))

    `(progn
       (defmacro ,defname/once (name &rest v+k)
	 `(prog1
	      (defrule/once ,name ,@v+k ,,@state/key)
	    (push-rule ,,push-obj ,name)))
       (defmacro ,defname/daily (name &rest v+k)
	 `(prog1
	      (defrule/daily ,name ,@v+k ,,@state/key)
	    (push-rule ,,push-obj ,name)))
       (defmacro ,defname/weekly (name &rest v+k)
	 `(prog1
	      (defrule/weekly ,name ,@v+k ,,@state/key)
	    (push-rule ,,push-obj ,name)))
       (defmacro ,defname/monthly (name &rest v+k)
	 `(prog1
	      (defrule/monthly ,name ,@v+k ,,@state/key)
	    (push-rule ,,push-obj ,name)))
       (defmacro ,defname/quarterly (name &rest v+k)
	 `(prog1
	      (defrule/quarterly ,name ,@v+k ,,@state/key)
	    (push-rule ,,push-obj ,name)))
       (defmacro ,defname/yearly (name &rest v+k)
	 `(prog1
	      (defrule/yearly ,name ,@v+k ,,@state/key)
	    (push-rule ,,push-obj ,name)))
       ;; lastly define the guy they all refer to
       (defmacro ,defname-rule (name &rest v+k)
	 `(prog1
	      (defrule ,name ,@v+k ,,@state/key)
	    (push-rule ,,push-obj ,name))))))


;; functionality on rules
(defgeneric next-state-flip (r stamp))

(defmethod next-state-flip ((r rule) (s stamp))
  (let ((s (max-stamp s (valid-from-of r))))
    (with-slots (next-lambda) r
      (when (or (not (slot-boundp r 'next))
		(dt>= s (get-interval-end (next-of r))))
	(setf (next-of r)
	      (funcall next-lambda s))))

    (let ((res
	   (cond
	    ((null (next-of r))
	     nil)
	    ((dt> s (get-interval-start (next-of r)))
	     (get-interval-end (next-of r)))
	    (t
	     (get-interval-start (next-of r))))))
      ;; inspect res once more, could be after valid-till-of
      (unless (dt> res (valid-till-of r))
	res))))

(provide "rule")
