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


;; recurring events class
(defclass recrev ()
  (;; stream closure, takes stamp and returns the next occurrence
   (next-lambda
    :initarg :next-lambda
    :initform nil
    :accessor lambda-of
    :type function)
   (next
    :initarg :next
    :initform nil
    :accessor next-of
    :type stamp)))

;; validity class
(defclass validity ()
  ((validity
    :initarg :validity
    :accessor validity-of
    ;; nil means never valid, t means always valid
    :initform nil
    :type interval)))

;; rule class
(defclass rule (recrev validity)
  ((name
    :initarg :name
    :accessor name-of)
   (state
    :initarg :state
    :initform nil
    :accessor state-of
    :type state)
   (timezone
    :initarg :timezone
    :accessor timezone-of)

   ;; slots that are gonna disappear when the uberfamilies come
   (products
    :initarg :products
    :accessor products-of)
   (markets
    :initarg :markets
    :accessor markets-of)))

(defun %validity-ctor (&key from till validity &allow-other-keys)
  (or validity
      (let ((st (or from +dawn-of-time+))
	    (en (or till +dusk-of-time+)))
	(make-interval :start st :end en))))

(defun make-recrev (&rest v+k)
  (multiple-value-bind (vals keys) (split-vals+keys v+k)
    (declare (ignore vals))
    (destructuring-bind (&key next next-lambda &allow-other-keys) keys
      (make-instance 'recrev :next next :next-lambda next-lambda))))

(defun make-validity (&rest v+k)
  (multiple-value-bind (vals keys) (split-vals+keys v+k)
    (declare (ignore vals))
    (let ((v (apply #'%validity-ctor keys)))
      (make-instance 'validity :validity v))))

(defmacro make-rule (&rest keys &key &allow-other-keys)
  (let ((v (apply #'%validity-ctor keys)))
    ;; check special keys
    (apply #'%auto-generate-families keys)
    (destructuring-bind (&key product market &allow-other-keys) keys
      `(make-instance 'rule
		      :validity ,v
		      :allow-other-keys t
		      ,@keys
		      :products ,product
		      :markets ,market))))

(defmethod print-object ((r recrev) out)
  (print-unreadable-object (r out :type t)
    (format out "~a" (lambda-of r))))

(defmethod print-object ((v validity) out)
  (print-unreadable-object (v out :type t)
    (format out "~a" (validity-of v))))

(defmethod print-object ((r rule) out)
  (print-unreadable-object (r out :type t)
    (format out "~a" (name-of r))))

(defmacro defrule (name &rest v+k)
  `(let ((r (make-rule ,@v+k :name ',name :allow-other-keys t)))

     ;; stuff that needs to close over R

     ;; and finally inject to environ
     (defvar ,name r)))

(defun %auto-generate-families (&key product market state &allow-other-keys)
  (macrolet ((maybe-gen (p &body form)
			`(when (and ,p (not (boundp ,p)))
			   (eval (progn ,@form)))))
    ;; (put 'maybe-gen 'lisp-indent-function 1)
    (maybe-gen product
      (format t "** auto-gen'd product ~a~%" product)
      `(defproduct ,product))

    (maybe-gen market
      (format t "** auto-gen'd market ~a~%" market)
      `(defmarket ,market))

    (maybe-gen state
      (format t "** auto-gen'd state ~a~%" state)
      `(defstate ,state))))


;; actual functionality
(defmacro defrule/once (name &rest keys &key from till on (for 1)
			     in-year function
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
       :state ,state
       :name ',name
       :next
       ,(if (and (d>= on/stamp from/stamp) (d<= on/stamp till/stamp))
	    (make-interval :start on/stamp :length for)
	  ;; otherwise the user is obviously confused
	  nil))))

(defmacro defrule/daily (name &rest keys &key from till
			      start starts end ends
			      timezone state
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
	     :state ,state
	     :name ',name
	     ,@keys
	     :allow-other-keys t))
	   (ou (mod ,(get-unix sta/stamp) 86400))
	   (cu (mod ,(get-unix end/stamp) 86400))
	   (zone ,zone))
       ;; close over the rule, and stuff like the open and close times
       (defun ,next-lambda (stamp)
	 (with-accessors ((tz timezone-of)) rule
	   (flet ((probe (day timeofday)
		    (let ((s (+ day timeofday)))
		      (make-datetime :unix (local-stamp->utc s tz)))))
	     (let* ((su (utc-stamp->local (get-unix stamp) tz))
		    (stamp/midnight (midnight su ou))
		    (probe/o (probe stamp/midnight ou))
		    (probe/c (probe stamp/midnight cu)))
	       (make-interval :start probe/o :end probe/c)))))
       ;; assign the closure as next-lambda
       (setf (slot-value rule 'next-lambda) #',next-lambda)
       (defvar ,name rule))))

(defmacro defrule/weekly (name &rest keys &key from till on (for 1)
			       state
			       &allow-other-keys)
  (let ((from/stamp (or (parse-dtall from) +dawn-of-time+))
	(till/stamp (or (parse-dtall till) +dusk-of-time+))
	(on/sym (get-dow/sym on))
	(next-lambda (gensym (symbol-name name))))

    `(let ((rule
	    (make-rule
	     :from ,from/stamp
	     :till ,till/stamp
	     :state ,state
	     :name ',name)))
       ;; close over the rule
       (defun ,next-lambda (stamp)
	 (do* ((ss (get-unix stamp))
	       (s (midnight ss 0) (+ 86400 s))
	       (probe))
	     ((and (eql (get-dow (setq probe (make-date :unix s))) ',on/sym)
		   (dt>= probe stamp))
	      (make-interval :start probe :length ,for))))
       ;; assign the closure as next-lambda
       (setf (slot-value rule 'next-lambda) #',next-lambda)
       (defvar ,name rule))))

(defmacro defrule/monthly (name &rest keys &key from till on which
				;; by-year+month
				function
				in-lieu
				(for 1)
				state
				&allow-other-keys)
  (let ((from/stamp (or (parse-dtall from) +dawn-of-time+))
	(till/stamp (or (parse-dtall till) +dusk-of-time+))
	(next-lambda (gensym (symbol-name name))))

    `(flet ((probe-fun (year month)
	      (cond
	       (,(and (null function) (null which))
		(make-date :year year :mon month :dom ,on))
	       (,(null function)
		(make-ymcw :year year :mon month :dow ,on :which ,which))
	       (,(eql (car function) 'function)
		(funcall ,function year month))
	       (t
		(error "~a is not a function" ,function)))))
       (let ((rule
	      (make-rule
	       :from ,from/stamp
	       :till ,till/stamp
	       :state ,state
	       :name ',name
	       :in-lieu ,in-lieu)))
	 ;; close over rule
	 (defun ,next-lambda (stamp)
	   (do* ((m (get-mon stamp) (if (> (1+ m) 12) 1 (1+ m)))
		 (y (get-year stamp) (if (= m 1) (1+ y) y))
		 (probe))
	       ((dt>= (setq probe (probe-fun y m)) stamp)
		(make-interval :start probe :length ,for))))
	 ;; assign the closure as next-lambda
	 (setf (slot-value rule 'next-lambda) #',next-lambda)
	 (defvar ,name rule)))))

(defmacro defrule/yearly (name &rest keys &key from till in on which
			       ;; by-year
			       function
			       in-lieu
			       (for 1)
			       state
			       &allow-other-keys)
  (let ((from/stamp (or (parse-dtall from) +dawn-of-time+))
	(till/stamp (or (parse-dtall till) +dusk-of-time+))
	(in/num (get-mon/num in))
	(next-lambda (gensym (symbol-name name))))

    `(flet ((probe-fun (year)
	      (cond
	       (,(and (null function) (null which))
		(make-date :year year :mon ,in/num :dom ,on))
	       (,(null function)
		(make-ymcw :year year :mon ,in/num :dow ,on :which ,which))
	       (,(eql (car function) 'function)
		(funcall ,function year))
	       (t
		(error "~a is not a function" ,function)))))
       (let ((rule
	      (make-rule
	       :from ,from/stamp
	       :till ,till/stamp
	       :state ,state
	       :name ',name
	       :in-lieu ,in-lieu)))
	 ;; close over rule
	 (defun ,next-lambda (stamp)
	   (do* ((y (get-year stamp) (1+ y))
		 (probe))
	       ((dt>= (setq probe (probe-fun y)) stamp)
		(make-interval :start probe :length ,for))))
	 (defvar ,name rule)))))


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

(provide "rule")
