;; thhrule -- rule definitions
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
(require "util")
(require "time")
(require "copy-instance")
(require "timezone")
(require "rule")
(in-package :thhrule)


;; cybertiggyr glue
(defparameter +date-recognisers+
  (list (make-fmt-recognizer "%Y-%m-%d")
	(make-fmt-recognizer "%Y-%b-%d")
	(make-fmt-recognizer "%Y-%m")
	(make-fmt-recognizer "%Y-%b")
	(make-fmt-recognizer "%Y")))

(defparameter +time-recognisers+
  (list (make-fmt-recognizer "%H:%M:%S")
	(make-fmt-recognizer "%H:%M:%S%Z")))

(defparameter +datetime-recognisers+
  (list (make-fmt-recognizer "%Y-%m-%dT%H:%M:%S")
	(make-fmt-recognizer "%Y-%m-%dT%H:%M:%S%Z")
	(make-fmt-recognizer "%Y-%B-%dT%H:%M:%S")
	(make-fmt-recognizer "%Y-%B-%dT%H:%M:%S%Z")))

(defun parse-time/glue-all (str)
  (let (u)
    (cond
     ((setq u (cybertiggyr-time::parse-time str +datetime-recognisers+))
      (make-datetime :unix u))
     ((setq u (cybertiggyr-time::parse-time str +date-recognisers+))
      (make-date :unix u))
     ((setq u (cybertiggyr-time::parse-time str +time-recognisers+))
      (make-time :unix u))
     ((setq u (cybertiggyr-time::parse-time str))
      (make-stamp :unix u)))))

(defun parse-time/glue-date (str)
  (let (u)
    (cond
     ((setq u (cybertiggyr-time::parse-time str +date-recognisers+))
      (make-date :unix u)))))

(defun parse-time/glue-time (str)
  (let (u)
    (cond
     ((setq u (cybertiggyr-time::parse-time str +time-recognisers+))
      (make-time :unix u)))))

(defun parse-time/glue-datetime (str)
  (let (u)
    (cond
     ((setq u (cybertiggyr-time::parse-time str +datetime-recognisers+))
      (make-datetime :unix u)))))


;; states
(defconstant +market-last+ t)
(defconstant +market-open+ t)
(defconstant +market-close+ t)
(defconstant +market-force+ t)

(defparameter +states+
  '(+market-last+ +market-open+ +market-close+ +market-force+))

;;(deftype state () `(member ,@+states+))

(defmethod state< (s1 s2)
  ;; s1 < s2 iff s1 further left in +states+
  (member s2 (cdr (member s1 +states+))))

(defmethod state<= (s1 s2)
  (or (eql s1 s2)
      (state< s1 s2)))

(defmethod state> (s1 s2)
  (not (state<= s1 s2)))

(defmethod state>= (s1 s2)
  (or (eql s1 s2)
      (state> s1 s2)))

(defun max-state (a b)
  (if b
      (if (state> a b)
	  a
	b)
    a))

(defun max-stamp (a b)
  (if b
      (if (dt> a b)
	  a
	b)
    a))

(defun get-ultimo (year mon)
  "Return ultimo of MON in YEAR."
  (let* ((nmon (1+ mon))
	 (nyear (+ year (floor nmon 12)))
	 (nx (make-date :year nyear :mon nmon :dom 1)))
    (d+ nx -1)))

(defun get-mdays (year mon)
  "Return the number of days in MON of YEAR."
  (let ((ult (get-ultimo year mon)))
    (get-dom ult)))

(defun make-ymcw (&key year mon dow which)
  "Like dateutils' ymcw."
  ;; 	wd01 = (wd_jan01 - 1 + wd01) % 7;
  ;; 
  ;; 	/* first WD1 is 1, second WD1 is 8, third WD1 is 15, etc.
  ;; 	 * so the first WDx with WDx > WD1 is on (WDx - WD1) + 1 */
  ;; 	res = (that.w + 7 - wd01) % 7 + 1 + 7 * (that.c - 1);
  ;; 	/* not all months have a 5th X, so check for this */
  ;; 	if (res > __get_mdays(that.y, that.m)) {
  ;; 		 /* 5th = 4th in that case */
  ;; 		res -= 7;
  ;; 	}
  ;; 	return res;
  (let* ((s (make-date :year year :mon mon :dom 1))
	 (sdow (get-dow/num (get-dow s)))
	 (dow/num (get-dow/num dow))
	 (which/num (cond
		     ((numberp which)
		      which)
		     ((symbolp which)
		      (let ((sym (intern (symbol-name which) 'thhrule)))
			(case sym
			  (1st 1)
			  (2nd 2)
			  (3rd 3)
			  (4th 4)
			  (last 5)
			  (otherwise 0))))))
	 (dom (+ (mod (- (+ dow/num 7) sdow) 7) 1 (* 7 (1- which/num))))
	 (ult (get-mdays year mon)))
    (make-date :year year
	       :mon mon
	       :dom (if (> dom ult)
			(- dom 7)
		      dom))))


;; sessions and timezones and other auxiliary stuff
(defmacro deftimezone (name value &optional doc)
  `(defparameter ,name ,(make-timezone :path value) ,doc))

(defmacro defsession (name &optional doc)
  `(defvar ,name ,doc))


(defgeneric parse-date (thing))
(defgeneric parse-time (thing))
(defgeneric parse-datetime (thing))
(defgeneric parse-dtall (thing))

(defmethod parse-date ((s string))
  (parse-time/glue-date s))

(defmethod parse-date ((s symbol))
  (parse-time/glue-date (symbol-name s)))

(defmethod parse-date ((s stamp))
  s)

(defmethod parse-time ((s string))
  (parse-time/glue-time s))

(defmethod parse-time ((s symbol))
  (parse-time/glue-time (symbol-name s)))

(defmethod parse-time ((s stamp))
  s)

(defmethod parse-datetime ((s string))
  (parse-time/glue-datetime s))

(defmethod parse-datetime ((s symbol))
  (parse-time/glue-datetime (symbol-name s)))

(defmethod parse-datetime ((s stamp))
  s)

(defmethod parse-dtall ((s string))
  (parse-time/glue-all s))

(defmethod parse-dtall ((s symbol))
  (parse-time/glue-all (symbol-name s)))

(defmethod parse-dtall ((s stamp))
  s)

(defmethod midnight ((su integer) &optional next)
  (let ((sm (mod su 86400)))
    (+ (- su sm) (or (and next (> sm next) 86400) 0))))

(defmethod midnight ((s stamp) &optional next)
  (midnight (get-unix s) next))

(defmethod get-start ((r rule))
  (with-slots (next) r
    (get-start next)))

(defmethod get-start ((r (eql nil)))
  nil)

(defmethod get-end ((r rule))
  (with-slots (next) r
    (get-end next)))

(defmethod get-end ((r (eql nil)))
  nil)

;; aux methods, eql specialisers
(defmethod dt> ((i1 interval) (i2 (eql nil)))
  nil)

(defmethod dt> ((i1 (eql nil)) i2)
  (declare (ignore i2))
  t)

(defmethod dt= ((s1 interval) (s2 (eql nil)))
  (declare (ignore s2))
  nil)

(defmethod dt= ((s1 (eql nil)) s2)
  (declare (ignore s2))
  nil)

(defmethod utc-stamp->offset ((s stamp) (tz timezone))
  (utc-stamp->offset (get-unix s) tz))

(defgeneric local-stamp->utc (s tz))
(defgeneric utc-stamp->local (s tz))

(defmethod utc-stamp->local ((s stamp) tz)
  (make-stamp :what (type-of s) :unix (utc-stamp->local (get-unix s) tz)))

(defmethod local-stamp->utc ((s stamp) tz)
  (make-stamp :what (type-of s) :unix (local-stamp->utc (get-unix s) tz)))

(defmethod utc-stamp->local ((s integer) tz)
  (+ s (utc-stamp->offset s tz)))

(defmethod local-stamp->utc ((s integer) tz)
  (- s (utc-stamp->offset s tz)))

;; some macros
(defmacro defholiday-fun (name def &optional comment)
  ;; double backquotes YAY
  ;; we want
  ;; (defmacro foo (name &rest rest)
  ;;   (def name ,@rest
  ;;        :start-state bla
  ;;        :end-state bla))
  (declare (ignore comment))
  `(defmacro ,name (name &rest rest)
     `(,',def ,name
	      ,@rest
	      :start-state +market-close+
	      :end-state +market-last+)))

(defmacro deftrading-hours (name &rest vals+keys)
  (multiple-value-bind (vals keys) (split-vals+keys vals+keys)
    (destructuring-bind (&key open close &allow-other-keys) keys
      (let ((doc (when (stringp (car vals))
		   (prog1
		       (car vals)
		     (setq vals (cdr vals))))))
	(declare (ignore doc))

	(if (null vals)
	    `(defrule/daily ,name ,@keys
	       :start ,open
	       :end ,close
	       :start-state +market-open+
	       :end-state +market-close+
	       :allow-other-keys t)
	  ;; otherwise assume it's a list so we deliver a ruleset
	  `(defruleset ,name
	     ,@(loop
		 with prev-r = nil
		 for r in vals
		 collect
		 (multiple-value-bind (vals keys) (split-vals+keys r)
		   (declare (ignore vals))
		   (destructuring-bind
		       (&key open close from till &allow-other-keys) keys
		     (declare (ignore till))
		     (let ((name (gensym (symbol-name name)))
			   rule)
		       (eval `(defrule/daily ,name ,@keys
				:start ,open
				:end ,close
				:start-state +market-open+
				:end-state +market-close+
				:allow-other-keys t))
		       ;; chain from/till slots
		       (setf rule (symbol-value name))
		       (when (and prev-r
				  from
				  (eql (slot-value prev-r 'till)
				       +dusk-of-time+))
			 (setf (slot-value prev-r 'till)
			       (slot-value rule 'from)))
		       (when (and prev-r
				  (null from)
				  (not (eql (slot-value prev-r 'till)
					    +dusk-of-time+)))
			 (setf (slot-value rule 'from)
			       (slot-value prev-r 'till)))
		       (setf prev-r rule)
		       ;; return the symbol so the ruleset makes sense
		       name))))))))))

(defmacro defholiday (name &rest vals+keys)
  (multiple-value-bind (vals keys) (split-vals+keys vals+keys)
    (destructuring-bind (&key in-lieu alias &allow-other-keys) keys
      (let ((doc (if (stringp (car vals))
		     (car vals)))
	    (clone (cond
		    ((boundp in-lieu)
		     (symbol-value in-lieu))
		    ((boundp alias)
		     (symbol-value alias)))))
	`(and
	  (defvar ,name
	    ,(and clone (copy-instance clone))
	    ,(or doc "Aliased holiday rule."))
	  ,(boundp in-lieu)
	  ;; replace the next-lambda with an in-lieu lambda
	  (with-slots (in-lieu) ,name
	    (setf in-lieu t)))))))

(defholiday-fun defholiday/once defrule/once
  "For one off rules.")

(defholiday-fun defholiday/weekly defrule/weekly
  "Define weekly recurring holidays, weekends, etc..")

(defholiday-fun defholiday/monthly defrule/monthly
  "Define monthly recurring holidays.")

(defholiday-fun defholiday/yearly defrule/yearly
  "Define yearly recurring holidays.")


(defmethod next-event/rule ((metronome stamp) (r rule))
  (with-slots (till next next-lambda) r
    (setf next
	  (cond
	   ((dt> metronome till)
	    nil)
	   ((and (slot-boundp r 'next)
		 (or (null next) (dt>= (get-start next) metronome)))
	    next)
	   ((and (slot-boundp r 'next-lambda)
		 (functionp next-lambda))
	    (funcall next-lambda metronome))
	   (t
	    nil)))))


;; rulesets
(defclass ruleset ()
  ((metronome
    :initarg :metronome
    :accessor metronome-of
    :type stamp)
   (state
    :initform +market-last+
    :type state)
   (rule
    :initform nil
    :type rule)
   (rules
    :initarg :rules
    :type sequence)))

(defmacro make-ruleset (&rest stuff)
  `(make-instance 'ruleset ,@stuff))

(defun expand-rules (rules)
  "Expand every ruleset in RULES by its rules."
  (flet ((rulep (var-or-sym)
	   (var-or-sym-type-p var-or-sym 'rule))
	 (rulesetp (var-or-sym)
	   (var-or-sym-type-p var-or-sym 'ruleset)))
    (declare (ignore #'rulep))
    (loop for sym in rules
      if (rulesetp sym)
      append (slot-value (var-or-sym-value sym) 'rules)
      else
      collect (var-or-sym-value sym)
      end)))

(defmacro defruleset (name &rest vars+keys)
  "&key (metronome +dawn-of-time+)"
  (multiple-value-bind (rules keys) (split-vals+keys vars+keys)
    (destructuring-bind (&key (metronome +dawn-of-time+)) keys
      `(defvar ,name
	 (make-ruleset
	  :metronome ,(parse-dtall (eval metronome))
	  :rules ',(expand-rules rules))))))

(defmethod move-in-lieu ((mover rule) (movee rule))
  "Move MOVEE to the end of MOVER."
  (with-slots ((mover-next next)) mover
    (let ((eo-mover (midnight (get-end mover-next) 0)))
      (with-slots ((movee-next next)) movee
	(let ((length (get-length movee-next))
	      (new-start (make-stamp :what (type-of (get-start movee-next))
				     :unix eo-mover)))
	  (setf movee-next
		(make-interval :start new-start :length length)))))))

(defun pick-next (rules)
  (let* ((chosen (car rules))
	 (chostart (get-start chosen))
	 (choend (get-end chosen))
	 (covers (remove-if #'(lambda (r)
				(or (null (in-lieu-of r))
				    (eql r chosen)
				    (dt>= (get-start r) choend)))
			    rules)))
    ;; reschedule in-lieu holidays, british meaning, i.e. postpone them
    (loop for r in covers
      do (move-in-lieu chosen r))

    (if chostart
	(values chostart (get-start-state chosen) chosen)
      (values nil '+market-last+ nil))))

(defmethod metro-sort ((metronome stamp) (r1 rule) (r2 rule))
  "Return T if R1 is sooner than R2."
  (let ((ne1 (next-event/rule metronome r1))
	(ne2 (next-event/rule metronome r2)))
    (cond
     ((dt< ne1 ne2)
      t)
     ((dt= ne1 ne2)
      ;; in-lieu rules count less
      (if (in-lieu-of r1)
	  nil
	(if (in-lieu-of r2)
	    t
	  (state> (get-start-state r1) (get-start-state r2))))))))

(defmethod metro-next ((rs ruleset) (r rule))
  "Find next metronome point, given that R is the chosen rule."
  (with-slots (rules) rs
    (with-slots ((rnext next) (rstate state-start)) r
      ;; find first rule whose start > r's start and whose state > r's state
      (let ((cand
	     (find-if #'(lambda (a)
			  (with-slots ((anext next) (astate state-start)) a
			    (let ((astart (get-start anext))
				  (aend (get-end anext))
				  (rstart (get-start rnext))
				  (rend (get-end rnext)))
			      (and astart
				   (dt> astart rstart)
				   (dt<= astart rend)
				   (or (state> astate rstate)
				       (dt> aend rend))))))
		      rules)))
	(if cand
	    (values (get-start cand) (get-start-state cand) cand)
	  (values (get-end rnext) (get-end-state r) r))))))

(defmethod metro-round ((rs ruleset))
  (with-slots (metronome state rules) rs
    ;; stable-sort needs #'setf'ing under sbcl
    (setf rules (sort rules #'(lambda (a b) (metro-sort metronome a b))))
    ;; pick a rule
    (pick-next rules)))

(defmethod next-event ((rs ruleset))
  (with-slots (metronome state rule rules) rs
    (multiple-value-bind (stamp newst newru) (metro-round rs)
      (declare (ignore newru))
      (loop
	when (null metronome)
	return nil
	do (setf (values metronome state rule)
		 (cond
		  ((or (not (eql newst state))
		       (dt> stamp metronome))
		   (metro-round rs))
		  ((dt= stamp metronome)
		   (metro-next rs rule))
		  (t
		   (error "state inconsistent ~a < ~a" stamp metronome))))
	unless (eql state '+market-last+)
	return (values metronome state rule
		       (get-end (slot-value rule 'next)))))))

(provide :thhrule)
(provide "thhrule")

;; thhrule.l ends here
