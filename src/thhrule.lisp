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
(require "stamp")
(require "time")
(require "copy-instance")
(require "timezone")
(require "util")
(require "rule")
(in-package :thhrule)


;; sessions and timezones and other auxiliary stuff
(defmacro deftimezone (name value &optional doc)
  `(defparameter ,name (make-timezone :path ,value) ,doc))

(defmacro defsession (name &optional doc)
  `(defvar ,name ,doc))


(defmethod start-of ((r rule))
  (when (next-of r)
    (start-of (next-of r))))

(defmethod start-of ((r (eql nil)))
  (declare (ignore r))
  nil)

(defmethod end-of ((r rule))
  (when (next-of r)
    (end-of (next-of r))))

(defmethod end-of ((r (eql nil)))
  (declare (ignore r))
  nil)

(defmethod length-of ((r rule))
  (when (next-of r)
    (length-of (next-of r))))

(defmethod length-of ((r (eql nil)))
  (declare (ignore r))
  nil)


;; aux methods, eql specialisers
(defmethod dt> ((s1 stamp) (s2 (eql nil)))
  nil)

(defmethod dt> ((i1 interval) (i2 (eql nil)))
  nil)

(defmethod dt> ((i1 (eql nil)) i2)
  (declare (ignore i2))
  t)

(defmethod dt= ((s1 stamp) (s2 (eql nil)))
  nil)

(defmethod dt= ((s1 interval) (s2 (eql nil)))
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


;; stuff that glues famiters and rules
(defgeneric metro-sort (stamp r1 r2)
  (:documentation
   ""))
(defgeneric pick-next (stamp rules)
  (:documentation
   "Given STAMP and a list of RULES, find rules that switch the state next."))
(defgeneric next-state-flip (stamp rule)
  (:documentation
   ""))

(defmethod next-state-flip ((s stamp) (r rule))
  (let* ((v (validity-of r))
	 (vst (start-of v))
	 (ven (end-of v))
	 (s (max-stamp s vst)))

    (when (or (null (next-of r))
	      (dt>= s (end-of (next-of r))))
      (setf (next-of r)
	    (when (lambda-of r)
	      (funcall (lambda-of r) s))))

    (let ((res
	   (when (next-of r)
	     (cond
	      ((dt< s (start-of (next-of r)))
	       (start-of (next-of r)))
	      ((dt< s (end-of (next-of r)))
	       (end-of (next-of r)))))))
      ;; inspect res once more, could be after valid-till-of
      (unless (dt> res ven)
	res))))

(defmethod metro-sort ((metronome stamp) (r1 rule) (r2 rule))
  "Return T if R1 is sooner than R2."
  (let ((ne1 (next-state-flip metronome r1))
	(ne2 (next-state-flip metronome r2)))
    (dt< ne1 ne2)))

(defmethod pick-next ((metro stamp) (rules list))
  (let* ((chosen (car rules))
	 (chosta (start-of chosen))
	 (choend (end-of chosen))
	 (cho (if (dt< metro chosta)
		  chosta
		choend))
	 (rules (remove-if #'(lambda (r)
			       (let ((sta (start-of r))
				     (end (end-of r)))
				 (not (or (dt= sta cho)
					  (dt= end cho)))))
			   rules)))
    ;; multi values?
    (cons cho rules)))


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
	      (new-start (make-stamp
			  :what (type-of (get-start movee-next))
			  :unix eo-mover)))
	  (setf movee-next
		(make-interval :start new-start :length length)))))))

(provide :thhrule)
(provide "thhrule")

;; thhrule.l ends here
