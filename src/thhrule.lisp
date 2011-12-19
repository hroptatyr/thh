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
(require "family")
(in-package :thhrule)


;; sessions and timezones and other auxiliary stuff
(defmacro deftimezone (name value &optional doc)
  `(defparameter ,name (make-timezone :path ,value) ,doc))


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

(defgeneric next-event (thing)
  (:documentation
   "Return as multiple values the stamp of the next event and
a bitmask to be xor'd to the current state of THING."))

(defgeneric metro-round (famiter)
  (:documentation
   "Resort rules in FAMITER."))

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
	      ((dt< s (start-of r))
	       (start-of r))
	      ((dt< s (end-of r))
	       (end-of r))))))
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
		choend)))
    (labels ((pick/rule (r)
	       (let ((sta (start-of r))
		     (end (end-of r)))
		 (or (dt= sta cho)
		     (dt= end cho))))
	     (picker (rules)
	       (remove-if #'(lambda (r)
			      (not (pick/rule r)))
			  rules)))
      ;; multi values?
      (values cho (picker rules)))))

(defun requisites-met (s states)
  (loop
    for r in (requisites-of s)
    always (member r states)))

(defun active-state (famiter)
  (let (active inactive)
    (loop
      for s across (states-of famiter)
      for i from 0
      when (= (sbit (state-of famiter) i) 1)
      do (pushnew (svref s 2) active))

    ;; inhibitions first
    (loop
      for s in active
      do (loop
	   for i in (inhibitions-of s)
	   when (member i active)
	   do (push i inactive)))

    ;; now check requisites
    (loop
      for s in active
      unless (or (member s inactive)
		 (requisites-met s active))
      do (push s inactive))

    inactive))

(defmethod metro-round ((fi famiter))
  (let ((f (family-of fi)))
    (with-accessors ((metro metronome-of)) fi
      (flet ((next (rule)
	       (next-state-flip metro rule))
	     (metro-sort (rule1 rule2)
	       (metro-sort metro rule1 rule2)))
	(with-accessors ((rules rules-of)) f
	  ;; traverse rules first to make sure they're all up to data
	  (mapc #'next rules)
	  ;; stable-sort needs #'setf'ing under sbcl
	  (setf rules (sort rules #'metro-sort))
	  ;; all rules that match
	  (pick-next metro rules))))))

(defmethod next-event ((fi famiter))
  (let (old-fist rules)
    (with-accessors ((metro metronome-of) (fist state-of)) fi
      (setf old-fist (copy-seq fist))
      (loop
	while (multiple-value-setq (metro rules) (metro-round fi))
	do (flet ((set-state (r)
		    (let ((sta (start-of r))
			  (end (end-of r)))
		      (cond
		       ((dt= sta metro)
			(famiter-set-state fi r))
		       ((dt= end metro)
			(famiter-clr-state fi r))))))
	     (mapc #'set-state rules))
	;; apply inhibition rules and stuff
	do (let ((dea (active-state fi)))
	     (loop
	       for s across (states-of fi)
	       for i from 0
	       when (and (= (sbit fist i) 1)
			 (member (svref s 2) dea))
	       do (setf (sbit fist i) 0)))
	;; check if we really ever changed the state
	until (not (equalp fist old-fist)))
      (values metro fist))))


;; other stuff, for keeps
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
