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

(defgeneric next-event (thing))

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
		choend))
	 (rules (remove-if #'(lambda (r)
			       (let ((sta (start-of r))
				     (end (end-of r)))
				 (not (or (dt= sta cho)
					  (dt= end cho)))))
			   rules)))
    ;; multi values?
    (cons cho rules)))

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
  (let ((rules (metro-round fi)))
    (with-accessors ((metro metronome-of)) fi
      (when (setf metro (car rules))
	(flet ((state-of (r)
		 (let ((sta (start-of r))
		       (end (end-of r)))
		   (cons (cond
			  ((dt= sta metro)
			   'set)
			  ((dt= end metro)
			   'unset)
			  (t
			   'identity)) (state-of r)))))
	  (values metro
		  (mapcar #'state-of (cdr rules))))))))


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
