;;; family -- families of rules
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
(require "timezone")
(require "util")
(require "rule")
(in-package :thhrule)


;; family class
(defclass family ()
  ((name
    :initarg :name
    :initform 'unknown
    :accessor name-of)
   (timezone
    :initarg :timezone
    :initform local-time::+utc-zone+
    :accessor timezone-of)
   (rules
    :initarg :rules
    :initform nil
    :accessor rules-of)))

(defun make-family (&rest v+k)
  (multiple-value-bind (vals keys) (split-vals+keys v+k)
    (declare (ignore vals))
    (apply #'make-instance 'family :allow-other-keys t keys)))

(defmethod print-object ((f family) out)
  (print-unreadable-object (f out :type t)
    (format out "~a" (name-of f))))


;; family iterators, we promote normal families to iters to capture
;; the state and metronome but via inheritance this allows to use
;; the forgetful functor to create another iterator
(defclass famiter ()
  ((metronome
    :initarg :metronome
    :initform nil
    :accessor metronome-of
    :type stamp)
   (state
    :initarg :state
    :initform nil
    :accessor state-of
    :type state)
   (family
    :initarg :family
    :initform nil
    :accessor family-of
    :type family)))

(defun compute-initial-metronome (f)
  ;; for now we just come up with a number
  (declare (ignore f))
  (make-date :year 1999 :mon 1 :dom 1))

(defun make-famiter (&rest v+k)
  (multiple-value-bind (vals keys) (split-vals+keys v+k)
    (let* ((old (or (and (subtypep (type-of (car vals)) 'family)
			 (car vals))
		    (destructuring-bind (&key family &allow-other-keys) keys
		      family)))
	   (sta (compute-initial-metronome old)))
      (make-instance 'famiter :family old :metronome sta))))

(defmethod print-object ((f famiter) out)
  (print-unreadable-object (f out :type t)
    (format out "~a @~a :state ~a"
	    (name-of (family-of f)) (metronome-of f) (state-of f))))

(defgeneric next-event (thing))
(defgeneric metro-round (thing))

(defun pick-next (rules)
  (let* ((chosen (car rules))
	 (chostart (get-start chosen))
	 (choend (get-end chosen))
	 (covers (remove-if #'(lambda (r)
				(or ;;(null (in-lieu-of r))
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
    (dt< ne1 ne2)))

(defmethod metro-round ((fi famiter))
  ;; stable-sort needs #'setf'ing under sbcl
  (let ((f (family-of fi)))
    (setf (rules-of f)
	  (sort (rules-of f) #'(lambda (a b)
				 (metro-sort (metronome-of fi) a b))))
    ;; pick a rule
    (pick-next (rules-of f))))

(defmethod metro-next ((fi famiter) (r rule))
  "Find next metronome point, given that R is the chosen rule."
  (let ((f (family-of fi)))
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
		      (rules-of f))))
	(if cand
	    (values (get-start cand) (get-start-state cand) cand)
	  (values (get-end rnext) (get-end-state r) r))))))

(defmethod next-event ((fi famiter))
  (multiple-value-bind (stamp newst newru rule) (metro-round fi)
    (declare (ignore newru))
    (loop
      when (null (metronome-of fi))
      return nil
      do (setf (values (metronome-of fi) (state-of fi) rule)
	       (cond
		((or (not (eql newst (state-of fi)))
		     (dt> stamp (metronome-of fi)))
		 (metro-round fi))
		((dt= stamp (metronome-of fi))
		 (metro-next fi rule))
		(t
		 (error "state inconsistent ~a < ~a" stamp (metronome-of fi)))))
      unless (null (state-of fi))
      return (values (metronome-of fi) (state-of fi) rule
		     (get-end (slot-value rule 'next))))))

(provide "family")
