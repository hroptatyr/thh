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
    :accessor state-of
    :type simple-bit-vector)
   (states
    :initarg :states
    :accessor states-of
    :type simple-vector)
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
	   (sta (destructuring-bind (&key metronome &allow-other-keys) keys
		  (or metronome (compute-initial-metronome old))))
	   ;; compute all states, so we can make a vector for it
	   (states nil)
	   (stv (progn
		  (dolist (r (rules-of old))
		    (pushnew (state-of r) states))
		  (make-array (length states)
			      :element-type 'bit
			      :initial-element 0)))
	   ;; promote 'states to bit-vector
	   (states (map 'simple-vector #'identity states)))
      (make-instance 'famiter
		     :family old :metronome sta
		     :states states :state stv))))

(defmethod print-object ((f famiter) out)
  (print-unreadable-object (f out :type t)
    (format out "~a @~a :state ~a"
	    (name-of (family-of f)) (metronome-of f) (state-of f))))

(provide "family")
