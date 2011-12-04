;;; state -- notion of market states
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
(in-package :thhrule)

(defclass state ()
  ((name
    :initarg :name
    :accessor name-of)
   (markets
    :initarg :markets
    :initform nil
    :accessor markets-of
    :type (list market))
   (rules
    :initarg :rules
    :initform nil
    :accessor rules-of
    :type (list rule))
   (implies
    :initarg :implies
    :initform nil
    :accessor implications-of
    :type (list state))
   (inhibits
    :initarg :inhibits
    :initform nil
    :accessor inhibitions-of
    :type (list state))))

(defun make-state (&rest v+k)
  (multiple-value-bind (vals keys) (split-vals+keys v+k)
    (apply #'make-instance 'state keys)))

(defmethod print-object ((s state) out)
  (print-unreadable-object (s out :type t)
    (format out "~a" (name-of s))))

(defgeneric state-inhibits (s &rest states))
(defmethod state-inhibits ((s state) &rest states)
  (pushnew-many (inhibitions-of s) states))

(defgeneric state-implies (s &rest states))
(defmethod state-implies ((s state) &rest states)
  (pushnew-many (implications-of s) states))

(defgeneric state-add-markets (s &rest markets))
(defmethod state-add-markets ((s state) &rest markets)
  (pushnew-many (markets-of s) markets))

(defmethod push-rule ((s state) r)
  (pushnew-many (rules-of s) (list r)))

(defmacro defstate (name &rest v+k)
  `(let ((st (make-state ,@v+k :name ',name)))
     ;; convenience
     (defrule-macros ,name :state st :push-obj st)

     ;; stuff that needs to close over ST
     (defun ,(sym-conc name '-inhibits) (&rest states)
       (apply #'state-inhibits st states))

     (defun ,(sym-conc name '-implies) (&rest states)
       (apply #'state-implies st states))

     (defun ,(sym-conc name '-add-markets) (&rest markets)
       (apply #'state-add-markets st markets))

     ;; and finally inject to environ
     (defvar ,name st)))

(provide "state")
