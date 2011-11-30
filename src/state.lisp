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

(defmacro nconc-or-setf-or-leave-t (val &rest more)
  `(cond
    ((eql ,val t)
     t)
    (,val
     (nconc ,val ,@more))
    (t
     (setf ,val ,@more))))

(defmacro defstate (name &rest v+k)
  `(let ((st (make-state ,@v+k)))
     ;; stuff that needs to close over ST
     (defun ,(sym-conc name '-inhibits) (&rest states)
       (nconc-or-setf-or-leave-t
	(inhibitions-of st)
	(mapcar #'var-or-sym-value states)))
     (defun ,(sym-conc name '-implies) (&rest states)
       (nconc-or-setf-or-leave-t
	(implications-of st)
	(mapcar #'var-or-sym-value states)))
     (defun ,(sym-conc name '-add-markets) (&rest markets)
       (nconc-or-setf-or-leave-t
	(markets-of st)
	(mapcar #'var-or-sym-value markets)))
     ;; and finally inject to environ
     (defvar ,name st)))

(provide "state")
