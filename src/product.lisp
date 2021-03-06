;;; product -- notion of products
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
(require "family")
(in-package :thhrule)

(defclass product (family)
  ((markets
    :initarg :markets
    :initform nil
    :accessor markets-of)
   (states
    :initarg :states
    :initform nil
    :accessor states-of)))

(defun make-product (&rest v+k)
  (multiple-value-bind (vals keys) (split-vals+keys v+k)
    (declare (ignore vals))
    (apply #'make-instance 'product :allow-other-keys t keys)))

;; no print-object method, fall back to family printer

(defgeneric product-add-markets (p &rest markets))
(defmethod product-add-markets ((p product) &rest markets)
  (loop
    for m in markets
    do (pushnew m (markets-of p))))

(defgeneric product-add-states (p &rest states))
(defmethod product-add-states ((p product) &rest states)
  (loop
    for s in states
    do (pushnew s (states-of p))))

(defgeneric product-add-rules (p &rest rules))
(defmethod product-add-rules ((p product) &rest rules)
  (loop
    for r in rules
    do (pushnew r (rules-of p))))

(defmethod push-rule ((p product) r)
  (product-add-rules p r))

(defmacro defproduct (name &rest v+k)
  `(let ((prod (make-product ,@v+k :name ',name)))
     ;; convenience
     (defrule-macros ,name :push-obj prod)

     ;; stuff that needs to close over PROD
     (defun ,(sym-conc name '-add-markets) (&rest markets)
       (apply #'product-add-markets prod markets))
     ;; and finally inject to environ
     (defvar ,name prod)))

(provide "product")
