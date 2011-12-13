;;; market -- notion of markets
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

(defclass market (family)
  ((products
    :initarg :products
    :initform nil
    :accessor products-of)
   (states
    :initarg :states
    :initform nil
    :accessor states-of)))

(defun make-market (&rest v+k)
  (multiple-value-bind (vals keys) (split-vals+keys v+k)
    (declare (ignore vals))
    (apply #'make-instance 'market :allow-other-keys t keys)))

;; no print-object method, fall back to family printer

(defgeneric market-add-states (m &rest states))
(defmethod market-add-states ((m market) &rest states)
  (pushnew-many (states-of m) states))

(defgeneric market-add-products (m &rest products))
(defmethod market-add-products ((m market) &rest products)
  (pushnew-many (products-of m) products))

(defgeneric market-add-rules (m &rest rules))
(defmethod market-add-rules ((m market) &rest rules)
  (pushnew-many (rules-of m) rules))

(defmethod push-rule ((m market) rule)
  (market-add-rules m rule))

(defmacro defmarket (name &rest v+k)
  `(let ((mkt (make-market ,@v+k :name ',name))
	 (rules))

     ;; convenience
     (defrule-macros ,name :push-obj mkt)

     ;; stuff that makes sense in conjunction with state or product
     (defmacro ,(sym-conc 'def name '-product) (name &rest v+k)
       (let ((mkt/prod (sym-conc ',name '/ name)))
	 `(prog1
	      (defproduct ,mkt/prod ,@v+k)
	    (product-add-markets ,mkt/prod ,,name)
	    (market-add-products ,,name ,mkt/prod))))
     (defmacro ,(sym-conc 'def name '-state) (name &rest v+k)
       (let ((mkt/st (sym-conc ',name '/ name)))
	 `(prog1
	      (defstate ,mkt/st ,@v+k)
	    (state-add-markets ,mkt/st ,,name)
	    (market-add-states ,,name ,mkt/st))))
     ;; stuff that needs to close over ST
     (defun ,(sym-conc name '-add-states) (&rest states)
       (apply #'market-add-states mkt states))
     (defun ,(sym-conc name '-add-products) (&rest products)
       (apply #'market-add-products mkt products))

     ;; and finally inject to environ
     (defvar ,name mkt)))

(provide "market")
