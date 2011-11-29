;; predef -- rules that need computation
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

(require "util")
(in-package :thhrule)

(defun calc-gregorian-easter (year)
  (let* ((a (mod year 19))
	 (b (floor year 4))
	 (c (1+ (floor b 25)))
	 (d (floor (* c 3) 4))
	 (e (mod (+ (* a 19) (- (floor (+ (* c 8) 5) 25)) d 15) 30)))
    (setq e (+ e (floor (- 29578 a (* e 32)) 1024)))
    (- e (mod (+ (mod year 7) b (- d) e 2) 7))))

(defun calc-julian-easter (year)
  (let* ((a (mod year 19))
	 (b (mod year 4))
	 (c (mod year 7))
	 (M 15)
	 (N 6)
	 (d (mod (+ (* 19 a) M) 30))
	 (e (mod (+ (* 2 b) (* 4 c) (* 6 d) N) 7)))
    (+ 22 d e
       ;; this is to express things in gregorian
       13)))

(defmacro datify-easter (what &key (add 0) as)
  (flet ((sym-conc (&rest syms)
	   (intern (apply #'concatenate 'string (mapcar #'symbol-name syms)))))
    `(defun ,as (year)
       (let* ((e (,(sym-conc 'calc- what '-easter) year))
	      (d (if (< e 32) 3 4)))
	 (d+ (make-date :year year :mon d :dom (if (= d 3) e (- e 31))) ,add)))))

;; convenience funs
(datify-easter gregorian :as gregorian-easter)
(datify-easter gregorian :add -2 :as gregorian-good-friday)
(datify-easter gregorian :add 1 :as gregorian-easter/mon)
(datify-easter julian :as julian-easter)
(datify-easter julian :add -2 :as julian-good-friday)
(datify-easter julian :add 1 :as julian-easter/mon)

(datify-easter gregorian :add 50 :as gregorian-whit-monday)
(datify-easter gregorian :add 49 :as gregorian-whit-sunday)
(datify-easter gregorian :add 39 :as gregorian-ascension)
(datify-easter gregorian :add 56 :as gregorian-trinity-sunday)
(datify-easter gregorian :add 60 :as gregorian-corpus-christi)

(provide :thhrule.predef)
(provide "predef")

;; predef.lisp ends here
