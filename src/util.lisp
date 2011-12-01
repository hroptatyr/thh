;; util -- basic time and date classes
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
(in-package :thhrule)


;; cybertiggyr glue
(defparameter +date-recognisers+
  (list (make-fmt-recognizer "%Y-%m-%d")
	(make-fmt-recognizer "%Y-%b-%d")
	(make-fmt-recognizer "%Y-%m")
	(make-fmt-recognizer "%Y-%b")
	(make-fmt-recognizer "%Y")))

(defparameter +time-recognisers+
  (list (make-fmt-recognizer "%H:%M:%S")
	(make-fmt-recognizer "%H:%M:%S%Z")))

(defparameter +datetime-recognisers+
  (list (make-fmt-recognizer "%Y-%m-%dT%H:%M:%S")
	(make-fmt-recognizer "%Y-%m-%dT%H:%M:%S%Z")
	(make-fmt-recognizer "%Y-%B-%dT%H:%M:%S")
	(make-fmt-recognizer "%Y-%B-%dT%H:%M:%S%Z")))

(defun parse-time/glue-all (str)
  (let (u)
    (cond
     ((setq u (cybertiggyr-time::parse-time str +datetime-recognisers+))
      (make-datetime :unix u))
     ((setq u (cybertiggyr-time::parse-time str +date-recognisers+))
      (make-date :unix u))
     ((setq u (cybertiggyr-time::parse-time str +time-recognisers+))
      (make-time :unix u))
     ((setq u (cybertiggyr-time::parse-time str))
      (make-stamp :unix u)))))

(defun parse-time/glue-date (str)
  (let (u)
    (cond
     ((setq u (cybertiggyr-time::parse-time str +date-recognisers+))
      (make-date :unix u)))))

(defun parse-time/glue-time (str)
  (let (u)
    (cond
     ((setq u (cybertiggyr-time::parse-time str +time-recognisers+))
      (make-time :unix u)))))

(defun parse-time/glue-datetime (str)
  (let (u)
    (cond
     ((setq u (cybertiggyr-time::parse-time str +datetime-recognisers+))
      (make-datetime :unix u)))))

(defparameter +dawn-of-time+ (make-stamp :unix 0))
(defparameter +dusk-of-time+ (make-stamp :unix 4294967295))

(defgeneric parse-date (thing))
(defgeneric parse-time (thing))
(defgeneric parse-datetime (thing))
(defgeneric parse-dtall (thing))

(defmethod parse-date ((s string))
  (parse-time/glue-date s))

(defmethod parse-date ((s symbol))
  (parse-time/glue-date (symbol-name s)))

(defmethod parse-date ((s stamp))
  s)

(defmethod parse-time ((s string))
  (parse-time/glue-time s))

(defmethod parse-time ((s symbol))
  (parse-time/glue-time (symbol-name s)))

(defmethod parse-time ((s stamp))
  s)

(defmethod parse-datetime ((s string))
  (parse-time/glue-datetime s))

(defmethod parse-datetime ((s symbol))
  (parse-time/glue-datetime (symbol-name s)))

(defmethod parse-datetime ((s stamp))
  s)

(defmethod parse-dtall ((s string))
  (parse-time/glue-all s))

(defmethod parse-dtall ((s symbol))
  (parse-time/glue-all (symbol-name s)))

(defmethod parse-dtall ((s stamp))
  s)


;; stamp glue, addenda, etc.
(defun max-stamp (a b)
  (if b
      (if (dt> a b)
	  a
	b)
    a))

(defun get-ultimo (year mon)
  "Return ultimo of MON in YEAR."
  (let* ((nmon (1+ mon))
	 (nyear (+ year (floor nmon 12)))
	 (nx (make-date :year nyear :mon nmon :dom 1)))
    (d+ nx -1)))

(defun get-mdays (year mon)
  "Return the number of days in MON of YEAR."
  (let ((ult (get-ultimo year mon)))
    (get-dom ult)))

(defun make-ymcw (&key year mon dow which)
  "Like dateutils' ymcw."
  ;; 	wd01 = (wd_jan01 - 1 + wd01) % 7;
  ;; 
  ;; 	/* first WD1 is 1, second WD1 is 8, third WD1 is 15, etc.
  ;; 	 * so the first WDx with WDx > WD1 is on (WDx - WD1) + 1 */
  ;; 	res = (that.w + 7 - wd01) % 7 + 1 + 7 * (that.c - 1);
  ;; 	/* not all months have a 5th X, so check for this */
  ;; 	if (res > __get_mdays(that.y, that.m)) {
  ;; 		 /* 5th = 4th in that case */
  ;; 		res -= 7;
  ;; 	}
  ;; 	return res;
  (let* ((s (make-date :year year :mon mon :dom 1))
	 (sdow (get-dow/num (get-dow s)))
	 (dow/num (get-dow/num dow))
	 (which/num (cond
		     ((numberp which)
		      which)
		     ((symbolp which)
		      (let ((sym (intern (symbol-name which) 'thhrule)))
			(case sym
			  (1st 1)
			  (2nd 2)
			  (3rd 3)
			  (4th 4)
			  (last 5)
			  (otherwise 0))))))
	 (dom (+ (mod (- (+ dow/num 7) sdow) 7) 1 (* 7 (1- which/num))))
	 (ult (get-mdays year mon)))
    (make-date :year year
	       :mon mon
	       :dom (if (> dom ult)
			(- dom 7)
		      dom))))

(defmethod midnight ((su integer) &optional next)
  (let ((sm (mod su 86400)))
    (+ (- su sm) (or (and next (> sm next) 86400) 0))))

(defmethod midnight ((s stamp) &optional next)
  (midnight (get-unix s) next))


;; other useful stuff
(defun split-vals+keys (list)
  (loop
    with vals = nil
    and keys = nil
    while list
    do (if (keywordp (car list))
	   (setq keys (cons (car list) (cons (cadr list) keys))
		 list (cddr list))
	 (setq vals (cons (car list) vals)
	       list (cdr list)))
    finally (return (values (nreverse vals) keys))))

(defun var-or-sym-value (var-or-sym)
  (if (and (symbolp var-or-sym)
	   (boundp var-or-sym))
      (symbol-value var-or-sym)
    var-or-sym))

(defun var-or-sym-type-p (var-or-sym type)
  (eql (type-of (var-or-sym-value var-or-sym)) type))

(defun sym-conc (&rest syms-or-strings)
  (flet ((sym-or-string-name (s)
	   (cond
	    ((symbolp s)
	     (symbol-name s))
	    ((stringp s)
	     s)
	    (t
	     ""))))
    (intern
     (apply #'concatenate 'string
       (mapcar #'sym-or-string-name syms-or-strings)))))

(defmacro pushnew-many (place list)
  `(setf ,place (union ,place ,list)))


(provide :thhrule.util)
(provide "util")

;; util.lisp ends here
