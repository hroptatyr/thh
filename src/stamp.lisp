;;; stamp -- basic time and date classes
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
(in-package :stamp)

(defparameter +day-of-week+
  '(mon tue wed thu fri sat sun))
(defparameter +month-of-year+
  '(mir jan feb mar apr may jun jul aug sep oct nov dec))

(deftype dow () `(member ,@+day-of-week+))

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970))

(defclass stamp ()
  ((unix
    :initarg :unix
    :reader get-unix
    :writer set-unix
    :type integer)
   (step
    :reader get-step
    :initform 86400)))

(defclass date (stamp)
  ((year
    :initarg :year
    :reader get-year
    :writer set-year
    :type integer)
   (mon
    :initarg :mon
    :reader get-mon
    :writer set-mon
    :initform 1
    :type integer)
   (dom
    :initarg :dom
    :reader get-dom
    :writer set-dom
    :initform 1
    :type integer)
   (dow
    :initarg :dow
    :reader get-dow
    :type dow)
   (step
    :reader get-step
    :initform 86400)))

(defclass tod (stamp)
  ((hour
    :initarg :hour
    :reader get-hour
    :writer set-hour
    :initform 0
    :type integer)
   (min
    :initarg :min
    :reader get-min
    :writer set-min
    :initform 0
    :type integer)
   (sec
    :initarg :sec
    :reader get-sec
    :writer set-sec
    :initform 0
    :type integer)
   (step
    :reader get-step
    :initform 1)))

(defclass datetime (date tod)
  ((step
    :reader get-step
    :initform 1)))

(defmethod fixup-human ((u integer) (s stamp)))

(defmethod fixup-human ((u integer) (d date))
  (multiple-value-bind (ts tm th dd dm dy wd)
      (decode-universal-time u)
    (declare (ignore ts tm th))
    (with-slots (year mon dom dow) d
      (setf year dy)
      (setf mon dm)
      (setf dom dd)
      (setf dow (nth wd +day-of-week+)))))

(defmethod fixup-human ((u integer) (tm tod))
  (let (s m h)
    (multiple-value-setq (u s) (floor u 60))
    (multiple-value-setq (u m) (floor u 60))
    (multiple-value-setq (u h) (floor u 24))
    (with-slots (sec min hour) tm
      (setf sec s)
      (setf min m)
      (setf hour h))))

(defmethod fixup-human ((u integer) (dt datetime))
  (multiple-value-bind (ts tm th dd dm dy wd)
      (decode-universal-time u)
    (with-slots (year mon dom dow hour min sec) dt
      (setf year dy)
      (setf mon dm)
      (setf dom dd)
      (setf dow (nth wd +day-of-week+))
      (setf hour th)
      (setf min tm)
      (setf sec ts))))

(defmethod fixup-stamp ((s stamp))
  (print "wrong fixup"))

(defmethod fixup-stamp ((d date))
  (with-slots (dom mon year unix) d
    (let ((u (encode-universal-time 0 0 0 dom mon year)))
      (setf unix u))))

(defmethod fixup-stamp ((tm tod))
  (with-slots (hour min sec unix) tm
    (let ((u (+ sec
		(* min 60)
		(* hour 3600))))
      (setf unix u))))

(defmethod fixup-stamp ((dt datetime))
  (with-slots (dom mon year sec min hour unix) dt
    (let ((u (encode-universal-time sec min hour dom mon year)))
      (setf unix u))))

(defmethod fixup-stamp :after ((d date))
  "Fix up the day of the week too."
  (with-slots (dow unix) d
    ;; veeeeery illegal computation
    (let* ((daisy (floor unix 86400))
	   (dm (mod daisy 7)))
      (setf dow (nth dm +day-of-week+)))))

(defmethod set-year :after (dummy (d date))
  (declare (ignore dummy))
  (fixup-stamp d))

(defmethod set-mon :after (dummy (d date))
  (declare (ignore dummy))
  (fixup-stamp d))

(defmethod set-dom :after (dummy (d date))
  (declare (ignore dummy))
  (fixup-stamp d))

(defmethod set-hour :after (dummy (tm tod))
  (declare (ignore dummy))
  (fixup-stamp tm))

(defmethod set-min :after (dummy (tm tod))
  (declare (ignore dummy))
  (fixup-stamp tm))

(defmethod set-sec :after (dummy (tm tod))
  (declare (ignore dummy))
  (fixup-stamp tm))

(defmethod set-unix :after (unix (s stamp))
  (fixup-human unix s))

(defmethod get-year ((s stamp))
  ;; temporarily promote
  (multiple-value-bind (ts tm th dd dm dy wd)
      (decode-universal-time (get-unix s))
    (declare (ignore ts tm th dd dm wd))
    dy))

(defmethod get-mon ((s stamp))
  ;; temporarily promote
  (multiple-value-bind (ts tm th dd dm dy wd)
      (decode-universal-time (get-unix s))
    (declare (ignore ts tm th dd dy wd))
    dm))

(defmethod get-dom ((s stamp))
  ;; temporarily promote
  (multiple-value-bind (ts tm th dd dm dy wd)
      (decode-universal-time (get-unix s))
    (declare (ignore ts tm th dm dy wd))
    dd))

(defmethod get-hour ((s (eql 'stamp)))
  (floor (mod s 86400) 3600))

(defmethod get-min ((s (eql 'stamp)))
  (floor (mod s 3600) 60))

(defmethod get-sec ((s (eql 'stamp)))
  (mod s 60))

(defun make-stamp (&key what unix)
  (let ((u (make-instance (or what 'stamp) :unix unix)))
    (fixup-human unix u)
    u))

(defun get-mon/num (mon)
  (cond
   ((stringp mon)
    (position (intern mon 'stamp) +month-of-year+))
   ((symbolp mon)
    (position (intern (symbol-name mon) 'stamp) +month-of-year+))
   ((numberp mon)
    mon)))

(defun get-dow/sym (dow)
  (if (numberp dow)
      (nth dow +day-of-week+)
    (intern (cond
	     ((symbolp dow)
	      (symbol-name dow))
	     ((stringp dow)
	      dow)
	     (t
	      "MIR"))
	    'stamp)))

(defun get-dow/num (dow)
  (if (numberp dow)
      dow
    (position (get-dow/sym dow) +day-of-week+)))

(defun make-date (&key year (mon 1) (dom 1) unix)
  (cond
   (unix
    (make-stamp :what 'date :unix unix))
   (year
    (let* ((mon/num (get-mon/num mon))
	   (d (make-instance 'date :year year :mon mon/num :dom dom)))
      (fixup-stamp d)
      d))))

(defun make-time (&key (hour 0) (min 0) (sec 0) unix)
  (cond
   (unix
    (make-stamp :what 'tod :unix unix))
   (t
    (let ((tm (make-instance 'tod :hour hour :min min :sec sec)))
      (fixup-stamp tm)
      tm))))

(defun make-datetime (&key year (mon 1) (dom 1) (hour 0) (min 0) (sec 0) unix)
  (cond
   (unix
    (make-stamp :what 'datetime :unix unix))
   (year
    (let* ((mon/num (get-mon/num mon))
	   (dt (make-instance 'datetime
			      :year year :mon mon/num :dom dom
			      :hour hour :min min :sec sec)))
      (fixup-stamp dt)
      dt))))

(defmethod print-object ((s stamp) out)
  (with-slots (unix) s
    (print-unreadable-object (s out :type t)
      (format out ":unix ~d" unix))))

(defmethod print-object ((d date) out)
  (with-slots (dom mon year dow unix) d
    (print-unreadable-object (d out :type t)
      (format out
	      "~4,'0d-~2,'0d-~2,'0d :dow ~a :unix ~d"
	      year mon dom dow unix))))

(defmethod print-object ((tm tod) out)
  (with-slots (hour min sec unix) tm
    (print-unreadable-object (tm out :type t)
      (format out
	      "~2,'0d:~2,'0d:~2,'0d :unix ~d"
	      hour min sec unix))))

(defmethod print-object ((dt datetime) out)
  (with-slots (dom mon year dow unix hour min sec) dt
    (print-unreadable-object (dt out :type t)
      (format out
	      "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d :dow ~a :unix ~d"
	      year mon dom hour min sec dow unix))))

;; arithmetic
(defmethod d= ((s1 stamp) (s2 stamp))
  (let ((step (max (get-step s1) (get-step s2))))
    (= (floor (get-unix s1) step) (floor (get-unix s2) step))))

(defmethod d> ((s1 stamp) (s2 stamp))
  (let ((step (max (get-step s1) (get-step s2))))
    (> (floor (get-unix s1) step) (floor (get-unix s2) step))))

(defmethod d>= (s1 s2)
  (or (d> s1 s2)
      (d= s1 s2)))

(defmethod d< (s1 s2)
  (not (d>= s1 s2)))

(defmethod d<= (s1 s2)
  (or (d< s1 s2)
      (d= s1 s2)))

(defmethod dt= ((s1 stamp) (s2 stamp))
  (= (get-unix s1) (get-unix s2)))

(defmethod dt> ((s1 stamp) (s2 stamp))
  (> (get-unix s1) (get-unix s2)))

(defmethod dt>= (s1 s2)
  (or (dt= s1 s2)
      (dt> s1 s2)))

(defmethod dt< (s1 s2)
  (not (dt>= s1 s2)))

(defmethod dt<= (s1 s2)
  (or (dt= s1 s2)
      (dt< s1 s2)))

(defmethod d+ ((s stamp) (inc number))
  (make-stamp :what (type-of s) :unix (+ (get-unix s) (* (get-step s) inc))))

(defmethod d- ((s stamp) (inc integer))
  (d+ s (- inc)))

(defmethod d- ((s1 stamp) (s2 stamp))
  (let ((step (max (get-step s1) (get-step s2))))
    (floor (- (get-unix s1) (get-unix s2)) step)))

(defgeneric consecutivep (thing1 thing2)
  (:documentation "Whether THING2 follows THING1."))

(defmethod consecutivep ((s1 stamp) (s2 stamp))
  "Return non-NIL when there is no further points between S1 and S2."
  (<= (abs (d- s1 s2)) 1))


;; intervals
(defclass interval ()
  ((start
    :type date
    :accessor start-of
    :initarg :start)
   (end
    :type date
    :accessor end-of
    :initarg :end)
   (length
    :type integer
    :reader length-of
    :initarg :length)))

(defun make-interval (&key start end length)
  (make-instance 'interval
		 :start start
		 :end (or end (d+ start length))
		 :length (or length (d- end start))))

(defmethod (setf start-of) :after (s (i interval))
  "Update length slot."
  (setf (slot-value i 'length) (d- (end-of i) s)))

(defmethod (setf end-of) :after (e (i interval))
  "Update length slot."
  (setf (slot-value i 'length) (d- e (start-of i))))

(defmethod print-object ((i interval) out)
  (with-slots (start end length) i
    (print-unreadable-object (i out :type t)
      (format out
	      "~a - ~a :length ~d"
	      start end length))))

(defmethod i= ((i1 interval) (i2 interval))
  (and (d= (start-of i1) (start-of i2))
       (d= (end-of i1) (end-of i2))))

(defmethod d= ((i1 interval) (i2 interval))
  (d= (start-of i1) (start-of i2)))

(defmethod d> ((i1 interval) (i2 interval))
  (d> (start-of i1) (start-of i2)))

(defmethod dt= ((i1 interval) (i2 interval))
  (dt= (start-of i1) (start-of i2)))

(defmethod dt> ((i1 interval) (i2 interval))
  (dt> (start-of i1) (start-of i2)))


(defgeneric containsp (thing1 thing2)
  (:documentation "Whether THING1 contains THING2 in some sense"))

(defmethod containsp ((i interval) (s stamp))
  "Return non-NIL when I contains S."
  (and (d>= s (start-of i))
       (d<= s (end-of i))))

(defmethod containsp ((i1 interval) (i2 interval))
  "Return non-NIL when I1 contains I2."
  (and (containsp i1 (start-of i2))
       (containsp i1 (end-of i2))))

(defgeneric connectedp (thing1 thing2)
  (:documentation "Whether things are connected in some sense."))

(defmethod connectedp ((i interval) (s stamp))
  "Return non-NIL when there is no datetime point between I and S."
  (or (containsp i s)
      (consecutivep (start-of i) s)
      (consecutivep (end-of i) s)))

(defmethod connectedp ((i1 interval) (i2 interval))
  "Return non-NIL when there is no datetime point between I1 and I2."
  (or (connectedp i1 (start-of i2))
      (connectedp i1 (end-of i2))))

(provide :stamp)
(provide "stamp")
