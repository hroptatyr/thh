(defpackage :thhrule
  (:use :cl)
  (:export
   #:make-date
   #:defrule
   #:defrule/once
   #:defrule/yearly))

(in-package :thhrule)

(defconstant *day-of-week* '(mon tue wed thu fri sat sun))
(deftype dow () `(member ,@*day-of-week*))

(defclass date ()
  ((year
    :initarg :year
    :reader get-year
    :writer set-year
    :type fixnum)
   (mon
    :initarg :mon
    :reader get-mon
    :writer set-mon
    :initform 1
    :type fixnum)
   (dom
    :initarg :dom
    :reader get-dom
    :writer set-dom
    :initform 1
    :type fixnum)
   (dow
    :initarg :dow
    :reader get-dow
    :type dow)
   (hour
    :initarg :hour
    :reader get-hour
    :writer set-hour
    :initform 0
    :type fixnum)
   (min
    :initarg :min
    :reader get-min
    :writer set-min
    :initform 0
    :type fixnum)
   (sec
    :initarg :sec
    :reader get-sec
    :writer set-sec
    :initform 0
    :type fixnum)
   (unix
    :initarg :unix
    :reader get-unix
    :writer set-unix
    :type fixnum)))

(defun make-date (&key year (mon 1) (dom 1) (hour 0) (min 0) (sec 0) unix)
  (let ((unix (or unix
		  (encode-universal-time sec min hour dom mon year))))
    (multiple-value-bind (ts tm th dd dm dy dow dst-p tz)
	(decode-universal-time unix)
      (make-instance 'date
		     :unix unix
		     :year dy
		     :mon dm
		     :dom dd
		     :dow (nth dow *day-of-week*)
		     :hour th
		     :min tm
		     :sec ts))))

(defmethod print-object ((d date) out)
  (print-unreadable-object (d out :type t)
    (format out
	    "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d :dom ~a :unix ~d"
	    (get-year d) (get-mon d) (get-dom d)
	    (get-hour d) (get-min d) (get-sec d)
	    (get-dow d) (get-unix d))))

(defmethod d= ((d1 date) (d2 date))
  (= (get-unix d1) (get-unix d2)))

(defmethod d> ((d1 date) (d2 date))
  (> (get-unix d1) (get-unix d2)))

(defmethod d>= ((d1 date) (d2 date))
  (or (d> d1 d2)
      (d= d1 d2)))

(defmethod d< ((d1 date) (d2 date))
  (not (d>= d1 d2)))

(defmethod d<= ((d1 date) (d2 date))
  (or (d< d1 d2)
      (d= d1 d2)))

(defmethod d+ ((d date) (inc number))
  (make-date :unix (+ (get-unix d) inc)))

(defmethod d- ((d date) (inc fixnum))
  (make-date :unix (- (get-unix d) inc)))

(defmethod d- ((d1 date) (d2 date))
  (make-date :unix (- (get-unix d1) (get-unix d2))))


;; intervals
(defclass interval ()
  ((start
    :type date
    :reader get-start
    :writer set-start
    :initarg :start)
   (end
    :type date
    :reader get-end
    :writer set-end
    :initarg :end)
   (length
    :type fixnum
    :reader get-length
    :writer set-length
    :initarg :length)))

(defmethod make-interval (&key start end length)
  (make-instance 'interval
		 :start start
		 :end (or end (d+ start length))
		 :length (or length (d- end start))))

(defmethod print-object ((i interval) out)
  (print-unreadable-object (i out :type t)
    (format out
	    "~a - ~a :length ~d"
	    (get-start i)
	    (get-end i)
	    (get-length i))))

(defmethod d> ((i1 interval) (i2 interval))
  (d> (get-start i1) (get-start i2)))

(defmethod containsp ((i interval) (d date))
  (and (d>= d (get-start i))
       (d<= d (get-end i))))

(print (d= (make-date :year 2011) (make-date :year 2011)))

;; thhrule.l ends here
