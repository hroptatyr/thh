(defpackage :thhrule
  (:use :cl)
  (:export
   #:make-date
   #:defrule
   #:defrule/once
   #:defrule/yearly))

(in-package :thhrule)

(defconstant *day-names*
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defclass date ()
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
    :type integer)
   (hour
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
   (unix
    :initarg :unix
    :reader get-unix
    :writer set-unix
    :type integer)))

(defmethod reset-unix ((d date))
  (setf (slot-value d 'unix)
	(encode-universal-time (slot-value d 'sec)
			       (slot-value d 'min)
			       (slot-value d 'hour)
			       (slot-value d 'dom)
			       (slot-value d 'mon)
			       (slot-value d 'year))))

(defmethod reset-all ((d date))
  "Assumes unix is correct, set all other slots"
  (slot-makunbound d 'dow)
  (multiple-value-bind (ts tm th dd dm dy dow dst-p tz)
      (decode-universal-time (slot-value d 'unix))
    (setf (slot-value d 'year) dy)
    (setf (slot-value d 'mon) dm)
    (setf (slot-value d 'dom) dd)
    (setf (slot-value d 'dow) dow)
    (setf (slot-value d 'hour) th)
    (setf (slot-value d 'min) tm)
    (setf (slot-value d 'sec) ts)))

(defmacro make-date (&rest init)
  `(let ((d (make-instance 'date ,@init)))
     (unless (slot-boundp d 'unix)
       (reset-unix d))
     d))

(defmethod get-unix ((d date))
  ;;(slot-value d 'unix)
  0)

(defmethod set-unix ((d date) unix)
  ;; setter
  (prog1
      (setf (slot-value d 'unix) unix)
    (reset-all d)))

(defmethod get-dow ((d date))
  (cond
   ((slot-boundp d 'dow)
    (slot-value d 'dow))
   ((slot-boundp d 'unix)
    (reset-all d)
    (slot-value d 'dow))))

(defmethod str-dow ((d date))
  (nth (get-dow d) *day-names*))

(defmethod dow-p ((d date) dow)
  (equal (nth (get-dow d) *day-names*) dow))

(defmethod dow-sun-p ((d date))
  (dow-p d "Sun"))

(defmethod dow-sat-p ((d date))
  (dow-p d "Sat"))

(defmethod dow-mon-p ((d date))
  (dow-p d "Mon"))

(defmethod dow-tue-p ((d date))
  (dow-p d "Tue"))

(defmethod dow-wed-p ((d date))
  (dow-p d "Wed"))

(defmethod dow-thu-p ((d date))
  (dow-p d "Thu"))

(defmethod dow-fri-p ((d date))
  (dow-p d "Fri"))

(defmethod get-year ((d date))
  (cond
   ((slot-boundp d 'year)
    (slot-value d 'year))))

(defmethod set-year ((d date) year)
  ;; invalidate others
  (slot-makunbound d 'dow)
  ;; setter
  (prog1
      (setf (slot-value d 'year) year)
    (reset-unix d)))

(defmethod get-mon ((d date))
  (cond
   ((slot-boundp d 'mon)
    (slot-value d 'mon))))

(defmethod set-mon ((d date) mon)
  ;; invalidate others
  (slot-makunbound d 'dow)
  ;; setter
  (prog1
      (setf (slot-value d 'mon) mon)
    (reset-unix d)))

(defmethod get-dom ((d date))
  (cond
   ((slot-boundp d 'dom)
    (slot-value d 'dom))))

(defmethod set-dom ((d date) dom)
  ;; invalidate others
  (slot-makunbound d 'dow)
  ;; setter
  (prog1
      (setf (slot-value d 'dom) dom)
    (reset-unix d)))

(defmethod get-hour ((d date))
  (cond
   ((slot-boundp d 'hour)
    (slot-value d 'hour))
   ((slot-boundp d 'unix)
    (truncate (/ (mod (slot-value d 'unix) 86400) 3600)))))

(defmethod set-hour ((d date) hour)
  ;; setter
  (prog1
      (setf (slot-value d 'hour) hour)
    (reset-unix d)))

(defmethod get-min ((d date))
  (cond
   ((slot-boundp d 'min)
    (slot-value d 'min))
   ((slot-boundp d 'unix)
    (truncate (/ (mod (slot-value d 'unix) 3600) 60)))))

(defmethod set-min ((d date) min)
  ;; setter
  (prog1
      (setf (slot-value d 'min) min)
    (reset-unix d)))

(defmethod get-sec ((d date))
  (cond
   ((slot-boundp d 'sec)
    (slot-value d 'sec))
   ((slot-boundp d 'unix)
    (mod (slot-value d 'unix) 60))))

(defmethod set-sec ((d date) sec)
  ;; setter
  (prog1
      (setf (slot-value d 'sec) sec)
    (reset-unix d)))

(defmethod print-object ((d date) out)
  (if (slot-boundp d 'unix)
      (reset-all d))
  (let ((dy (or (get-year d) 0))
	(dm (or (get-mon d) 0))
	(dd (or (get-dom d) 0))
	(th (or (get-hour d) 0))
	(tm (or (get-min d) 0))
	(ts (or (get-sec d) 0))
	(u (or (get-unix d) 0)))
    (print-unreadable-object (d out :type t)
      (format out
	      "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d :dom ~a :unix ~d"
	      dy dm dd
	      th tm ts
	      (str-dow d)
	      u))))

(defmethod d> ((d1 date) (d2 date))
  (> (get-unix d1) (get-unix d2)))

(defmethod d+ ((d date) (inc number))
  (make-date :unix (+ (get-unix d) inc)))

(defmethod d- ((d date) (inc integer))
  (make-date :unix (- (get-unix d) inc)))

(defmethod d- ((d1 date) (d2 date))
  (make-date :unix (- (get-unix d1) (get-unix d2))))


;; states
(deftype state () '(market-open market-close))


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
    :type integer
    :reader get-length
    :writer set-length
    :initarg :length)))

(defmethod make-interval (&key start end length)
  (make-instance 'interval
		 :start start
		 :end (or end (d+ start length))
		 :length (or length (d- end start))))

(print (make-interval :start (make-date :year 12) :length 14))


(defclass thhcal ()
  ())


(defclass thhrule ()
  ())

(defmacro defrule
  (name (arg &key (from (make-date :year 0)) (till (make-date :year 9999)))
	&body body)
  `(defun ,name (,arg)
     (if (d> ,arg ,till)
	 nil
       (scons (multiple-value-list ,@body) nil))))

(defmacro defrule/yearly
  (name (arg &key (from (make-date :year 0)) (till (make-date :year 9999)))
	&body body)
  `(defrule ,name (,arg :from ,from :till ,till)
     ,@body))


;;; tests
;;(defrule/once new-year-2011 ())
;;
;; (defrule/yearly boxing-day (d)
;;   (values (make-date :year (get-year d) :mon 12 :dom 26) 86400 :market-close))
;; 
;; (defun in-lieu (date)
;;   "Move DATE to the monday afterwards"
;;   (cond
;;    ((dow-sat-p date)
;;     (setf date (d+ date (* 2 86400))))
;;    ((dow-sun-p date)
;;     (setf date (d+ date 86400)))
;;    (t date)))
;; 
;; (defrule/yearly xmas-in-lieu (d :from 1970)
;;   (let ((try (make-date :year (get-year d) :mon 12 :dom 25))
;; 	(reason 'xmas))
;;     (cond
;;      ((or (dow-sat-p try) (dow-sun-p try))
;;       ;; in lieu case
;;       (set-dom try 27)
;;       (setq reason 'xmas-in-lieu)))
;;     (values try 86400 :market-close :reason reason)))
;; 
;; (defrule/yearly boxing-day-in-lieu (d)
;;   (let ((try (make-date :year (get-year d) :mon 12 :dom 26))
;; 	(reason 'xmas))
;;     (cond
;;      ((or (dow-sat-p try) (dow-sun-p try)
;;       (set-dom try 28)
;;       (setq reason 'boxing-day-in-lieu))))
;;     (values try 86400 :market-close :reason reason)))

;; thhrule.l ends here
