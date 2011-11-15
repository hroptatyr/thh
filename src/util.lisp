(in-package :thhrule)

(defconstant +day-of-week+ '(mon tue wed thu fri sat sun))
(deftype dow () `(member ,@+day-of-week+))

(defclass datetime ()
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

(defmethod set-unix :after ((u integer) (d datetime))
  (multiple-value-bind (ts tm th dd dm dy dow dst-p tz)
      (decode-universal-time u)
    (setf (slot-value d 'year) dy)
    (setf (slot-value d 'mon) dm)
    (setf (slot-value d 'dom) dd)
    (setf (slot-value d 'dow) (nth dow +day-of-week+))
    (setf (slot-value d 'hour) th)
    (setf (slot-value d 'min) tm)
    (setf (slot-value d 'sec) ts)))

(defmethod fixup-datetime ((d datetime))
  (let ((unix (encode-universal-time
	       (get-sec d)
	       (get-min d)
	       (get-hour d)
	       (get-dom d)
	       (get-mon d)
	       (get-year d))))
    (set-unix unix d)))

(defmethod set-year :after (dummy (d datetime))
  (fixup-datetime d))

(defmethod set-mon :after (dummy (d datetime))
  (fixup-datetime d))

(defmethod set-dom :after (dummy (d datetime))
  (fixup-datetime d))

(defmethod set-hour :after (dummy (d datetime))
  (fixup-datetime d))

(defmethod set-min :after (dummy (d datetime))
  (fixup-datetime d))

(defmethod set-sec :after (dummy (d datetime))
  (fixup-datetime d))

(defun make-datetime (&key year (mon 1) (dom 1) (hour 0) (min 0) (sec 0) unix)
  (let ((unix (or unix
		  (encode-universal-time sec min hour dom mon year)))
	(d (make-instance 'datetime)))
    (set-unix unix d)
    d))

(defmethod print-object ((d datetime) out)
  (print-unreadable-object (d out :type t)
    (format out
	    "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d :dom ~a :unix ~d"
	    (get-year d) (get-mon d) (get-dom d)
	    (get-hour d) (get-min d) (get-sec d)
	    (get-dow d) (get-unix d))))

(defmethod d= ((d1 datetime) (d2 datetime))
  (= (get-unix d1) (get-unix d2)))

(defmethod d> ((d1 datetime) (d2 datetime))
  (> (get-unix d1) (get-unix d2)))

(defmethod d>= ((d1 datetime) (d2 datetime))
  (or (d> d1 d2)
      (d= d1 d2)))

(defmethod d< ((d1 datetime) (d2 datetime))
  (not (d>= d1 d2)))

(defmethod d<= ((d1 datetime) (d2 datetime))
  (or (d< d1 d2)
      (d= d1 d2)))

(defmethod d+ ((d datetime) (inc number))
  (make-datetime :unix (+ (get-unix d) inc)))

(defmethod d- ((d datetime) (inc integer))
  (make-datetime :unix (- (get-unix d) inc)))

(defmethod d- ((d1 datetime) (d2 datetime))
  (make-datetime :unix (- (get-unix d1) (get-unix d2))))

(defmethod consecutivep ((d1 datetime) (d2 datetime))
  "Return non-NIL when there is no further datetime between D1 and D2."
  (<= (abs (- (get-unix d1) (get-unix d2))) 1))


;; intervals
(defclass interval ()
  ((start
    :type datetime
    :reader get-start
    :writer set-start
    :initarg :start)
   (end
    :type datetime
    :reader get-end
    :writer set-end
    :initarg :end)
   (length
    :type integer
    :reader get-length
    :initarg :length)))

(defmethod make-interval (&key start end length unit)
  (make-instance 'interval
		 :start start
		 :end (or end (d+ start length))
		 :length (or length (d- end start))))

(defmethod set-start :after (s (i interval))
  "Updatetime length slot."
  (setf (slot-value i 'length) (d- (get-end i) s)))

(defmethod set-end :after (e (i interval))
  "Updatetime length slot."
  (setf (slot-value i 'length) (d- e (get-start i))))

(defmethod print-object ((i interval) out)
  (print-unreadable-object (i out :type t)
    (format out
	    "~a - ~a :length ~d"
	    (get-start i)
	    (get-end i)
	    (get-length i))))

(defmethod d> ((i1 interval) (i2 interval))
  (d> (get-start i1) (get-start i2)))

(defmethod containsp ((i interval) (d datetime))
  "Return non-NIL when I contains D."
  (and (d>= d (get-start i))
       (d<= d (get-end i))))

(defmethod containsp ((i1 interval) (i2 interval))
  "Return non-NIL when I1 contains I2."
  (and (containsp i1 (get-start i2))
       (containsp i1 (get-end i2))))

(defmethod connectedp ((i interval) (d datetime))
  "Return non-NIL when there is no datetime point between I and D."
  (or (containsp i d)
      (consecutivep (get-start i) d)
      (consecutivep (get-end i) d)))

(defmethod connectedp ((i1 interval) (i2 interval))
  "Return non-NIL when there is no datetime point between I1 and I2."
  (or (connectedp i1 (get-start i2))
      (connectedp i1 (get-end i2))))

(provide :thhrule.util)
