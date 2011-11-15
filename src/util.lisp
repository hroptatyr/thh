(in-package :thhrule)

(defparameter +day-of-week+ '(mon tue wed thu fri sat sun))
(deftype dow () `(member ,@+day-of-week+))

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
    :type dow)
   (unix
    :initarg :unix
    :reader get-unix
    :writer set-unix
    :type integer)
   (step
    :reader get-step
    :initform 86400)))

(defclass datetime (date)
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
    :initform 1)))

(defun trunc/ (a b)
  (truncate (/ a b)))

(defmethod fixup-date ((u integer) (d date))
  (multiple-value-bind (ts tm th dd dm dy dow dst-p tz)
      (decode-universal-time u)
    (setf (slot-value d 'year) dy)
    (setf (slot-value d 'mon) dm)
    (setf (slot-value d 'dom) dd)
    (setf (slot-value d 'dow) (nth dow +day-of-week+))))

(defmethod fixup-date ((u integer) (d datetime))
  (multiple-value-bind (ts tm th dd dm dy dow dst-p tz)
      (decode-universal-time u)
    (setf (slot-value d 'year) dy)
    (setf (slot-value d 'mon) dm)
    (setf (slot-value d 'dom) dd)
    (setf (slot-value d 'dow) (nth dow +day-of-week+))
    (setf (slot-value d 'hour) th)
    (setf (slot-value d 'min) tm)
    (setf (slot-value d 'sec) ts)))

(defmethod fixup-unix ((d date))
  (with-slots (dom mon year) d
    (let ((unix (encode-universal-time
		 0 0 0
		 dom mon year)))
      (setf (slot-value d 'unix) unix))))

(defmethod fixup-unix ((d datetime))
  (with-slots (dom mon year sec min hour) d
    (let ((unix (encode-universal-time
		 sec min hour
		 dom mon year)))
      (setf (slot-value d 'unix) unix))))

(defmethod fixup-unix :after ((d date))
  "Fix up the day of the week too."
  (with-slots (unix) d
    ;; veeeeery illegal computation
    (let* ((daisy (trunc/ unix 86400))
	   (dow (mod daisy 7)))
      (setf (slot-value d 'dow) (nth dow +day-of-week+)))))

(defmethod set-year :after (dummy (d date))
  (fixup-unix d))

(defmethod set-mon :after (dummy (d date))
  (fixup-unix d))

(defmethod set-dom :after (dummy (d date))
  (fixup-unix d))

(defmethod set-hour :after (dummy (d datetime))
  (fixup-unix d))

(defmethod set-min :after (dummy (d datetime))
  (fixup-unix d))

(defmethod set-sec :after (dummy (d datetime))
  (fixup-unix d))

(defmethod set-unix :after (unix (d date))
  (fixup-date unix d))

(defun make-date-by-unix (what &key unix)
  (let ((d (make-instance what :unix unix)))
    (fixup-date unix d)
    d))

(defun make-date (&key year (mon 1) (dom 1) unix)
  (cond
   (unix
    (make-date-by-unix 'date :unix unix))
   (year
    (let ((d (make-instance 'date :year year :mon mon :dom dom)))
      (fixup-unix d)
      d))))

(defun make-datetime (&key year (mon 1) (dom 1) (hour 0) (min 0) (sec 0) unix)
  (cond
   (unix
    (make-date-by-unix 'datetime :unix unix))
   (year
    (let ((d (make-instance 'datetime
			    :year year :mon mon :dom dom
			    :hour hour :min min :sec sec)))
      (fixup-unix d)
      d))))

(defmethod print-object ((d date) out)
  (with-slots (dom mon year dow unix) d
    (print-unreadable-object (d out :type t)
      (format out
	      "~4,'0d-~2,'0d-~2,'0d :dom ~a :unix ~d"
	      year mon dom dow unix))))

(defmethod print-object ((d datetime) out)
  (with-slots (dom mon year dow unix hour min sec) d
    (print-unreadable-object (d out :type t)
      (format out
	      "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d :dom ~a :unix ~d"
	      year mon dom hour min sec dow unix))))

;; arithmetic
(defmethod d= ((d1 date) (d2 date))
  (let ((step (max (get-step d1) (get-step d2))))
    (= (trunc/ (get-unix d1) step) (trunc/ (get-unix d2) step))))

(defmethod d> ((d1 date) (d2 date))
  (let ((step (max (get-step d1) (get-step d2))))
    (> (trunc/ (get-unix d1) step) (trunc/ (get-unix d2) step))))

(defmethod d>= ((d1 date) (d2 date))
  (or (d> d1 d2)
      (d= d1 d2)))

(defmethod d< ((d1 date) (d2 date))
  (not (d>= d1 d2)))

(defmethod d<= ((d1 date) (d2 date))
  (or (d< d1 d2)
      (d= d1 d2)))

(defmethod d+ ((d date) (inc number))
  (make-date-by-unix (type-of d) :unix (+ (get-unix d) (* (get-step d) inc))))

(defmethod d- ((d date) (inc integer))
  (d+ d (- inc)))

(defmethod d- ((d1 date) (d2 date))
  (let ((step (max (get-step d1) (get-step d2))))
    (trunc/ (- (get-unix d1) (get-unix d2)) step)))

(defmethod consecutivep ((d1 date) (d2 date))
  "Return non-NIL when there is no further datetime between D1 and D2."
  (<= (abs (d- d1 d2)) 1))


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
    :initarg :length)))

(defmethod make-interval (&key start end length)
  (make-instance 'interval
		 :start start
		 :end (or end (d+ start length))
		 :length (or length (d- end start))))

(defmethod set-start :after (s (i interval))
  "Update length slot."
  (setf (slot-value i 'length) (d- (get-end i) s)))

(defmethod set-end :after (e (i interval))
  "Update length slot."
  (setf (slot-value i 'length) (d- e (get-start i))))

(defmethod print-object ((i interval) out)
  (with-slots (start end length) i
    (print-unreadable-object (i out :type t)
      (format out
	      "~a - ~a :length ~d"
	      start end length))))

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
