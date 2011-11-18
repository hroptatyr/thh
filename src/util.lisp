(in-package :thhrule)

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
  (multiple-value-bind (ts tm th dd dm dy wd dst-p tz)
      (decode-universal-time u)
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
  (multiple-value-bind (ts tm th dd dm dy wd dst-p tz)
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
  (fixup-stamp d))

(defmethod set-mon :after (dummy (d date))
  (fixup-stamp d))

(defmethod set-dom :after (dummy (d date))
  (fixup-stamp d))

(defmethod set-hour :after (dummy (tm tod))
  (fixup-stamp tm))

(defmethod set-min :after (dummy (tm tod))
  (fixup-stamp tm))

(defmethod set-sec :after (dummy (tm tod))
  (fixup-stamp tm))

(defmethod set-unix :after (unix (s stamp))
  (fixup-human unix s))

(defmethod get-year ((s stamp))
  ;; temporarily promote
  (multiple-value-bind (ts tm th dd dm dy wd dst-p tz)
      (decode-universal-time (get-unix s))
    dy))

;; (defmethod get-mon :after (dummy (d date))
;;   1)
;; 
;; (defmethod get-dom :after (dummy (d date))
;;   1)
;; 
;; (defmethod get-hour :after (dummy (tm tod))
;;   0)
;; 
;; (defmethod get-min :after (dummy (tm tod))
;;   0)

(defmethod get-sec ((s (eql 'stamp)))
  (mod s 60))

(defun make-stamp (&key what unix)
  (let ((u (make-instance (or what 'stamp) :unix unix)))
    (fixup-human unix u)
    u))

(defun get-mon/num (mon)
  (cond
   ((stringp mon)
    (position (intern mon 'thhrule) +month-of-year+))
   ((symbolp mon)
    (position (intern (symbol-name mon) 'thhrule) +month-of-year+))
   ((numberp mon)
    mon)))

(defun get-dow/sym (dow)
  (if (numberp dow)
      (nth dow +day-of-week+)
    (intern (cond
	     ((symbolp dow)
	      (symbol-name dow))
	     ((stringp dow)
	      dow))
	    'thhrule)))

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
	      "~4,'0d-~2,'0d-~2,'0d :dom ~a :unix ~d"
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
	      "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d :dom ~a :unix ~d"
	      year mon dom hour min sec dow unix))))

;; arithmetic
(defmethod d= ((s1 stamp) (s2 stamp))
  (let ((step (max (get-step s1) (get-step s2))))
    (= (floor (get-unix s1) step) (floor (get-unix s2) step))))

(defmethod d> ((s1 stamp) (s2 stamp))
  (let ((step (max (get-step s1) (get-step s2))))
    (> (floor (get-unix s1) step) (floor (get-unix s2) step))))

(defmethod d>= ((s1 stamp) (s2 stamp))
  (or (d> s1 s2)
      (d= s1 s2)))

(defmethod d< ((s1 stamp) (s2 stamp))
  (not (d>= s1 s2)))

(defmethod d<= ((s1 stamp) (s2 stamp))
  (or (d< s1 s2)
      (d= s1 s2)))

(defmethod d+ ((s stamp) (inc number))
  (make-stamp :what (type-of s) :unix (+ (get-unix s) (* (get-step s) inc))))

(defmethod d- ((s stamp) (inc integer))
  (d+ s (- inc)))

(defmethod d- ((s1 stamp) (s2 stamp))
  (let ((step (max (get-step s1) (get-step s2))))
    (floor (- (get-unix s1) (get-unix s2)) step)))

(defmethod consecutivep ((s1 stamp) (s2 stamp))
  "Return non-NIL when there is no further points between S1 and S2."
  (<= (abs (d- s1 s2)) 1))


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

(defun make-interval (&key start end length)
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

(defmethod containsp ((i interval) (s stamp))
  "Return non-NIL when I contains S."
  (and (d>= s (get-start i))
       (d<= s (get-end i))))

(defmethod containsp ((i1 interval) (i2 interval))
  "Return non-NIL when I1 contains I2."
  (and (containsp i1 (get-start i2))
       (containsp i1 (get-end i2))))

(defmethod connectedp ((i interval) (s stamp))
  "Return non-NIL when there is no datetime point between I and S."
  (or (containsp i s)
      (consecutivep (get-start i) s)
      (consecutivep (get-end i) s)))

(defmethod connectedp ((i1 interval) (i2 interval))
  "Return non-NIL when there is no datetime point between I1 and I2."
  (or (connectedp i1 (get-start i2))
      (connectedp i1 (get-end i2))))

(provide :thhrule.util)
