(require "asdf")
(require :thhrule.util)
(require :cybertiggyr-time)
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


;; states
(defconstant +market-last+ t)
(defconstant +market-open+ t)
(defconstant +market-close+ t)
(defconstant +market-force+ t)

(defparameter +states+
  '(+market-last+ +market-open+ +market-close+ +market-force+))

(deftype state () `(member ,@+states+))

(defmethod state< (s1 s2)
  ;; s1 < s2 iff s1 further left in +states+
  (member s2 (cdr (member s1 +states+))))

(defmethod state<= (s1 s2)
  (or (eql s1 s2)
      (state< s1 s2)))

(defmethod state> (s1 s2)
  (not (state<= s1 s2)))

(defmethod state>= (s1 s2)
  (or (eql s1 s2)
      (state> s1 s2)))

(defun max-state (a b)
  (if b
      (if (state> a b)
	  a
	b)
    a))


;; rule class
(defparameter +dawn-of-time+ (make-stamp :unix 0))
(defparameter +dusk-of-time+ (make-stamp :unix 4294967295))

(defclass rule ()
  (
   ;; validity forms first
   (from
    :initarg :from
    :initform +dawn-of-time+)
   (till
    :initarg :till
    :initform +dusk-of-time+)
   ;; stream closure, takes stamp and returns the next occurrence
   (next-lambda
    :initarg :next-lambda
    :type function)
   (next
    :initarg :next
    :type stamp)
   (state-start
    :initarg :state-start
    :reader get-start-state
    :type state)
   (state-end
    :initarg :state-end
    :reader get-end-state
    :type state)
   (name
    :initarg :name
    :reader get-name)))

(defmacro make-rule (&rest stuff)
  `(make-instance 'rule ,@stuff))

(defmethod print-object ((r rule) out)
  (with-slots (name) r
    (print-unreadable-object (r out :type t)
      (format out "~a" name))))

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

(defmethod midnight ((su integer) &optional next)
  (let ((sm (mod su 86400)))
    (+ (- su sm) (or (and next (> sm next) 86400) 0))))

(defmethod midnight ((s stamp) &optional next)
  (midnight (get-unix s) next))

(defmethod get-start ((r rule))
  (with-slots (next) r
    (get-start next)))

(defmethod get-start ((r (eql nil)))
  nil)

(defmethod get-end ((r rule))
  (with-slots (next) r
    (get-end next)))

(defmethod get-end ((r (eql nil)))
  nil)

;; aux methods, eql specialisers
(defmethod dt< ((i1 interval) (i2 (eql nil)))
  t)

(defmethod dt< ((i1 (eql nil)) i2)
  nil)

(defmethod dt= ((s1 interval) (s2 (eql nil)))
  nil)

(defmethod dt= ((s1 (eql nil)) s2)
  nil)

;; some macros
(defmacro defholiday (&rest ignore))

(defmacro deftrading-hours (name &key from till open close)
  (let* ((open/stamp (parse-time open))
	 (close/stamp (parse-time close))
	 (ou (mod (get-unix open/stamp) 86400))
	 (cu (mod (get-unix close/stamp) 86400))
	 (from/stamp (or (parse-dtall from) +dawn-of-time+))
	 (till/stamp (or (parse-dtall till) +dusk-of-time+)))
    `(defvar ,name
       (make-rule
	:from ,from/stamp
	:till ,till/stamp
	:state-start '+market-open+
	:state-end '+market-close+
	:name ',name
	:next-lambda
	(lambda (stamp)
	  (let* ((fu ,(get-unix from/stamp))
		 (su (get-unix stamp))
		 (stamp/midnight (midnight (max su fu) ,ou))
		 (probe/from (make-datetime :unix (+ stamp/midnight ,ou)))
		 (probe/till (make-datetime :unix (+ stamp/midnight ,cu))))
	    (if (dt<= probe/from ,till/stamp)
		(make-interval :start probe/from :end probe/till))))))))

(defmacro defholiday/yearly (name &key from till in on)
  "Define yearly recurring holidays."
  (let ((from/stamp (or (parse-dtall from) +dawn-of-time+))
	(till/stamp (or (parse-dtall till) +dusk-of-time+))
	(in/num (get-mon/num in)))
    `(defvar ,name
       (make-rule
	:from ,from/stamp
	:till ,till/stamp
	:state-start '+market-close+
	:state-end '+market-last+
	:name ',name
	:next-lambda
	(lambda (stamp)
	  (do* ((ys (get-year stamp))
		(yf ,(get-year from/stamp))
		(yt ,(get-year till/stamp))
		(y (max ys yf) (1+ y))
		(probe))
	      ((d>= (setq probe (make-date :year y :mon ,in/num :dom ,on)) stamp)
	       (if (d<= probe ,till/stamp)
		   (make-interval :start probe :length 1)))))))))

(defmacro defholiday/weekly (name &key from till on)
  "Define weekly recurring holidays, weekends, etc.."
  (let ((from/stamp (or (parse-dtall from) +dawn-of-time+))
	(till/stamp (or (parse-dtall till) +dusk-of-time+))
	(on/sym (get-dow/sym on)))
    `(defvar ,name
       (make-rule
	:from ,from/stamp
	:till ,till/stamp
	:state-start '+market-close+
	:state-end '+market-last+
	:name ',name
	:next-lambda
	(lambda (stamp)
	  (do* ((sf (get-unix ,from/stamp))
		(ss (get-unix stamp))
		(s (midnight (max sf ss) 0) (+ 86400 s))
		(probe))
	      ((and (eql (get-dow (setq probe (make-date :unix s))) ',on/sym)
		    (d>= probe stamp))
	       (if (d<= probe ,till/stamp)
		   (make-interval :start probe :length 1)))))))))

(defmacro defholiday/once (name &key from till on for)
  "Define a one-off event."
  (let ((on/stamp (parse-date on))
	(from/stamp (or (parse-dtall from) +dawn-of-time+))
	(till/stamp (or (parse-dtall till) +dusk-of-time+)))
    `(defvar ,name
       (make-rule
	:from ,from/stamp
	:till ,till/stamp
	:state-start '+market-close+
	:state-end '+market-last+
	:name ',name
	:next
	,(if (and (d>= on/stamp from/stamp) (d<= on/stamp till/stamp))
	     (make-interval :start on/stamp :length 1)
	   ;; otherwise the user is obviously confused
	   nil)))))

(defmethod next-event/rule ((metronome stamp) (r rule))
  (with-slots (till next next-lambda) r
    (setf next
	  (cond
	   ((dt> metronome till)
	    nil)
	   ((and (slot-boundp r 'next)
		 (or (null next) (dt>= (get-start next) metronome)))
	    next)
	   ((and (slot-boundp r 'next-lambda)
		 (functionp next-lambda))
	    (funcall next-lambda metronome))
	   (t
	    nil)))))


;; rulesets
(defclass ruleset ()
  ((metronome
    :initarg :metronome
    :type stamp)
   (state
    :initform +market-last+
    :type state)
   (rule
    :initform nil
    :type rule)
   (rules
    :initarg :rules
    :type sequence)))

(defmacro make-ruleset (&rest stuff)
  `(make-instance 'ruleset ,@stuff))

(defun ruleset-rules+keys (list)
  (loop
    with rules = nil
    and keys = nil
    while list
    do (if (keywordp (car list))
	   (setq keys (cons (car list) (cons (cadr list) keys))
		 list (cddr list))
	 (setq rules (cons (car list) rules)
	       list (cdr list)))
    finally (return (values rules keys))))

(defmacro defruleset (name &rest vars+keys)
  "&key (metronome +dawn-of-time+))"
  (multiple-value-bind (rules keys) (ruleset-rules+keys vars+keys)
    (destructuring-bind (&key (metronome +dawn-of-time+)) keys
      `(defvar ,name
	 (make-ruleset
	  :metronome ,(parse-dtall metronome)
	  :rules (list ,@rules))))))

(defmethod metro-next ((rs ruleset) (r rule))
  "Find next metronome point, given that R is the chosen rule."
  (with-slots (rules) rs
    (with-slots ((rnext next) (rstate state-start)) r
      ;; find first rule whose start > r's start and whose state > r's state
      (let ((cand
	     (find-if #'(lambda (a)
			  (with-slots ((anext next) (astate state-start)) a
			    (let ((astart (get-start anext))
				  (rstart (get-start rnext))
				  (rend (get-end rnext)))
			      (and astart
				   (dt> astart rstart)
				   (dt<= astart rend)
				   (state> astate rstate)))))
		      rules)))
	(if cand
	    (values (get-start cand) (get-start-state cand) cand)
	  (values (get-end rnext) (get-end-state r) r))))))

(defmethod metro-sort ((metronome stamp) (r1 rule) (r2 rule))
  (let ((ne1 (next-event/rule metronome r1))
	(ne2 (next-event/rule metronome r2)))
    (cond
     ((dt< ne1 ne2)
      t)
     ((dt= ne1 ne2)
      (state> (get-start-state r1) (get-start-state r2))))))

(defmethod metro-round ((rs ruleset))
  (with-slots (metronome state rules) rs
    ;; stable-sort needs #'setf'ing under sbcl
    (setf rules (stable-sort rules #'(lambda (a b) (metro-sort metronome a b))))
    (let* ((chosen (car rules))
	   (chostart (get-start chosen)))
      (if chostart
	  (values chostart (get-start-state chosen) chosen)
	(values nil '+market-last+ nil)))))

(defmethod next-event ((rs ruleset))
  (with-slots (metronome state rule rules) rs
    (multiple-value-bind (stamp newst newru) (metro-round rs)
      (loop
	when (null metronome)
	return nil
	do (setf (values metronome state rule)
		 (cond
		  ((or (not (eql newst state))
		       (dt> stamp metronome))
		   (metro-round rs))
		  ((dt= stamp metronome)
		   (metro-next rs rule))
		  (t
		   (error "state inconsistent"))))
	unless (eql state '+market-last+)
	return (values metronome state rule)))))

(provide :thhrule)

;; thhrule.l ends here
