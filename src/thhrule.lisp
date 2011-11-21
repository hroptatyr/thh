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


;; rule class
(defparameter +dawn-of-time+ (make-stamp :unix 0))
(defparameter +dusk-of-time+ (make-stamp :unix 4294967295))

(defconstant +market-open+ t)
(defconstant +market-close+ t)
(defconstant +market-force+ t)

(deftype state ()
  `(member +market-last+ +market-open+ +market-close+ +market-force+))

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
    :type state)
   (state-end
    :initarg :state-end
    :type state)))

(defmacro make-rule (&rest stuff)
  `(make-instance 'rule ,@stuff))

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
	:next-lambda
	(lambda (stamp)
	  (let* ((u (get-unix stamp))
		 (su (mod u 86400))
		 (stamp/midnight (- u su))
		 (probe (if (>= su ,ou)
			    86400
			  0))
		 (probe/from (make-datetime :unix (+ stamp/midnight probe ,ou)))
		 (probe/till (make-datetime :unix (+ stamp/midnight probe ,cu))))
	    (make-interval :start probe/from :end probe/till)))))))

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
	:next-lambda
	(lambda (stamp)
	  (do* ((ys (get-year stamp))
		(yf ,(get-year from/stamp))
		(yt ,(get-year till/stamp))
		(y (max ys yf) (1+ y))
		(probe))
	      ((d> (setq probe (make-date :year y :mon ,in/num :dom ,on)) stamp)
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
	:next-lambda
	(lambda (stamp)
	  (do* ((sf (get-unix ,from/stamp))
		(ss (get-unix stamp))
		(s (max sf ss) (+ 86400 s))
		(probe))
	      ((and (eql (get-dow (setq probe (make-date :unix s))) ',on/sym)
		    (d> probe stamp))
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
	:next
	,(if (and (d>= on/stamp from/stamp) (d<= on/stamp till/stamp))
	     (make-interval :start on/stamp :length 1)
	   ;; otherwise the user is obviously confused
	   nil)))))


;; rulesets
(defclass ruleset ()
  ((metronome
    :initarg :metronome
    :type stamp)
   (rules
    :initarg :rules
    :type sequence)))

(defmacro make-ruleset (&rest stuff)
  `(make-instance 'ruleset ,@stuff))

(defmacro defruleset (name &rest vars
			   &key (metronome +dawn-of-time+) &allow-other-keys)
  (let ((rules (remove (cadr (member ':metronome vars))
		       (remove ':metronome vars))))
    `(defvar ,name
       (make-ruleset
	:metronome ,(parse-dtall metronome)
	:rules (list ,@rules)))))

(defmethod next-event/rule ((rs ruleset) (r rule))
  (with-slots (metronome) rs
    (with-slots (next next-lambda) r
      (cond
       ((slot-boundp r 'next)
	next)
       ((and (slot-boundp r 'next-lambda) (functionp next-lambda))
	(setf next (funcall next-lambda metronome)))
       (t
	(setf next nil))))))

(defmethod next-event ((rs ruleset))
  (with-slots (metronome rules) rs
    (stable-sort rules
		 #'(lambda (a b)
		     (d< (next-event/rule rs a) (next-event/rule rs b))))
    (let ((r (car rules)))
      (with-slots (next) r
	(let ((s (get-start next)))
	  (slot-makunbound r 'next)
	  (setf metronome (make-datetime :unix (get-unix s))))))))

(provide :thhrule)

;; thhrule.l ends here
