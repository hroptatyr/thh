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

(defun parse-time/glue (str)
  (let (u)
    (cond
     ((setq u (parse-time str +datetime-recognisers+))
      (make-datetime :unix u))
     ((setq u (parse-time str +date-recognisers+))
      (make-date :unix u))
     ((setq u (parse-time str +time-recognisers+))
      (make-time :unix u))
     ((setq u (parse-time str))
      (make-stamp :unix u)))))


;; rule class
(defclass rule ()
  (
   ;; validity forms first
   (:from
    :type stamp)
   (:till
    :type stamp)))

(defmacro make-rule (&rest stuff)
  `(make-instance 'rule ,@stuff))

(defun parse-dtval (thing)
  (cond
   ((stringp thing)
    (parse-time/glue thing))
   ((member (type-of thing) '(stamp date tod datetime))
    thing)))

;; some macros
(defmacro defholiday (&rest ignore))
(defmacro deftrading-hours (&rest ignore))
(defmacro defholiday/yearly (&rest ignore))

(defmacro defholiday/once (name &key from till on)
  (let ((on/stamp (parse-dtval on))
	(from/stamp (parse-dtval from))
	(till/stamp (parse-dtval till)))
    ;; produce a closure for now
    `(defun ,name (stamp)
       (and
	(or (null ,from/stamp) (d>= stamp ,from/stamp))
	(or (null ,till/stamp) (d<= stamp ,till/stamp))
	(d>= ,on/stamp stamp)
	,on/stamp))))

(provide :thhrule)

;; thhrule.l ends here
