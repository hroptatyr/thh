(require "asdf")
(require :thhrule.util)
(require :cybertiggyr-time)
(in-package :thhrule)


;; cybertiggyr glue
(defparameter +date-recognisers+
  (list (make-fmt-recognizer "%Y-%m-%d")
	(make-fmt-recognizer "%Y-%b-%d")))

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


;; some macros
(defmacro defholiday (&rest ignore))
(defmacro deftrading-hours (&rest ignore))
(defmacro defholiday/yearly (&rest ignore))

(defmacro defholiday/once (name &key on)
  `(parse-time/glue ,on))

(provide :thhrule)

;; thhrule.l ends here
