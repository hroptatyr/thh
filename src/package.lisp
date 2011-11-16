(defpackage :thhrule.util
  (:use #:cl)
  (:export
   #:make-stamp
   #:make-date
   #:make-datetime
   #:make-interval
   #:d>
   #:d>=
   #:d<
   #:d<=
   #:d=
   #:d+
   #:d-
   #:consecutivep
   #:connectedp
   #:containsp))

(defpackage :cybertiggyr-time
  (:documentation "CyberTiggyr's Time-related library")
  (:use #:cl)
  (:export
   #:make-fmt-recognizer
   #:format-time
   #:parse-time
   #:recognize-fmt))

(defpackage :thhrule
  (:use
   #:cl
   #:thhrule.util
   #:cybertiggyr-time)
  (:export
   #:defholiday
   #:deftrading-hours
   #:defholiday/yearly
   #:defholiday/once))

;; package.lisp ends here
