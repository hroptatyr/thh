(defpackage :thhrule.util
  (:use #:cl)
  (:export
   #:make-date
   #:make-datetime
   #:make-interval))

(defpackage :cybertiggyr-time
  (:documentation "CyberTiggyr's Time-related library")
  (:use #:cl)
  (:export #:format-time
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
