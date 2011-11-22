(defpackage :cybertiggyr-time
  (:documentation "CyberTiggyr's Time-related library")
  (:use #:cl)
  (:export
   #:make-fmt-recognizer
   #:format-time
   #:recognize-fmt))

(defpackage :thhrule
  (:use
   #:cl
   #:cybertiggyr-time)
  (:export
   #:defrule/once
   #:defrule/daily
   #:defrule/weekly
   #:defrule/yearly
   #:deftrading-hours
   #:defholiday/weekly
   #:defholiday/yearly
   #:defholiday/once
   #:defruleset

   #:next-event))

;; package.lisp ends here
