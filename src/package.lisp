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
   #:make-date
   #:make-time
   #:make-datetime

   #:defrule
   #:defrule/once
   #:defrule/daily
   #:defrule/weekly
   #:defrule/monthly
   #:defrule/yearly
   #:deftrading-hours
   #:defholiday/weekly
   #:defholiday/monthly
   #:defholiday/yearly
   #:defholiday/once
   #:defruleset

   #:deftimezone
   #:defsession

   ;; for the rules eval'er
   #:next-event

   ;; predefined rules
   #:gregorian-good-friday
   #:gregorian-easter
   #:gregorian-easter/mon
   #:julian-good-friday
   #:julian-easter
   #:julian-easter/mon))

(provide :package)
(provide "package")

;; package.lisp ends here
