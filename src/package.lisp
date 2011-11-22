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
   #:defrule/once
   #:defrule/daily
   #:defrule/weekly
   #:defrule/yearly
   #:deftrading-hours
   #:defholiday/weekly
   #:defholiday/yearly
   #:defholiday/once
   #:defruleset

   ;; for the rules eval'er
   #:next-event

   ;; predefined rules
   #:gregorian-easter/fri
   #:gregorian-easter/sun
   #:gregorian-easter/mon))

;; package.lisp ends here
