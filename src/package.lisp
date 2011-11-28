(defpackage :cybertiggyr-time
  (:documentation "CyberTiggyr's Time-related library")
  (:use #:cl)
  (:export
   #:make-fmt-recognizer
   #:format-time
   #:recognize-fmt))

(defpackage :copy-instance
  (:documentation "Michael Weber's copy-instance goodness")
  (:use
   #:cl
   #+sbcl #:sb-mop
   #+clisp #:clos)
  (:export
   #:copy-instance))

(defpackage :thhrule
  (:use
   #:cl
   #:cybertiggyr-time
   #:copy-instance)
  (:export
   #:make-date
   #:make-time
   #:make-datetime
   #:make-stamp

   #:defrule
   #:defrule/once
   #:defrule/daily
   #:defrule/weekly
   #:defrule/monthly
   #:defrule/yearly
   #:deftrading-hours
   #:defholiday
   #:defholiday/weekly
   #:defholiday/monthly
   #:defholiday/yearly
   #:defholiday/once
   #:defruleset

   #:deftimezone
   #:defsession

   ;; for the rules eval'er
   #:next-event
   #:metronome-of

   ;; predefined rules
   #:gregorian-good-friday
   #:gregorian-easter
   #:gregorian-easter/mon
   #:gregorian-whit-sunday
   #:gregorian-whit-monday
   #:julian-good-friday
   #:julian-easter
   #:julian-easter/mon))

(provide :package)
(provide "package")

;; package.lisp ends here
