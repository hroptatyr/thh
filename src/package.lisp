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

(defpackage :local-time
  (:use #:cl)
  (:export
   #:timezone
   #:timezonep
   #:make-timezone
   #:utc-stamp->offset))

(defpackage :thhrule
  (:use
   #:cl
   #:cybertiggyr-time
   #:copy-instance
   #:local-time)
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
   #:defruleset

   #:defmarket
   #:defstate
   #:defproduct

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
   #:gregorian-ascension
   #:gregorian-trinity-sunday
   #:gregorian-corpus-christi
   #:julian-good-friday
   #:julian-easter
   #:julian-easter/mon))

(provide :package)
(provide "package")

;; package.lisp ends here
