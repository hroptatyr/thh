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

(defpackage :stamp
  (:use #:cl)
  (:export
   #:stamp
   #:interval
   #:make-date
   #:make-time
   #:make-datetime
   #:make-stamp

   #:get-unix
   #:get-year
   #:get-mon
   #:get-dom
   #:get-hour
   #:get-min
   #:get-sec

   #:get-dow/num
   #:get-dow/sym
   #:get-mon/num
   #:get-mon/sym

   #:make-interval

   #:d+
   #:d-
   #:d>
   #:d<
   #:d=
   #:d<=
   #:d>=

   #:dt=
   #:dt<
   #:dt>
   #:dt<=
   #:dt>=

   #:i=))

(defpackage :thhrule
  (:use
   #:cl
   #:cybertiggyr-time
   #:copy-instance
   #:local-time
   #:stamp)
  (:export
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
