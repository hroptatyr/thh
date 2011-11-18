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
   #:parse-date
   #:parse-time
   #:parse-datetime
   #:parse-dtall
   #:defholiday
   #:deftrading-hours
   #:defholiday/yearly
   #:defholiday/weekly
   #:defholiday/once

   #:next-event))

;; package.lisp ends here
