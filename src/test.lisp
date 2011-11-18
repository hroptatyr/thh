(require "asdf")
(asdf:load-system :thhrule)

(use-package :thhrule)

(deftrading-hours th/eurex
  :from "08:00:00 CET" :till "20:00:00 CET")

;;(defholiday xmas :every 'year :in 'dec :on 25 :in 'dec :on 26)
(defholiday/yearly new-year :in jan :on 1 :for 1day)

(defholiday/once new-year/2012 :on 2012-01-02)

(next-event new-year/2012)
(next-event new-year)

(in-package :thhrule)
;; (make-datetime :unix 0)

#+sbcl
(sb-ext:save-lisp-and-die "test.o" :executable t)
#+clisp
(ext:saveinitmem "test.o" :executable t :norc t :quiet t)
#+cmu
(save-lisp "test.o" :executable t :load-init-file nil)
