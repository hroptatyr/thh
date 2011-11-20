(require "asdf")
(asdf:load-system :thhrule)

(use-package :thhrule)

(deftrading-hours th/eurex
  :from "08:00:00" :till "20:00:00")

(defholiday/yearly xmas :in dec :on 25)
(defholiday/yearly new-year :in jan :on 1)
(defholiday/weekly weekend/sat :on sat)
(defholiday/weekly weekend/sun :on sun)

(defholiday/once new-year/2012 :on 2012-01-02)

(defvar eurex-plain
  (list th/eurex new-year/2012 new-year xmas weekend/sat weekend/sun))

(sort-ruleset eurex-plain)
(sort-ruleset eurex-plain)
(sort-ruleset eurex-plain)

(in-package :thhrule)

#+sbcl
(sb-ext:save-lisp-and-die "test.o" :executable t)
#+clisp
(ext:saveinitmem "test.o" :executable t :norc t :quiet t)
#+cmu
(save-lisp "test.o" :executable t :load-init-file nil)
