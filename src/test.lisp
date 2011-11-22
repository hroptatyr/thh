(require "asdf")
(asdf:load-system :thhrule)

(use-package :thhrule)

(deftrading-hours th/eurex
  :from "2000-01-04" :till "2000-01-11"
  :open "08:00:00" :close "20:00:00")

(defholiday/yearly xmas :in dec :on 25)
(defholiday/yearly new-year :in jan :on 1)
(defholiday/weekly weekend/sat :on sat)
(defholiday/weekly weekend/sun :on sun)

(defholiday/once belated-xmas :on 2000-01-06)
(defholiday/once new-year/2012 :on 2012-01-02)

(defrule/daily settle
  :start "22:05:00" :end "22:10:00"
  :start-state +market-force+ :end-state +market-close+)

(defruleset eurex-plain
  :metronome "2000-01-01"
  th/eurex new-year/2012 new-year xmas weekend/sat settle
  weekend/sun belated-xmas)

(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)
(next-event eurex-plain)

#+sbcl
(sb-ext:save-lisp-and-die "test.o" :executable t)
#+clisp
(ext:saveinitmem "test.o" :executable t :norc t :quiet t)
#+cmu
(save-lisp "test.o" :executable t :load-init-file nil)
