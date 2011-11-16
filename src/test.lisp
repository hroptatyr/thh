(require "asdf")
(asdf:load-system :thhrule)

(use-package :thhrule)

(deftrading-hours th/eurex
  :from "08:00:00 CET" :till "20:00:00 CET")

(defholiday xmas :every 'year :in 'dec :on 25 :in 'dec :on 26)
(defholiday/yearly new-year :in 'jan :on 1)

(defholiday/once new-year/2012 :on "2012-01-02")
