(require :asdf)
(require :thhrule)

(use-package :thhrule)

(defun xmas (year)
  "Covers xmas and boxing day."
  (make-interval
   :start (make-date :year year :mon 12 :dom 25)
   :length (* 2 86400)))

(loop for i from 2010 to 2019
  do
  (print (xmas i)))
