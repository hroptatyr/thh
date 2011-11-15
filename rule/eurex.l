(load "../src/thhrule.l")

(defun xmas (year)
  (values (make-date :year year :mon 12 :dom 25) 86400 'market-close))

(loop for i from 2010 to 2019
  do
  (print (xmas i)))
