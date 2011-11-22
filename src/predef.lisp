(require "asdf")
(require :thhrule.util)
(in-package :thhrule)

(defun gregorian-easter (year)
  (let* ((a (mod year 19))
	 (b (floor year 4))
	 (c (1+ (floor b 25)))
	 (d (floor (* c 3) 4))
	 (e (mod (+ (* a 19) (- (floor (+ (* c 8) 5) 25)) d 15) 30))
	 dom mon)
    (setq e (+ e (floor (- 29578 a (* e 32)) 1024)))
    (setq e (- e (mod (+ (mod year 7) b (- d) e 2) 7) 2))
    (setq d (floor e 32))
    (make-date :year year :mon (+ d 3) :dom (- e (* d 31)))))

(provide :thhrule.predef)

;; predef.lisp ends here