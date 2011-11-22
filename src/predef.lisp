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
    (- e (mod (+ (mod year 7) b (- d) e 2) 7))))

(defun gregorian-easter/sun (year)
  (let* ((e (gregorian-easter year))
	 (d (if (< e 32) 3 4)))
    (make-date :year year :mon d :dom (if (= d 3) e (- e 31)))))

(defun gregorian-easter/fri (year)
  (let* ((e (- (gregorian-easter year) 2))
	 (d (if (< e 32) 3 4)))
    (make-date :year year :mon d :dom (if (= d 3) e (- e 31)))))

(defun gregorian-easter/mon (year)
  (let* ((e (1+ (gregorian-easter year)))
	 (d (if (< e 32) 3 4)))
    (make-date :year year :mon d :dom (if (= d 3) e (- e 31)))))

(provide :thhrule.predef)

;; predef.lisp ends here