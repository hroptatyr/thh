(require "util")
(in-package :thhrule)

(defun calc-gregorian-easter (year)
  (let* ((a (mod year 19))
	 (b (floor year 4))
	 (c (1+ (floor b 25)))
	 (d (floor (* c 3) 4))
	 (e (mod (+ (* a 19) (- (floor (+ (* c 8) 5) 25)) d 15) 30)))
    (setq e (+ e (floor (- 29578 a (* e 32)) 1024)))
    (- e (mod (+ (mod year 7) b (- d) e 2) 7))))

(defun calc-julian-easter (year)
  (let* ((a (mod year 19))
	 (b (mod year 4))
	 (c (mod year 7))
	 (M 15)
	 (N 6)
	 (d (mod (+ (* 19 a) M) 30))
	 (e (mod (+ (* 2 b) (* 4 c) (* 6 d) N) 7)))
    (+ 22 d e
       ;; this is to express things in gregorian
       13)))

(defmacro datify-easter (what &key (add 0) as)
  (flet ((sym-conc (&rest syms)
	   (intern (apply #'concatenate 'string (mapcar #'symbol-name syms)))))
    `(defun ,as (year)
       (let* ((e (+ (,(sym-conc 'calc- what '-easter) year) ,add))
	      (d (if (< e 32) 3 4)))
	 (make-date :year year :mon d :dom (if (= d 3) e (- e 31)))))))

;; convenience funs
(datify-easter gregorian :as gregorian-easter)
(datify-easter gregorian :add -2 :as gregorian-good-friday)
(datify-easter gregorian :add 1 :as gregorian-easter/mon)
(datify-easter julian :as julian-easter)
(datify-easter julian :add -2 :as julian-good-friday)
(datify-easter julian :add 1 :as julian-easter/mon)

(provide :thhrule.predef)

;; predef.lisp ends here