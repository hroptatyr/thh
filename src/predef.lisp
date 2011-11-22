(require "asdf")
(require :thhrule.util)
(in-package :thhrule)

(defun gregorian-easter (year)
  (let* ((a (mod year 19))
	 (b (floor year 4))
	 (c (1+ (floor b 25)))
	 (d (floor (* c 3) 4))
	 (e (mod (+ (* a 19) (- (floor (+ (* c 8) 5) 25)) d 15) 30)))
    (setq e (+ e (floor (- 29578 a (* e 32)) 1024)))
    (- e (mod (+ (mod year 7) b (- d) e 2) 7))))

(defun julian-easter (year)
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

(defmacro datify-easter (what suf add)
  (flet ((sym-conc (&rest syms)
	   (intern (apply #'concatenate 'string (mapcar #'symbol-name syms)))))
    `(defun ,(sym-conc what '-easter/ suf) (year)
       (let* ((e (+ (,(sym-conc what '-easter) year) ,add))
	      (d (if (< e 32) 3 4)))
	 (make-date :year year :mon d :dom (if (= d 3) e (- e 31)))))))

;; convenience funs
(datify-easter gregorian sun 0)
(datify-easter gregorian fri -2)
(datify-easter gregorian mon 1)
(datify-easter julian sun 0)
(datify-easter julian fri -2)
(datify-easter julian mon 1)

(provide :thhrule.predef)

;; predef.lisp ends here