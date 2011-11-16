(require "asdf")
(require :thhrule.util)

(in-package :thhrule)

(defmacro defholiday (&rest ignore))
(defmacro deftrading-hours (&rest ignore))
(defmacro defholiday/yearly (&rest ignore))

(defmacro defholiday/once (name &key on)
  `(parse-time ,on))

(provide :thhrule)

;; thhrule.l ends here
