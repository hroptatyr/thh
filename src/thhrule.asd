;; -*- lisp -*-
(asdf:defsystem :thhrule
  :description "trading hours & holidays"
  :version "0.1"
  :author "Sebastian Freundt"
  :licence "3-clause BSD License"
  :components ((:file "package")
	       (:file "thhrule" :depends-on ("package" "util"))
	       (:file "util" :depends-on ("package"))))
