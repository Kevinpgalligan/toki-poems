(defpackage :toki-asd
  (:use :cl :asdf))

(in-package :toki-asd)

(defsystem toki
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:cl-ppcre)
  :pathname "src"
  :serial t
  :components ((:file "package")
	       (:file "markov")
	       (:file "poem")))
