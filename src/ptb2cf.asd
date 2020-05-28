(defpackage :ptb2cf.system
  (:use :cl
	:asdf))

(in-package :ptb2cf.system)

(defsystem "ptb2cf"
  :depends-on (:cl-ppcre)
  :description "A tool for approximating PTB graph-structured representations by trees"
  :version "0.2"
  :author "Yoshihide Kato"
  :components ((:file "util")
	       (:file "label")
	       (:file "node")
	       (:file "ptb2cf")
	       (:file "misc")
	       ))
