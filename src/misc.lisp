(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :ptb2cf.misc
  (:use :cl
	:ptb2cf.node)
  (:export :extract-sentence))

(in-package :ptb2cf.misc)

(defun extract-sentence (&key input output)
  (with-open-file (out output :direction :output)
    (dolist (tree (read-trees input))
      (format out "~{~a~^ ~}~%"
	      (mapcar #'word (terminals tree))))))
