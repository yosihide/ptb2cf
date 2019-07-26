(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :ptb2cf.label
  (:use :cl
	:cl-ppcre
	:ptb2cf.util)
  (:export :make-label
	   :label-category
	   :label-functions
	   :label--index
	   :label-=index
	   :label-annotations
	   :string->label))

(in-package :ptb2cf.label)

(defstruct label
  category
  functions
  -index
  =index
  annotations
  )

(declaim (inline remove-annotations))
(defun remove-annotations (string)
  (regex-replace-all "<[^<>]+>" string ""))

(declaim (inline extract-category))
(defun extract-category (string)
  (regex-replace-all "(-[A-Z0-9]+|=[0-9]+|<[^<>]+>)" string ""))

(declaim (inline extract-functions))
(defun extract-functions (string)
  (mapcar #'(lambda (x) (subseq x 1))
	  (all-matches-as-strings "-[A-Z]+" (remove-annotations string))))

(declaim (inline extract-index))
(defun extract-index (string)
  (awhen (nth-value 1 (scan-to-strings "(-)([0-9]+)" (remove-annotations string)))
    (read-from-string (svref it 1))))

(declaim (inline extract-=index))
(defun extract-=index (string)
  (awhen (nth-value 1 (scan-to-strings "(=)([0-9]+)" (remove-annotations string)))
    (read-from-string (svref it 1))))

(declaim (inline extract-annotations))
(defun extract-annotations (string)
  (mapcar #'(lambda (x)
	      (let ((len (length x)))
		(subseq x 1 (- len 1))))
	  (all-matches-as-strings "<[^<>]+>" string)))

(defun string->label (string)
  (cond
    ((char= (char string 0) #\-)
     (make-label :category (scan-to-strings "^-[A-Z]+-" string)
		 :annotations (extract-annotations string)))
    (t
     (make-label :category (extract-category string)
		 :functions (extract-functions string)
		 :-index (extract-index string)
		 :=index (extract-=index string)
		 :annotations (extract-annotations string)))))
