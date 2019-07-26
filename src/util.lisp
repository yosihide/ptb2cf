(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :ptb2cf.util
  (:use :cl)
  (:export :aif
	   :awhen
	   :acond
	   :it
	   :append1
	   :hash-keys))

(in-package :ptb2cf.util)

;; anaphoric macro
(defmacro aif (test then &optional else)
  `(let ((it ,test))
    (declare (ignorable it))
    (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(aif ,test
    (progn ,@body)))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (sym (gensym)))
      `(let ((,sym ,(car cl1)))
	 (if ,sym
	     (let ((it ,sym))
	       (declare (ignorable it))
	       ,@(cdr cl1))
	   (acond ,@(cdr clauses)))))))

;; list
(declaim (inline append1))
(defun append1 (lst x) (append lst (list x)))

;; hash table
(declaim (inline hash-keys))
(defun hash-keys (hash)
  (let ((result nil))
    (maphash #'(lambda (key val) (declare (ignore val)) (push key result))
	     hash)
    result))
