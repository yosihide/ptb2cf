(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :ptb2cf.node
  (:use :cl
	:cl-ppcre
	:ptb2cf.util
	:ptb2cf.label)
  (:export :make-node
	   :node-label
	   :node-empty?
	   :cat
	   :cat=
	   :function?
	   :annotations
	   :annotation?
	   :push-annotation!
	   :id
	   ;; internal node
	   :internal-node
	   :make-internal-node
	   :children
	   ;; terminal node
	   :terminal-node
	   :make-terminal-node
	   :word
	   ;; empty element
	   :empty-element?
	   :empty-element-type
	   :etype=
	   :empty-element-id
	   ;; filler
	   :filler?
	   :NP?
	   :SBJ?
	   ;; util
	   :remove-nodes-if
	   :node-ref
	   :terminals
	   :preorder
	   :preorder-right-first
	   ;; io
	   :string->tree
	   :tree->string
	   :read-tree
	   :read-trees
	   :write-tree
	   :write-trees
	   ))

(in-package :ptb2cf.node)

(defstruct node
  label
  empty?
  )

(defun cat (node)
  (label-category (node-label node)))

(defun cat= (node cat)
  (string= (cat node) cat))

(defun function? (f node)
  (and (find f (label-functions (node-label node)) :test #'string=) t))

(defun annotations (node)
  (label-annotations (node-label node)))

(defun annotation? (a node)
  (and (find a (label-annotations (node-label node)) :test #'string=) t))

(defun push-annotation! (a node)
  (pushnew a (label-annotations (node-label node))))

(defun id (node)
  (label--index (node-label node)))

(defun (setf id) (id node)
  (setf (label--index (node-label node)) id))

;; internal node
(defstruct (internal-node (:include node) (:constructor make-internal-node%))
  children
  )

(defun children (node)
  (internal-node-children node))

(defun (setf children) (children node)
  (setf (internal-node-children node) children))

(defun make-internal-node (&key label children)
  (make-internal-node% :label label
		       :children children
		       :empty? (every #'node-empty? children)))

;; terminal node
(defstruct (terminal-node (:include node))
  word
  )

(defun word (node)
  (terminal-node-word node))

;; empty element
(defun empty-element? (node)
  (cat= node "-NONE-"))

(defun empty-element-type (node)
  (when (not (empty-element? node))
    (error "~a is not empty element." node))
  (terminal-node-word node))

(defun etype= (node type)
  (string= (empty-element-type node) type))

(defun empty-element-id (node)
  (and (empty-element? node)
       (id node)))

;; filler
(defun filler? (node)
  (and (not (empty-element? node))
       (id node)
       t))

(defun NP? (node)
  (or (cat= node "NP")
      (annotation? "NOM" node)))

(defun SBJ? (node)
  (annotation? "SBJ" node))

;; util
(defun remove-nodes-if (test node)
  (declare (function test))
  (cond
    ((funcall test node)
     nil)

    ((eq (type-of node) 'terminal-node)
     node)

    (t
     (let ((children (delete nil
			     (mapcar #'(lambda (x) (remove-nodes-if test x))
				     (children node)))))
       (if (null children)
	   nil
	   (make-internal-node :label (node-label node)
			       :children children))))))

(defmacro node-ref (node &rest indexes)
  (if (null indexes)
      node
      `(when (eq (type-of ,node) 'internal-node)
	 (node-ref (nth ,(car indexes) (children ,node)) ,@(cdr indexes)))))

(defun preorder (node)
  (case (type-of node)
    (terminal-node
     (list node))
    (internal-node
     (cons node (mapcan #'preorder (children node))))))

(defun preorder-right-first (node)
  (case (type-of node)
    (terminal-node
     (list node))
    (internal-node
     (cons node (mapcan #'preorder-right-first (reverse (children node)))))))

(defun terminals (node)
  (case (type-of node)
    (terminal-node
     (list node))
    (internal-node
     (mapcan #'terminals (children node)))))

;;; reader
(defvar +eos+ (gensym))

(defun whitespace? (x)
  (member x '(#\Linefeed #\Newline #\Page #\Return #\Space #\Tab) :test #'char=))

(defun separator? (x)
  (or (char= x #\()
      (char= x #\))
      (whitespace? x)))

(defun make-char-buffer nil
  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))

(defun push-char-buffer (x buf)
  (vector-push-extend x buf))

(defun get-char-buffer (buf)
  (let ((str (make-array (length buf) :element-type 'character)))
    (dotimes (i (length buf))
      (setf (char str i) (char buf i)))
    (setf (fill-pointer buf) 0)
    str))

(defun read-string (stream)
  (let ((buf (make-char-buffer)))
    (loop
       :while (not (separator? (peek-char nil stream)))
       :do
       (vector-push-extend (read-char stream) buf))
    (get-char-buffer buf)))

(defun expected-char (char stream &key skip?)
  (when (not (char= (peek-char t stream) char))
    (error "error: expected ~a~%" char))
  (when skip?
    (read-char stream)))

(defun unexpected-char (char stream)
  (when (char= (peek-char t stream) char)
    (error "error: unexpected ~a~%" char)))

(defun read-tree-aux (stream)

  (expected-char #\( stream :skip? t)

  (let ((root-label nil)
	(children nil)
	(result nil))

    ;; read label
    (unexpected-char #\) stream)

    (cond
      ((char= (peek-char t stream) #\()
       (setf root-label (string->label "TOP")))
      (t
       (setf root-label (string->label (read-string stream)))))

    (unexpected-char #\) stream)

    (cond
      ;; internal node
      ((char= (peek-char t stream) #\()
       (loop
	  :while (char= (peek-char t stream) #\()
	  :do
	  (push (read-tree-aux stream) children))
       (setf result
	     (make-internal-node :label root-label
				 :children (nreverse children))))

      ;; empty terminal
      ((string= (label-category root-label) "-NONE-")
       (destructuring-bind (type &optional id) (split "-" (read-string stream))
	 (when id
	   (setf (label--index root-label) (read-from-string id)))
	 (setf result (make-terminal-node :label root-label :word type :empty? t))))

      ;; terminal
      (t
       (setf result (make-terminal-node :label root-label :word (read-string stream)))))

    (expected-char #\) stream :skip? t)

    result))

(defun read-tree (&optional (stream t) (eof-error-p t) eof-value)
  (if (and (not eof-error-p)
	   (eq (peek-char t stream nil +eos+) +eos+))
      (return-from read-tree eof-value)
      (read-tree-aux stream)))

(defun string->tree (string)
  (with-input-from-string (in string)
    (read-tree in)))

(defmacro do-stream-tree ((x stream &optional result-form) &body body)
  (let ((gstrm (gensym)))
    `(let ((,gstrm ,stream))
      (do ((,x))
	  ((eq (setf ,x (read-tree ,gstrm nil +eos+)) +eos+) ,result-form)
	,@body))))

(defmacro do-file-tree ((x path &optional result-form) &body body)
  (let ((gstrm (gensym)))
    `(with-open-file (,gstrm ,path)
      (do-stream-tree (,x ,gstrm ,result-form) ,@body))))

(defun read-trees (file)
  (let ((trees nil))
    (do-file-tree (x file)
      (push x trees))
    (nreverse trees)))

;; writer
(defun write-tree (tree out)
  (let ((label (node-label tree)))
    (format out "(")
    (format out "~a" (label-category label))
    (dolist (f (label-functions label))
      (format out "-~a" f))
    (dolist (a (label-annotations label))
      (format out "<~a>" a))
    (case (type-of tree)
      (internal-node
       (awhen (label--index label)
	 (format out "-~a" it))
       (awhen (label-=index label)
	 (format out "=~a" it))
       (dolist (x (children tree))
	 (format out " ")
	 (write-tree x out)))

      (terminal-node
       (format out " ~a" (terminal-node-word tree))
       (when (and (node-empty? tree) (label--index label))
	 (format out "-~a" (label--index label)))))
    (format out ")")))

(defun write-trees (trees file)
  (with-open-file (out file :direction :output)
    (dolist (tree trees)
      (write-tree tree out)
      (terpri out)))
  t)

(defun tree->string (tree)
  (let ((str (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (with-output-to-string (out str)
      (write-tree tree out))
    str))
