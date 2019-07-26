(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :ptb2cf
  (:use :cl
	:cl-ppcre
	:ptb2cf.util
	:ptb2cf.label
	:ptb2cf.node)
  (:export :ptb->cf
	   :cf->ptb))

(in-package :ptb2cf)

;;; ptb->cf
;; annotate-nonlocal-dependency
(defun filler-tag (cat type)
  (format nil "~a~a" cat type))

(defun annotate-nonlocal-dependency (tree)
  (let ((nodes (preorder tree))
	(parent-tab (make-hash-table :test 'eq))
	(index=>empty-element (make-hash-table))
	(index=>filler (make-hash-table)))

    ;; parent table
    (labels
	((traverse (node parent)
	   (setf (gethash node parent-tab) parent)
	   (when (eq (type-of node) 'internal-node)
	     (dolist (c (children node))
	       (traverse c node)))))
      (traverse tree nil))

    ;; empty element
    (dolist (n nodes)
      (awhen (empty-element-id n)
	;; If its filler is an ancestor, the filler is not annotated.
	(when (loop
		 :for a :=(gethash n parent-tab) :then (gethash a parent-tab)
		 :while a
		 :always (not (equal (id a) it)))
	  (setf (gethash (id n) index=>empty-element) n))))

    ;; filler
    (dolist (n nodes)
      (when (filler? n)
	(setf (gethash (id n) index=>filler) n)))

    ;; from left to right (preorder)
    (let ((empty-element-tab (make-hash-table))
	  (filler-tab (make-hash-table)))
      (dolist (n nodes)

	(when (function? "SBJ" n)
	  (push-annotation! "SBJ" n))
	(when (function? "NOM" n)
	  (push-annotation! "NOM" n))

	(acond
	 ;; filler
	 ((and (filler? n)
	       (gethash (id n) index=>empty-element))

	  (setf (gethash (id n) filler-tab) t)

	  (when (not (etype= it "*"))
	    (cond
	      ((gethash (id n) empty-element-tab)
	       (push-annotation! "DIRR" n))
	      (t
	       (push-annotation! "DIRL" n)))
	    (push-annotation! (filler-tag (cat (gethash it parent-tab)) (empty-element-type it)) n)))

	 ;; empty element
	 ((empty-element-id n)

	  (setf (gethash it empty-element-tab) t)

	  ;; object control
	  (when (and (etype= n "*")
		     (not (SBJ? (gethash it index=>filler))))
	    (push-annotation! "OBJCTRL" n))

	  ;; direction
	  (cond
	    ((null (gethash (id n) index=>empty-element))
	     (push-annotation! "DIRA" n))
	    ((gethash (id n) filler-tab)
	     (push-annotation! "DIRL" n))
	    (t
	     (push-annotation! "DIRR" n)))))))

    (dolist (n nodes)
      (let ((label (node-label n)))
	(setf (node-label n)
	      (make-label :category (label-category label)
			  :annotations (label-annotations label))))))
  tree)

;; remove-empty-element
(defun remove-empty-element (tree)
  (remove-nodes-if #'(lambda (x) (and (empty-element? x)
				      (or (etype= x "*EXP*")
					  (etype= x "*PPA*"))))
		   tree))

;; remove-recursive-unary-rule
(defun remove-recursive-unary-rule (tree)
  (cond
    ((eq (type-of tree) 'terminal-node)
     tree)
    ((and (= (length (children tree)) 1)
	  (string= (cat tree)
		   (cat (node-ref tree 0))))
     (remove-recursive-unary-rule (make-internal-node :label (node-label tree)
						      :children (children (node-ref tree 0)))))
    (t
     (make-internal-node :label (node-label tree)
			 :children (mapcar #'remove-recursive-unary-rule (children tree))))))

;; remove-annotation (for CF-tree)
(defun remove-annotation (tree)
  (cond
    ((eq (type-of tree) 'terminal-node)
     (make-terminal-node :label (make-label :category (cat tree))
			 :word (word tree)))
    (t
     (make-internal-node :label (make-label :category (cat tree))
			 :children (mapcar #'remove-annotation (children tree))))))

;; dictionary
(defstruct (dictionary (:constructor make-dict))
  empty-element-string=>index
  index=>empty-element-string
  )

(defun get-index (e dict)
  (when (node-empty? e)
    (gethash (tree->string e) (dictionary-empty-element-string=>index dict))))

(defun get-empty-element (i dict)
  (string->tree (svref (dictionary-index=>empty-element-string dict) i)))

(defun make-dictionary (trees)
  (let ((hash (make-hash-table :test 'equal)))
    (loop
       :for tree :in trees
       :for i :from 0
       :do
       (setf (gethash (tree->string tree) hash) i))
    (make-dict :empty-element-string=>index hash
	       :index=>empty-element-string (map 'simple-vector #'tree->string trees))))

(defun dictionary-save (dict file)
  (write-trees (map 'list #'string->tree (dictionary-index=>empty-element-string dict)) file))

(defun dictionary-load (file)
  (make-dictionary (read-trees file)))

(defun extract-empty-trees (tree)
  (cond
    ((node-empty? tree)
     (list tree))
    ((eq (type-of tree) 'terminal-node)
     nil)
    (t
     (mapcan #'extract-empty-trees (children tree)))))

;; annotate-empty-element
(defun annotate-empty-element (tree dict)
  (cond
    ((eq (type-of tree) 'terminal-node)
     tree)
    (t
     (let ((head (position-if-not #'node-empty? (children tree)))
	   (children nil))

       (setf children (list (annotate-empty-element (nth head (children tree)) dict)))

       (loop
	  :for r :from (1+ head) :below (length (children tree))
	  :do
	  (acond
	   ((and (node-empty? (nth r (children tree)))
		 (get-index (nth r (children tree)) dict))
	    (setf children
		  (list (make-internal-node :label (make-label :category (cat tree)
							       :annotations (list (format nil "RE~a" it)))
					    :children children))))
	   (t
	    (when (node-empty? (nth r (children tree)))
	      (write-tree (nth r (children tree)) t)
	      (format t " does not exist in the dictionary.~%"))
	    (setf children (append1 children (annotate-empty-element (nth r (children tree)) dict))))))

       (loop
	  :for l :from (- head 1) :downto 0
	  :do
	  (acond
	   ((and (node-empty? (nth l (children tree)))
		 (get-index (nth l (children tree)) dict))
	    (setf children
		  (list (make-internal-node :label (make-label :category (cat tree)
							       :annotations (list (format nil "LE~a" it)))
					    :children children))))
	   (t
	    (when (node-empty? (nth l (children tree)))
	      (write-tree (nth l (children tree)) t)
	      (format t " does not exist in the dictionary.~%"))

	    (setf children (cons (annotate-empty-element (nth l (children tree)) dict) children)))))

       (make-internal-node :label (node-label tree)
			   :children children)))))

;; replace-tag
(defun replace-tag (node terminals)
  (labels
      ((aux (node terminals)
	 (case (type-of node)
	   (terminal-node
	    (cond
	      ((null terminals)
	       (error "The length of the terminals is shorter than that of the sentence of the tree."))
	      ((string= (word node) (word (first terminals)))
	       (values (first terminals) (rest terminals)))
	      (t
	       (error "A word of a node is not identical to its corresponding terminal."))))
	   (internal-node
	    (let ((new-children nil))
	      (dolist (c (children node))
		(multiple-value-bind (new ts) (aux c terminals)
		  (push new new-children)
		  (setf terminals ts)))
	      (values (make-internal-node :label (node-label node)
					  :children (nreverse new-children))
		      terminals))))))
    (multiple-value-bind (new ts) (aux node terminals)
      (when ts
	(error "The length of the terminals is longer than that of the sentence of the tree."))
      new)))

(defun ptb->cf (&key ptb cf dictionary tag (nld? t))
  (let ((hash (make-hash-table :test 'equal))
	(dict nil)
	(result nil))

    (when (and nld? (probe-file dictionary))
      (format t "Dictionary file ~a is used." dictionary)
      (setf dict (dictionary-load dictionary)))

    (dolist (tree (read-trees ptb))
      (let ((x nil))
	(cond
	  (nld?
	   (setf x
		 (remove-recursive-unary-rule
		  (remove-empty-element
		   (annotate-nonlocal-dependency tree)))))
	  (t
	   (setf x
		 (remove-annotation
		  (remove-recursive-unary-rule
		   (remove-nodes-if #'(lambda (x) (cat= x "-NONE-")) tree))))))
	(push x result)
	(when (null dict)
	  (dolist (e (extract-empty-trees x))
	    (setf (gethash (tree->string e) hash) t)))))
    (setf result (nreverse result))

    (when (and (null dict) nld?)
      (setf dict (make-dictionary (mapcar #'string->tree (hash-keys hash))))
      (dictionary-save dict dictionary))

    (when nld?
      (setf result
	    (mapcar #'(lambda (x) (annotate-empty-element x dict)) result)))

    (when tag
      (setf result
	    (mapcar #'(lambda (x y) (replace-tag x (terminals y)))
		    result
		    (read-trees tag))))

    (write-trees result cf))
  t)

;;; cf->ptb
;; recover-empty-element
(defun left-empty? (node)
  (awhen (find-if #'(lambda (x) (scan "LE" x)) (label-annotations (node-label node)))
    (read-from-string (regex-replace-all "LE" it ""))))

(defun right-empty? (node)
  (awhen (find-if #'(lambda (x) (scan "RE" x)) (label-annotations (node-label node)))
    (read-from-string (regex-replace-all "RE" it ""))))

(defun recover-empty-element (tree dict)
  (cond
    ((eq (type-of tree) 'terminal-node)
     tree)
    (t
     (labels
	 ((recover-children (trees)
	    (let ((result nil))
	      (loop
		 :for tree := (pop trees)
		 :while tree
		 :do
		 (acond
		  ((node-empty? tree)
		   (push tree result))
		  ((left-empty? tree)
		   (setf trees (append (list (get-empty-element it dict)) (children tree) trees)))
		  ((right-empty? tree)
		   (setf trees (append (children tree) (list (get-empty-element it dict)) trees)))
		  (t
		   (push (recover-empty-element tree dict) result))))
	      (nreverse result))))
       (make-internal-node :label (node-label tree)
			   :children (recover-children (children tree)))))))

;; recover-nonlocal-dependency
(defun recover-nonlocal-dependency (tree)
  (let ((parent-tab (make-hash-table :test 'eq))
	(prev-tab (make-hash-table :test 'eq))
	(next-tab (make-hash-table :test 'eq))
	(index-tab (make-hash-table))
	(nodes (preorder tree)))
    (labels
	((traverse (node parent)
	   (setf (gethash node parent-tab) parent)
	   (when (eq (type-of node) 'internal-node)
	     (loop
		:for c1 :in (children node)
		:for c2 :in (rest (children node))
		:do
		(setf (gethash c1 next-tab) c2)
		(setf (gethash c2 prev-tab) c1))
	     (dolist (c (children node))
	       (traverse c node))))

	 (parent (node)
	   (gethash node parent-tab))

	 (ancestors (node)
	   (cond
	     ((null node)
	      nil)
	     (t
	      (cons node (ancestors (parent node))))))

	 (left-siblings (node)
	   (let ((prev (gethash node prev-tab)))
	     (cond
	       ((null prev)
		nil)
	       (t
		(cons prev (left-siblings prev))))))

	 (right-siblings (node)
	   (let ((next (gethash node next-tab)))
	     (cond
	       ((null next)
		nil)
	       (t
		(cons next (right-siblings next))))))

	 (c-cmd-left (node)
	   (cond
	     ((null node)
	      nil)
	     (t
	      (append (left-siblings node) (c-cmd-left (parent node))))))

	 (c-cmd-right (node)
	   (cond
	     ((null node)
	      nil)
	     (t
	      (append (right-siblings node) (c-cmd-right (parent node))))))

	 (set-id (node id)
	   (setf (id node) id)
	   (setf (gethash id index-tab) t))

	 (rule-*-L (node)
	   (awhen (find-if #'SBJ? (c-cmd-left node))
	     (set-id node (id it))))

	 (rule-*-L-OBJCTRL (node)
	   (awhen (find-if #'(lambda (x) (and (or (NP? x)
						  (and (cat= x "PP")
						       (find-if #'NP? (children x))))
					      (cat= (parent x) "VP")))
			   (c-cmd-left node))
	     (cond
	       ((cat= it "PP")
		(set-id node (id (find-if #'NP? (children it)))))
	       (t
		(set-id node (id it))))))

	 (rule-*-R (node)
	   (awhen (find-if #'SBJ? (c-cmd-right node))
	     (set-id node (id it))))

	 (rule-*T*-L (node)
	   (let ((tag (filler-tag (cat (parent node)) "*T*")))
	     (awhen (find-if #'(lambda (x) (annotation? tag x)) (c-cmd-left node))
	       (set-id node (id it)))))

	 (rule-*T*-A (node)
	   (let ((fcat (cat (parent node))))
	     (awhen (find-if #'(lambda (x) (cat= x fcat))
			     (member-if #'(lambda (x) (cat= x "PRN")) (ancestors node)))
	       (set-id node (id it)))))

	 (rule-*EXP* (node)
	   (awhen (find-if #'(lambda (x) (and (NP? x)
					      (eq (type-of (node-ref x 0)) 'terminal-node)
					      (string-equal "it" (word (node-ref x 0)))))
			   (c-cmd-left node))
	     (setf (children it)
		   (list (string->tree (format nil "(NP (PRP ~a))" (word (node-ref it 0))))
			 (string->tree (format nil "(~a (-NONE- *EXP*))" (cat node)))))
	     (set-id (node-ref it 1 0) (id node))))

	 (rule-*RNR* (node)
	   (let ((tag (filler-tag (cat (parent node)) "*RNR*")))
	     (awhen (find-if #'(lambda (x) (annotation? tag x)) (c-cmd-right node))
	       (set-id node (id it)))))

	 (rule-*ICH*-L (node)
	   (let ((tag (filler-tag (cat (parent node)) "*ICH*")))
	     (dolist (a (ancestors node))
	       (dolist (l (left-siblings a))
		 (dolist (x (preorder-right-first l))
		   (when (annotation? tag x)
		     (set-id node (id x))
		     (return-from rule-*ICH*-L t)))))))

	 (rule-*ICH*-R (node)
	   (let ((tag (filler-tag (cat (parent node)) "*ICH*")))
	     (dolist (a (ancestors node))
	       (dolist (l (right-siblings a))
		 (dolist (x (preorder l))
		   (when (annotation? tag x)
		     (set-id node (id x))
		     (return-from rule-*ICH*-R t))))))))

      (traverse tree nil)

      (loop
	 :for n :in nodes
	 :for i :from 1
	 :do
	 (when (eq (type-of n) 'internal-node)
	   (setf (id n) i)))

      (dolist (n nodes)
	(when (empty-element? n)
	  (cond
	    ((etype= n "*")
	     (cond
	       ((annotation? "DIRL" n)
		(cond
		  ((annotation? "OBJCTRL" n)
		   (rule-*-L-OBJCTRL n))
		  (t
		   (rule-*-L n))))
	       ((annotation? "DIRR" n)
		(rule-*-R n))))
	    ((etype= n "*T*")
	     (cond
	       ((annotation? "DIRL" n)
		(rule-*T*-L n))
	       ((annotation? "DIRA" n)
		(rule-*T*-A n))))

	    ((etype= n "*RNR*")
	     (rule-*RNR* n))

	    ((etype= n "*ICH*")
	     (cond
	       ((annotation? "DIRL" n)
		(rule-*ICH*-L n))
	       ((annotation? "DIRR" n)
		(rule-*ICH*-R n))))))

	(when (find-if #'(lambda (x) (scan "\\*EXP\\*" x)) (annotations n))
	  (rule-*EXP* n))))

    (dolist (n nodes)
      (setf (label-functions (node-label n)) nil)
      (setf (label-annotations (node-label n)) nil)
      (when (not (gethash (id n) index-tab))
	(setf (id n) nil)))
    tree))

(defun cf->ptb (&key cf ptb dictionary)
  (let ((dict (dictionary-load dictionary)))
    (write-trees (mapcar #'(lambda (x) (recover-nonlocal-dependency (recover-empty-element x dict)))
			 (read-trees cf))
		 ptb)))
