(in-package #:cl-react)

(defvar *test*
  '(:a 
    :href "#"
    :disabled t
    (:span :class "text-primary" "Some text")
    (:span :class "text-muted" (:i :class "pencil-icon"))))

(defparameter *binary-attrs*
  '(:read-only
    :disabled
    :hidden)
  "True/false attributes. When set to NIL, they are simply ignored.")

(defparameter *attr-synonyms*
  '(:readonly :read-only
    :class :class-name)
  "HTML attributes that differ from JSX or that need some doctoring to work with Parenscript")

(defun parse-attr (attr value)
  (let ((jsx-attr (or (getf *attr-synonyms* attr) attr)))
    (when (or (not (find jsx-attr *binary-attrs*)) value)
	(list (make-symbol (string jsx-attr)) value))))
	
(defun parse-node (node)
  "Parses a quoted PSX node into a plist with the fields :TYPE, :PROPS:, and :CHILDREN.
   Does not parse the child nodes."
  (if (atom node)
      node
      (let (done attr props)
	(do* ((tokens (rest node)
		      (funcall (if done #'identity #'rest) tokens))
	      (token (first tokens)
		     (first tokens)))
	     ((or done (null tokens))
	      `(:type ,(first node) :props ,props :children (list ,@tokens)))
	  (cond
	    ((keywordp token)
	     (setf attr token))
	    (attr
	     (setf props (append (parse-attr attr token) props)
		   attr nil))
	    (t
	     (setf done t)))))))
	            
(defun parse-node* (node)
  "Fully recursive parse of a PSX node, i.e. also parses child nodes"
  (let* ((parent (parse-node node)))
    (if (atom parent)
	parent
	`(:type ,(getf parent :type)
	  :props ,(getf parent :props)
	  :children ,(mapcar #'parse-node* (getf parent :children))))))	

(defun compile-node (parsed-node)
  (if (atom parsed-node)
      parsed-node
      `(ps:chain |React|
		 (create-element ,(getf parsed-node :type)
				 (ps:create ,@(getf parsed-node :props))
				 ,(mapcar #'compile-node (getf parsed-node :children))))))

(ps:defmacro+ps psx (body)
  (compile-node (parse-node* body)))
