;(in-package #:cl-react)
 
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


(defconstant +dom-types+
  '(:math :svg :a :abbr :address :area :article :aside :audio :b :base :bdi :bdo
 :blockquote :body :br :button :button :button :button :canvas :caption :cite
 :code :col :colgroup :command :command :command :command :datalist :dd :del
 :details :dfn :div :dl :dt :em :embed :fieldset :figcaption :figure :footer
 :form :h1 :h2 :h3 :h4 :h5 :h6 :head :header :hgroup :hr :html :i :iframe :img
 :input :ins :kbd :keygen :label :legend :li :link :map :mark :menu
 :meta :meta :meta :meta :meta :meta :meter :nav :noscript :object :ol
 :optgroup :option :output :p :param :pre :progress :q :rp :rt :ruby :s :samp
 :script :section :select :small :source :span :strong :style :sub :summary
 :sup :table :tbody :td :textarea :tfoot :th :thead :time :title :tr :track :u
    :ul :var :video :wbr))
	    
(defparameter *attr-synonyms*
  '(:readonly :read-only
    :class :class-name)
  "HTML attributes that differ from JSX or that need some doctoring to work with Parenscript")

(defun parse-attr (attr value)
  (let ((jsx-attr (or (getf *attr-synonyms* attr) attr)))
    (when (or (not (find jsx-attr *binary-attrs*)) value)
	(list (make-symbol (string jsx-attr)) value))))
	

(defun parse-node (node)
  (let (done attr props)
    (if (atom node)
	node
	(do* ((tokens (rest node) (funcall (if done #'identity #'rest) tokens))
	      (token (first tokens) (first tokens)))
	     ((or done (null tokens))
	      (list :type (first node) :props props :children tokens))
	  (cond (attr (setf props (list* attr token props)
			    attr nil))
		((keywordp token) (setf attr token))
		(t (setf done t)))))))

(defun traverse-tree (function tree)
  (let ((stack (list (list :parent nil :child tree))))
    (do ((top (pop stack) (pop stack)))
	((null top))
      (multiple-value-bind (node children) (apply function top)
	(dolist (child children)
	  (push (list :parent node :child child) stack))))))
	  
	    	      
(defun parse-tree (tree)
  (let (root parsed-node children)
      (traverse-tree
       (lambda (&key child parent)
	 (setf parsed-node (parse-node child)
	       children (and (listp parsed-node) (getf parsed-node :children)))
	 (when (null root)
	   (setf root parsed-node))
	 (when children
	   (setf (getf parsed-node :children) nil))
	 (when parent
	   (push parsed-node (getf parent :children)))
	 (values parsed-node children))
       tree)
      root))
		     
  
(defun dom-type-p (type)
  (find type +dom-types+))

(defun compile-node (parsed-node)
  (if (atom parsed-node)
      parsed-node
      (destructuring-bind (&key type props children) parsed-node
	(let ((type-sym
		(make-symbol (string type)))
	       (props-obj
		(cond
		  (props `((ps:create ,@props)))
		  (children (list nil)))))
	  (values
	   (if (dom-type-p type)
	       `(ps:chain React DOM (,type-sym ,@props-obj))
	       `(ps:chain React (create-element ,type-sym ,@props-obj)))
	   children)))))

(defun compile-tree (parsed-tree)
  (let ((adjacency-table (make-hash-table))
	(root nil))
    (traverse-tree
     (lambda (&key parent child)
       (multiple-value-bind (compiled-node children) (compile-node child)
	 (push compiled-node (gethash parent adjacency-table))
	 (values compiled-node children)))
     parsed-tree)
    (maphash
     (lambda (parent children)
       (if (null parent)
	   (setf root (first children))
	   (setf (cdr (last (car (last parent))))
		 (if (rest children)
		     `((list ,@children))
		     `(,(first children))))))
       adjacency-table)
     root))
	     	 
(ps:defpsmacro psx (form)
  (compile-tree (parse-tree form)))
