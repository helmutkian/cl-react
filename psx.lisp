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
  '(:MATH :SVG :A :ABBR :ADDRESS :AREA :ARTICLE :ASIDE :AUDIO :B :BASE :BDI :BDO
 :BLOCKQUOTE :BODY :BR :BUTTON :BUTTON :BUTTON :BUTTON :CANVAS :CAPTION :CITE
 :CODE :COL :COLGROUP :COMMAND :COMMAND :COMMAND :COMMAND :DATALIST :DD :DEL
 :DETAILS :DFN :DIV :DL :DT :EM :EMBED :FIELDSET :FIGCAPTION :FIGURE :FOOTER
 :FORM :H1 :H2 :H3 :H4 :H5 :H6 :HEAD :HEADER :HGROUP :HR :HTML :I :IFRAME :IMG
 :INPUT :INS :KBD :KEYGEN :LABEL :LEGEND :LI :LINK :MAP :MARK :MENU
 :META :META :META :META :META :META :METER :NAV :NOSCRIPT :OBJECT :OL
 :OPTGROUP :OPTION :OUTPUT :P :PARAM :PRE :PROGRESS :Q :RP :RT :RUBY :S :SAMP
 :SCRIPT :SECTION :SELECT :SMALL :SOURCE :SPAN :STRONG :STYLE :SUB :SUMMARY
 :SUP :TABLE :TBODY :TD :TEXTAREA :TFOOT :TH :THEAD :TIME :TITLE :TR :TRACK :U
    :UL :VAR :VIDEO :WBR))
	    
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
	(let* ((type-sym
		(make-symbol (string type)))
	       (props-obj
		(cond
		  (props `((ps:create ,@props)))
		  (children (list nil))))
	       (is-dom-type (dom-type-p type))
	       (inner-form
		(if is-dom-type
		    `(,type-sym ,@props-obj)
		    `(create-element ,type-sym ,@props-obj))))
	  (values
	   (if is-dom-type
	       `(ps:chain React DOM ,inner-form)
	       `(ps:chain React ,inner-form))
	   (cdr inner-form)
	   children)))))

(defun compile-tree (parsed-tree)
  (let (root)
    (traverse-tree
     (lambda (&key parent child)
       (multiple-value-bind (outer-form inner-form children) (compile-node child)
	 (when (null root)
	   (setf root outer-form))
	 (when parent
	   (cond
	     ((null (cdr parent))
	      (push outer-form (cdr parent)))
	     ((and (listp (cadr parent))
		   (eql (caadr parent) 'list))
	      (push outer-form (cdadr parent)))
	     (t
	      (setf (cadr parent)
		    (list 'list outer-form (cadr parent))))))
	 (values inner-form children)))
     parsed-tree)
    root))
	 
(ps:defpsmacro psx (form)
  (compile-tree (parse-tree form)))
