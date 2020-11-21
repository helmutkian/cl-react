(in-package :cl-react.psx)

;;; *******************************************************************************************

#.(progn
    (defvar *user-readtable-case* (readtable-case *readtable*))
    (setf (readtable-case *readtable*) :invert))

;;; *******************************************************************************************

(defparameter *binary-props*
  '(:read-only
    :disabled
    :hidden)
  "True/false attributes. When set to NIL, they are simply ignored.")


(defparameter *dom-types*
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

(defparameter *react-types*
  '(:fragment :-fragment))

(defparameter *react-type-symbols*
  '(:fragment -fragment))

(defparameter *prop-synonyms*
  '(:readonly :read-only
    :class :class-name)
  "HTML attributes that differ from JSX or that need some doctoring to work with Parenscript")

(defparameter *spread-prop-symbol*
  :...)

(defun psx-atom-p (form)
  "Is value atomic--i.e. non-traversable."
  (not (and (listp form)
	    (keywordp (first form)))))

(defun spread-prop-symbol-p (object)
  (and (symbolp object) (string-equal *spread-prop-symbol* object)))

(defun parse-prop (prop value)
  (let ((jsx-prop (or (getf *prop-synonyms* prop) prop)))
    (when (or (not (find jsx-prop *binary-props*)) value)
      (list (make-symbol (string jsx-prop))
	    value))))

(defun parse-props (form)
  (flet ((prop-key-p (token)
	   (or (spread-prop-symbol-p token)
	       (keywordp token))))
    (loop
       with prop-key = nil
       for rest-form on form
       for token = (first rest-form)
       until (and (not (prop-key-p token))
		  (null prop-key))
       if prop-key
         append (parse-prop prop-key token) into props and
         do (setf prop-key nil)
       else
         do (setf prop-key token)
       finally (return (values props rest-form)))))

(defun parse-node (node)
  (if (psx-atom-p node)
      node
      (multiple-value-bind (props children) (parse-props (rest node))
	(list :type (first node)
	      :props props
	      :children children))))


(defun traverse-tree (fn tree)
  (loop
     with stack = (list (list tree))
     for top = (pop stack)
     until (null top)
     do (loop
	   for child in (apply fn top)
	   do (push child stack))))

(defun make-branch (parsed-node)
  (if (psx-atom-p parsed-node)
      parsed-node
      (list :type (getf parsed-node :type)
	    :props (getf parsed-node :props)
	    :children nil)))

(defun parse-tree (tree)
  (let (root)
    (traverse-tree
     (lambda (child &optional parent)
       (let* ((parsed-node (parse-node child))
	      (branch (make-branch parsed-node)))
	 (if (null root)
	     (setf root branch)
	     (push branch (getf parent :children)))
	 (when (not (psx-atom-p parsed-node))
	   (mapcar (lambda (child) (list child branch))
		   (getf parsed-node :children)))))
     tree)
    root))


(defun dom-type-p (type)
  (find type *dom-types*))

(defun react-type-p (type)
  (find type *react-types*))

(defun compile-props (plist)
  (loop
     with key = nil
     for token in plist
     if (spread-prop-symbol-p key)
       collect token into prop-objs and
       do (setf key nil)
     else if key
       collect `(ps:create ,key ,token) into prop-objs and
       do (setf key nil)
     else
       do (setf key token)
     finally
       (return
	 (if prop-objs
	     `(react:merge-objects ,@prop-objs)
	     (list nil)))))

(defun split-string (str delimiter)
  (loop with substrs = (list nil)
	for char across str
	if (eql char delimiter)
	  do (push nil substrs)
	else
	  do (push char (car substrs))
	finally (return (loop for substr in (nreverse substrs)
			      collect (coerce (nreverse substr) 'string)))))
       
(defun compile-node (parsed-node)
  (if (psx-atom-p parsed-node)
      parsed-node
      (destructuring-bind (&key type props children) parsed-node
	(let* ((type-str (string type))
	       (type-sym (make-symbol type-str))
	       (props-form (cond ((and (null children) (null props)) nil)
				 ((null props) (list nil))
				 (t `(,(compile-props props)))))
	       (children-form (cond ((null children) nil)
				    ((rest children) (list (list 'array)))
				    (t (list nil)))))
	  (values (cond
		    ((dom-type-p type)
		     `(chain React DOM (,type-sym ,@props-form ,@children-form)))
		    ((react-type-p type)
		     (let ((react-type-sym (or (getf *react-type-symbols* type)
					       type-sym)))
		       `(chain React (create-element (ps:@ React ,react-type-sym) ,@props-form ,@children-form))))
		    ((find #\. type-str)
		     (let ((split-type (mapcar #'make-symbol (split-string type-str #\.))))
		       `(chain React (create-element (ps:@ ,@split-type) ,@props-form ,@children-form))))
		    (t
		     `(chain React (create-element ,type-sym ,@props-form ,@children-form))))
		  children)))))

(defun push-compiled-child (child compiled-node)
  (let* ((children-cell (last (first (last compiled-node))))
	 (children (first children-cell)))
    (if (null children)
	(setf (first children-cell) child)
	(push child (rest children)))))

(defun compile-tree (parsed-tree)
  (let (root)
    (traverse-tree
     (lambda (node &optional parent)
       (multiple-value-bind (compiled-node children) (compile-node node)
	 (if (null root)
	     (setf root compiled-node)
	     (push-compiled-child compiled-node parent))
	 (mapcar (lambda (child) (list child compiled-node)) children)))
     parsed-tree)
    root))


(defun compile-psx (form)
  (compile-tree (parse-tree form)))

;;; *******************************************************************************************

#.(setf (readtable-case *readtable*) *user-readtable-case*)

;;; *******************************************************************************************
