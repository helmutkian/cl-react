
(unless #:-react
  (setf #:-react (require "react")))
(setf (@ #:-react #:create-class) (require "create-react-class"))
(setf (@ #:-react #:-d-o-m) (require "react-dom-factories"))

(cl-react::define-react-function cl-react:create-class (object-specification))

(cl-react::define-react-function cl-react:create-element (type &optional props children))

(cl-react::define-react-function cl-react:create-factory (type))

(cl-react::define-react-function cl-react:clone-element (type &optional props children))

(defun cl-react:render (element container &optional callback)
  (chain #:|ReactDOM| (#:render element container callback)))

(cl-react::define-react-function cl-react:unmount-component-at-node (container))

(cl-react::define-react-function cl-react:render-to-string (element))

(cl-react::define-react-function cl-react:render-to-static-markup (element))

(cl-react::define-react-function cl-react:is-valid-element (object)
  :nicknames (valid-element-p))

(cl-react::define-react-function cl-react:|findDOMNode| (component)
				 :nicknames (cl-react:find-dom-node))

(defun cl-react:children-map (fn &optional context)
  (chain #:|React| "Children" (map fn context)))

(defun cl-react:children-for-each (fn &optional context)
  (chain #:|React| "Children" (map fn context)))

(defun cl-react:children-count ()
  (chain #:|React| "Children" (count)))

(defun cl-react:children-only ()
  (chain #:|React| "Children" (only)))

(defun cl-react:merge-objects (&rest objects)
  (let ((result (create)))
    (chain objects
	      (#:for-each
	       (lambda (obj)
		 (chain #:|Object|
			(#:keys obj)
			(#:for-each (lambda (key) (setf (getprop result key)
                                      (getprop obj key))))))))
    result))
