(in-package #:cl-react)

(defpsmacro define-react-function (name lambda-list &key nicknames)
  (let ((args
	 (remove-if (lambda (sym) (find sym '(&optional &key))) lambda-list)))
    `(progn
       (defun ,name ,lambda-list
         (chain #:|React| (,(make-symbol (string name)) ,@args)))
       ,@(mapcar (lambda (nickname)
		   `(defun ,nickname ,lambda-list (,name ,@args)))
		 nicknames))))

(defpsmacro cl-react:define-class (name &rest object-specification)
  `(defvar ,name
     (create-class (create ,@(reduce #'append object-specification)))))

(defpsmacro cl-react:psx (form)
  (compile-psx form))

(defpsmacro cl-react:set-state* (&body object-specification)
  `(chain this
	  (#:|setState| (create ,@object-specification))))

(defpsmacro cl-react:bind-lambda (args &body body)
  `(chain (lambda ,args ,@body)
	  (bind this)))

(defun build ()
  (ps-compile-file (asdf:system-relative-pathname 'cl-react "cl-react.lisp")))
