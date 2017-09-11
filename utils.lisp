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

(defparameter cl-react:with-ps 'with-ps)

(defun build ()
  (let ((*package* (find-package 'cl-react)))
    (ps-compile-file
     (asdf:system-relative-pathname 'cl-react "cl-react.lisp"))))

(defun %needs-thisref-p (code)
  "Tries to determine if 'code' needs access to 'this', the instance of the component defined by def-component"
  (some (alexandria:rcurry #'member '(prop state set-state component-this-ref))
        (alexandria:flatten code)))

(defun %add-thisref-binding (code &key (lambda-wrap t))
  "Wrap code in a rebinding of 'this', so that macros in the code can find it even when it has been stomped. Do so only if the code uses those macros."
  (if (%needs-thisref-p code)
      (if lambda-wrap
          (let ((args (gensym)))
            `(lambda (&rest ,args)
               (let ((component-this-ref this))
                 (apply ,code ,args))))
          `(let ((component-this-ref this))
             ,code))
      code))

(defpsmacro cl-react:def-component (name &body params)
  "A convenience wrapper macro for create-class. The created class will be
assigned to the name specified by the first variable. The second value is
code to be placed in the render method. It will be automatically wrapped in
an anonymous function. The remainder of the parameters are key/value pairs
That will become attributes of the object.

If name is set to nil, the macro will return the class without attempting to
assign it to a variable.

If the first form of params is set to nil, the macro will not fill the render attribute. It can then be manually filled later in the params section."
  (let ((classcode
          `(cl-react:create-class
            (ps:create
             ,@(when (car params)
                     `(:render (lambda ()
                                 ,(%add-thisref-binding (car params)
                                                        :lambda-wrap nil))))
             ,@(when name
                     `(#:display-name ',name))
             ,@(loop for (k v) on (cdr params) by #'cddr
                  collect k
                  collect (%add-thisref-binding v))))))
    `(macrolet
         ((cl-react:prop (&rest params)
            `(chain component-this-ref #:props ,@params))
          (cl-react:state (&rest params)
            `(chain component-this-ref #:state ,@params))
          (cl-react:set-state (&rest params)
            `(chain component-this-ref (#:set-state (create ,@params)))))
       ,(if name
            `(ps:var ,name ,classcode)
            classcode))))

(defpsmacro cl-react:prop (&rest params)
  `(chain this #:props ,@params))

(defpsmacro cl-react:state (&rest params)
  `(chain this #:state ,@params))

(defpsmacro cl-react:set-state (&rest params)
  `(chain this (#:set-state (create ,@params))))
