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
  (let ((*package* (find-package 'cl-react)))
    (ps-compile-file
     (asdf:system-relative-pathname 'cl-react "cl-react.lisp"))))

(defun %needs-thisref-p (code)
  "Tries to determine if 'code' needs access to 'this', the instance of the component defined by def-component"
  (some (alexandria:rcurry #'member '(prop state set-state %thisref))
        (alexandria:flatten code)))

(defun %add-thisref-binding (code)
  "Wrap code in a rebinding of 'this', so that macros in the code can find it even when it has been stomped. Do so only if the code uses those macros."
  (if (%needs-thisref-p code)
      `((let ((%thisref this))
          ,@code))
      code))

(defun %add-thisref-lambda (code)
  (if (%needs-thisref-p code)
      (let ((args (gensym)))
        `(lambda (&rest ,args)
           (let ((%thisref this))
             (apply ,code ,args))))
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
                                 ,@(%add-thisref-binding  (list (car params))))))
             ,@(when name
                     `(#:display-name ',name))
             ,@(loop for (k v) on (cdr params) by #'cddr
                  collect k
                  collect (%add-thisref-lambda v))))))
    `(macrolet
         ((cl-react:prop (&rest params)
            `(chain %thisref #:props ,@params))
          (cl-react:state (&rest params)
            `(chain %thisref #:state ,@params))
          (cl-react:set-state (&rest params)
            `(chain %thisref (#:set-state (create ,@params)))))
       ,(if name
            `(ps:var ,name ,classcode)
            classcode))))

(defpsmacro cl-react:def-pure-component (name &body params)
  "Like def-component, but uses javascript classes internally, so some naming conventions must be
handled differently."
  (let*
      ((constructors
        (remove-if-not (lambda (x) (and (listp x) (string-equal (car x) 'constructor)))
                       (cdr params)))
       (others
        (remove-if (lambda (x) (and (listp x) (string-equal (car x) 'constructor)))
                   (cdr params)))
       (constructor
        (case (length constructors)
          (0 `(defun ,name () (chain #:-react #:-pure-component (#:call #:this))))
          (1 `(defun ,name ,@(cddr constructors)))
          (otherwise (error "Component can't have more than one constructor")))))
    `(macrolet
         ((cl-react:prop (&rest params)
            `(chain %thisref #:props ,@params))
          ;;FIXME: do pure components use state?
          (cl-react:state (&rest params)
            `(chain %thisref #:state ,@params))
          (cl-react:set-state (&rest params)
            `(chain %thisref (#:set-state (create ,@params)))))
       (setf (@ ,name #:prototype)
             (chain #:-object (create (@ #:-react #:-pure-component #:prototype))))
       (setf (@ ,name #:prototype #:constructor) ,name)
       (setf (@ ,name #:prototype #:render)
             (lambda () ,@(%add-thisref-binding (list (car params)))))
       ,constructor
       ,@(mapcar
          (lambda (item)
            `(setf (@ ,name #:prototype ,(car item))
                   (lambda ,(second item) ,@(%add-thisref-binding (cddr item)))))
          others))))

(defpsmacro cl-react:prop (&rest params)
  `(chain this #:props ,@params))

(defpsmacro cl-react:state (&rest params)
  `(chain this #:state ,@params))

(defpsmacro cl-react:set-state (&rest params)
  `(chain this (#:set-state (create ,@params))))
