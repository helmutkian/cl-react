(in-package #:cl-react)

(ps:defpsmacro define-react-function (name lambda-list &key nicknames)
  (let ((args
          (remove-if (lambda (sym) (find sym '(&optional &key))) lambda-list)))
    `(progn
       (defun ,name ,lambda-list
         (ps:chain |React| (,name ,@args)))
       ,@(mapcar (lambda (nickname)
                   `(defun ,nickname ,lambda-list (,name ,@args)))
                 nicknames))))

(ps:defpsmacro define-react-dom (name)
  `(defun ,name (&optiodnal props children)
     (ps:chain (ps:getprop |React| "DOM") (,name props children))))

(ps:defpsmacro define-react-children (name &optional lambda-list)
  (let ((args
          (remove-if (lambda (sym) (find sym '(&optional &key))) lambda-list))
        (fn-name
          (intern (map 'string
                       #'char-upcase
                       (concatenate 'string "children-" (string name))))))
    `(defun ,fn-name ,(cons 'children lambda-list)
       (ps:chain (ps:getprop |React| "Children")
                 (,name ,@args)))))

(ps:defpsmacro define-react-class (name &rest object-specification)
  `(defvar ,name (create-class ,object-specification)))


(defparameter *cl-react-lib*
  (ps:ps
    (define-react-function create-class (object-specification))

    (define-react-function create-element (type &optional props children))

    (define-react-function create-factory (type))

    (define-react-function render (element container &optional callback))

    (define-react-function unmount-component-at-node (container))

    (define-react-function render-to-string (element))

    (define-react-function render-to-static-markup (element))

    (define-react-function is-valid-element (object)
      :nicknames (valid-element-p))

    (define-react-function |findDOMNode| (component)
      :nicknames (find-dom-node))

    (define-react-children map (fn &optional context))

    (define-react-children for-each (fn &optional context))

    (define-react-children count)

    (define-react-children only)))

(ps:defpsmacro define-react-class (name render &rest other)
  "A convenience wrapper macro for create-class. The created class will be
assigned to the name specified by the first variable. The second value is
code to be placed in the render method. It will be automatically wrapped in
an anonymous function. The remainder of the parameters are key/value pairs
That will become attributes of the object.

If name is set to nil, the macro will return the class without attempting to
assign it to a variable.

If render is set to nil, the macro will not fill the render attribute. It can
then be manually filled in the rest section."
  (let ((classcode
          `(create-class
            (ps:create ,@(when render `(:render (lambda () ,render)))
                    ,@other))))
    (if name
        `(ps:var ,name ,classcode)
        classcode)))
