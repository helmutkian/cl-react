(in-package #:cl-react)

(defun destruct-bind (&optional lambda-list expr &rest body)
  (if (null body)
      `(progn ,lambda-list)
      `(ps:destructuring-bind ,lambda-list ,expr
	 ,(apply #'destruct-bind body))))
  
(ps:defpsmacro destructuring-bind* (forms &body body)
  (apply #'destruct-bind `(,@forms ,@body)))

(ps:defpsmacro cl-react:with-state (forms &body body)
  "Convience macro for declaring React#useState hooks

  WITH-STATE (({ var init-value } | (var set-fn) init-value})*) body 

  => const [var, set-fn] = React.useState(init-value); ...body;

  If set-fn is not defined, a function will be automatically bound to the symbol SET-{var}, for example:

  (with-state ((foo 1)) (set-foo (1+ foo)))
  "
  (apply #'destruct-bind
	 `(,@(loop for (binding init-value) in forms
		   if (atom binding)
		     append `((,binding ,(alexandria:symbolicate 'set- binding))
			      (ps:chain |React| (use-state ,init-value)))
		   else
		     append `(,binding (ps:chain |React| (use-state ,init-value))))
	   ,@body)))

(ps:defpsmacro cl-react:use-effect (dependencies &body body)
  "Convenience macro for declaring React#useEffect hooks
   
   USE-EFFECT { (dependencies) | undefined } body

   => React.useState(function () { ...body }, [...dependencies] | undefined); ...body;

   If dependencies is set to symbol UNDEFINED, then React.useState(function () { ... }, undefined)
   will be generated.
 "
  `(ps:chain |React|
	     (use-state (lambda () ,@body)
			,(if (eql dependencies 'undefined)
			     dependencies
			     `(array ,@dependencies)))))

(ps:defpsmacro cl-react:with-callback (forms &body body)
  "Convenience macro for declaring React#useCallback hooks

  WITH-CALLBACK ((callback-var { (dependencies) | undefined } callback-body)*) body

  => var callbackVar = React.useCallback(function () { ...callbackBody }, [...dependencies] | undefined); ...body;
  "
  `(let ,(loop for (binding dependencies . lambda-body) in forms
	       collect `(,binding (ps:chain |React|
					    (use-callback (lambda () ,@lambda-body)
							  ,(if (eql dependencies 'undefined)
							       dependencies
							       `(array ,@dependencies))))))
     ,@body))

(ps:defpsmacro with-memo (forms &body body)
  "Convenience macro for declaring React#useMemo hooks

  WITH-MEMO ((memoized-var { (dependencies) | undefined } fn-body)*) body

  => var memoizedVar = React.useCallback(function () { ...fnBody }, [...dependencies] | undefined); ...body;
  "
  `(let ,(loop for (binding dependencies . lambda-body) in forms
	       collect `(,binding (ps:chain |React|
					    (use-memo (lambda () ,@lambda-body)
							  ,(if (eql dependencies 'undefined)
							       dependencies
							       `(array ,@dependencies))))))
     ,@body))

(ps:defun cl-react:use-context (context)
  "Convenience function wrapper for React#useContect

   USE-CONTEXT context

   => React.useContext(context)
  "
  (ps:chain |React| (use-context context)))

(ps:defun cl-react:use-ref (init-value)
  "Convenience function wrapper for React#use-ref

   USE-REF init-value

   => React.useRef(initValue)
  "
  (ps:chain |React| (use-ref init-value)))


      
	 

