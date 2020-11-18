(in-package #:cl-react)

(defpsmacro cl-react:with-state (forms &body body)
  "Convenience macro for declaring React#useState hooks

  WITH-STATE (({ var init-value } | (var set-fn) init-value})*) body 

  => const [var, setFn] = React.useState(init-value); ...body;

  If set-fn is not defined, a function will be automatically bound to the symbol SET-{var}, for example:

  (with-state ((foo 1)) (set-foo (1+ foo)))
  "
  `(destructuring-bind*
    ,(loop for (binding init-value) in forms
	   if (atom binding)
	     collect `((,binding ,(make-symbol (concatenate 'string "set-" (string binding))))
		       (chain #:|React| (use-state ,init-value)))
	   else
	     collect `(,binding (chain #:|React| (use-state ,init-value))))
    ,@body))


(defpsmacro cl-react:with-reducer (forms &body body)
  "Convenience macro for declaring React#useReducer hooks

   WITH-REDUCER (({ var | (var dispatch-fn) } { (reducer init-value) | (reducer init-value init-fn) })*) body
  
   => const [var, dispatchFn] = React.useReducer(reducer, initValue, initFn)

   If dispatch-fn is not defined, a function will be automatically bound to the symbol DISPATCH-{var}, for example:

   (with-reducer ((foo (reducer 1))) (dispatch-foo (1+ foo)))
  "
  `(destructuring-bind*
    ,(loop for (binding (reducer init-value . init-fn)) in forms
	   if (atom binding)
	     collect `((,binding ,(make-symbol (concatenate 'string ("dispatch-" (string binding)))))
		       (chain #:|React| (use-reducer ,reducer ,init-value ,@init-fn)))
	   else
	     collect `(,binding (chain #:|React| (use-state ,reducer ,init-value ,@init-fn))))
    ,@body))

  

(defpsmacro cl-react:use-effect (dependencies &body body)
  "Convenience macro for declaring React#useEffect hooks
   
   USE-EFFECT { (dependencies) | undefined } body

   => React.useState(function () { ...body }, [...dependencies] | undefined); ...body;

   If dependencies is set to symbol UNDEFINED, then React.useState(function () { ... }, undefined)
   will be generated.
 "
  `(chain #:|React|
	  (use-state (lambda () ,@body)
		     ,(if (eql dependencies 'undefined)
			  dependencies
			  `(array ,@dependencies)))))

(defpsmacro cl-react:with-callback (forms &body body)
  "Convenience macro for declaring React#useCallback hooks

  WITH-CALLBACK ((callback-var { (dependencies) | undefined } callback-body)*) body

  => var callbackVar = React.useCallback(function () { ...callbackBody }, [...dependencies] | undefined); ...body;
  "
  `(let ,(loop for (binding dependencies . lambda-body) in forms
	       collect `(,binding (chain #:|React|
					 (use-callback (lambda () ,@lambda-body)
						       ,(if (eql dependencies 'undefined)
							    dependencies
							    `(array ,@dependencies))))))
     ,@body))

(defpsmacro cl-react:with-memo (forms &body body)
  "Convenience macro for declaring React#useMemo hooks

  WITH-MEMO ((memoized-var { (dependencies) | undefined } fn-body)*) body

  => var memoizedVar = React.useCallback(function () { ...fnBody }, [...dependencies] | undefined); ...body;
  "
  `(let ,(loop for (binding dependencies . lambda-body) in forms
	       collect `(,binding (chain #:|React|
					 (use-memo (lambda () ,@lambda-body)
						   ,(if (eql dependencies 'undefined)
							dependencies
							`(array ,@dependencies))))))
     ,@body))


      
