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

  => const [var, set-fn] = React.useState(init-value)

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

	 

