(defpackage #:cl-react.psx
  (:nicknames #:psx)
  (:use #:cl #:parenscript)
  (:export #:compile-psx))
  
(defpackage #:cl-react
  (:nicknames #:react)
  (:use :cl :parenscript :cl-react.psx)
  (:export #:build
	   #:psx
	   #:define-class
	   #:set-state*
	   #:bind-lambda
           #:create-class
	   #:create-element
	   #:create-factory
	   #:render
	   #:unmount-component-at-node
	   #:render-to-string
	   #:render-to-static-markup
	   #:is-valid-element
	   #:valid-element-p
	   #:|findDOMNode|
	   #:find-dom-node
	   #:children-map
	   #:children-for-each
	   #:children-count
	   #:children-only
	   #:with-ps
     #:def-component
     #:prop
     #:state
     #:set-state
     #:merge-objects
     #:%thisref))

(setf (ps:ps-package-prefix :cl-react) "cl_react_")
