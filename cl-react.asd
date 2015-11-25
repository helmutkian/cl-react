<<<<<<< HEAD
;;;; cl-react.asd

(asdf:defsystem #:cl-react
  :description "Common Lisp (Parenscript) utilities for building web apps in ReactJs"
  :author "Helmut Kian Rohrbacher"
  :license ""
  :depends-on (#:parenscript)
  :serial t
  :components ((:file "package")
               (:file "cl-react")
               (:file "psx")))
=======
(asdf:defsystem #:cl-react
  :description "cl-react"
  :long-description "Parenscript wrapper for ReactJs"
  :author "Helmut Kian Rohrbacher <github.com/helmutkian>"
  :license "MIT"
  :description "Common Lisp (Parenscript) utilities for building web apps in ReactJs"
  :depends-on (#:parenscript)
  :components ((:file "package")
	       (:file "psx"
		      :depends-on ("package"))
	       (:file "utils"
		      :depends-on ("package" "psx"))))
>>>>>>> 70012abb67bb06bbbb997f610d7fe8975cb250d9

