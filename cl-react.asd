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

