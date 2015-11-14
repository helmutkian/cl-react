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

