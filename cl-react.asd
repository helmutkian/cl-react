;;;; cl-react.asd

(asdf:defsystem #:cl-react
  :description "Describe cl-react here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:parenscript)
  :serial t
  :components ((:file "package")
               (:file "cl-react")
               (:file "psx")))

