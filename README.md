# cl-react
Common Lisp (Parenscript) utilities for building web apps in ReactJs
 
### Installation

* Clone the repo ``git clone https://github.com/helmutkian/cl-react.git``
* Fire up your Common Lisp environment
* Load the ASDF system definition ``(load "<path to cl-react.asd">)``
* Load the system ``(ql:quickload 'cl-react)``
* Build the JavaScript library ``(cl-react:build)``

### PSX

PSX is a Parenscript equivilent to JSX, ReactJs's extended JavaScript syntax. It uses the familiar CL-WHO syntax for markup generation.

````common-lisp
(ps:ps 
  (psx 
    (:a :href "http://www.google.com"
      (:span :class "text-green" "Click here!"))))
````

=>

````javascript
  React.DOM.a({ href: 'http://www.google.com' }, [
    React.DOM.span({ className: 'text-green' }, "Click here")
  ]);
````
