# cl-react
Common Lisp (Parenscript) utilities for building web apps in ReactJs

### PSX

PSX is a Parenscript equivilent to JSX, ReactJs's extended JavaScript syntax. It uses the familiar CL-WHO syntax for markup generation.

````
(ps:ps 
  (psx 
    (:a :href "http://www.google.com"
      (:span :class "text-green" "Click here!"))))
````

=>

````
  React.createElement('a', { href: 'http://www.google.com' }, [
    React.createElement('span', { className: 'text-green }, ["Click here"]
  ]);
````
