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
  (cl-react:psx 
    (:a :href (cl-react:with-ps (ps:@ this props site-url))
      (:span :class "text-green" (cl-react:with-ps (ps:@ this props site-name))))))
````

=>

````javascript
  React.DOM.a({ href: this.props.siteUrl }, [
    React.DOM.span({ className: 'text-green' }, this.props.siteName)
  ]);
````

### Convenience Functions / Macros

CL-React contains some convenience aliases for commonly used React functions:

<table>
<tr><th>Lisp function</th><th>React original</th></tr>
<tr><td>create-class</td><td>React.createClass</td></tr> 
<tr><td>create-element</td><td>React.createElement</td></tr>
<tr><td>create-factory</td><td>React.createFactory</td></tr>
<tr><td>unmount-component-at-node</td><td>React.unmountComponentAtNode</td></tr>
<tr><td>is-valid-element</td><td>React.isValidElement</td></tr>
<tr><td>find-dom-node</td><td>React.findDOMNode</td></tr>
<tr><td>render</td><td>ReactDOM.render</td></tr>
<tr><td>render-to-string</td><td>React.renderToString</td></tr>
<tr><td>render-to-static-markup</td><td>React.renderToStaticMarkup</td></tr>
<tr><td>children-map</td><td>React.Children.map</td></tr>
<tr><td>children-count</td><td>React.Children.count</td></tr>
<tr><td>children-only</td><td>React.Children.only</td></tr>

<tr><td>(prop <i>item</i>)</td><td>this.props.<i>item</i></td></tr>
<tr><td>(state <i>item</i>)</td><td>this.state.<i>item</i></td></tr>
<tr><td>(set-state <i>item</i> <i>value</i>)</td><td>this.setState(<i>item, value</i>)</td></tr>
</table>

####Def-component
The def-component macro is a wrapper around create-class.

A typical create-class might look like this:

````common-lisp
(var sample-component
     (create-class
       (create
         get-initial-state
         (lambda () ...)
         render
         (lambda () <component body here>)
         ...)))
````

Def-component tightens it up to this:

````common-lisp
(def-component sample-component
  <component body here>
  get-initial-state
  (lambda () ...)
  ...)
````

The body of the def-component macro is implicitly wrapped in a (create ...) call that, with the exception of the first item, must contain key/value pairs. The first body item will be automatically paired with the render key and enclosed in a lambda expression.

A nil in the name parameter will cause def-component to return a class without attempting to save it.

To skip the implicit render body - for example, when supplying an external render function - place nil as the first item in the body.

