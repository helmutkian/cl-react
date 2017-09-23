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

###Example code

Below is an example based on the [React tutorial](https://facebook.github.io/react/docs/tutorial.html). The javascript version is followed by the equivalent in parenscript/cl-react.

````javascript
var data = [
  {id: 1, author: "Pete Hunt", text: "This is one comment"},
  {id: 2, author: "Jordan Walke", text: "This is *another* comment"}
];

var CommentForm = React.createClass({
  render: function() {
    return (
      <div className="commentForm">
        Hello, world! I am a CommentForm.
      </div>
    );
  }
});

var Comment = React.createClass({
  rawMarkup: function() {
    var rawMarkup = marked(this.props.children.toString(), {sanitize: true});
    return { __html: rawMarkup };
  },

  render: function() {
    return (
      <div className="comment">
        <h2 className="commentAuthor">
          {this.props.author}
        </h2>
        <span dangerouslySetInnerHTML={this.rawMarkup()} />
      </div>
    );
  }
});

var CommentList = React.createClass({
  render: function() {
    var commentNodes = this.props.data.map(function(comment) {
      return (
        <Comment author={comment.author} key={comment.id}>
          {comment.text}
        </Comment>
      );
    });
    return (
      <div className="commentList">
        {commentNodes}
      </div>
    );
  }
});

var CommentBox = React.createClass({
  render: function() {
    return (
      <div className="commentBox">
        <h1>Comments</h1>
        <CommentList data={this.props.data} />
        <CommentForm />
      </div>
    );
  }
});

ReactDOM.render(
  <CommentBox data={data} />,
  document.getElementById('content')
);
````

````common-lisp
(var data
     (list
      (create id 1 author "Peter Hunt" text "This is one comment")
      (create id 2 author "Jordan Walke" text "This is *another* comment")))

(def-component -comment-form
    (psx (:div :class-name "commentForm" "Hello, world! I am a CommentForm")))

(def-component -comment
    (psx
     (:div :class-name "comment"
           (:h2 :class-name "commentAuthor" (prop author))
           (:span :dangerously-set-inner-h-t-m-l (chain this (raw-markup)))))
  raw-markup
  (lambda ()
    (create __html (marked (prop children (to-string)) (create sanitize t)))))

(def-component -comment-list
    (let ((comment-nodes
            (mapcar
             (lambda (comment)
               (psx (:-comment :author (@ comment author) :key (@ comment id)
                               (@ comment text))))
             (prop data))))
      (psx (:div :class-name "commentList" comment-nodes))))

(def-component -comment-box
    (psx (:div :class-name "commentBox"
               (:h1 "Comments")
               (:-comment-list :data (prop data))
               (:-comment-form))))

(render (psx (:-comment-box :data data))
        (chain document (get-element-by-id "content")))
````
