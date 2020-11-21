(in-package :cl-react.psx)

(defun strip-whitespace (string)
  (remove-if (lambda (c) (find c '(#\space #\tab #\newline)))
	     string))

(defun test-psx* (psx-form expected-js)
  (let* ((compiled-psx (ps:ps* (compile-psx psx-form)))
	(result (string= (strip-whitespace compiled-psx)
			 (strip-whitespace expected-js))))
    (format t
	    " PSX:      ~s ~% ACTUAL:   ~a ~% EXPECTED: ~a ~% RESULT:   ~a~%"
	    psx-form
	    compiled-psx
	    expected-js
	    (if result "passed" "failed"))))

(defmacro test-psx (psx-form expected-js)
  `(test-psx* ',psx-form ,expected-js))

(defmacro define-test-suite (name &body tests)
  `(defun ,name ()
     ,@(mapcar (lambda (test)
		 `(progn ,test
			(format t "-----------~%")))
	       tests)))

(define-test-suite psx-tests
    (test-psx
     (:br)
     "React.DOM.br();")
    
    (test-psx
     (:span :class-name "icon")
     "React.DOM.span(cl_react_mergeObjects({ className : 'icon' }));")
    
    (test-psx
     (:span "Test")
     "React.DOM.span(null, 'Test');")
    
    (test-psx
     (:a :href "#"
	 (:span "Test"))
     "React.DOM.a(cl_react_mergeObjects({ href : '#' }), React.DOM.span(null, 'Test'));")
    
    (test-psx
     (:p (:b "Bold")
	 (:i "Italic"))
     "React.DOM.p(null, [React.DOM.b(null, 'Bold'), React.DOM.i(null, 'Italic')]);")

  (test-psx
   (:fragment (:td "foo") (:td "bar"))
   "React.createElement(React.Fragment, null, [React.DOM.td(null, 'foo'), React.DOM.td(null, 'bar')]);")

  (test-psx
   (:foo.bar)
   "React.createElement(foo.bar);"))
   
  
