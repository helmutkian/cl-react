(defun ajax (&key (type "GET") (cache t) url data success error)
  (chain $
	 (ajax (create :url url
		       :type type
		       :cache cache
		       :data-type "json"
		       :data data
		       :success success
		       :error error))))
	       

(define-class comment-box
  (:load-comments-from-server
   (lambda ()
     (ajax :url (@ this props url)
	   :data-type "json"
	   :cache false
	   :success (bind-lambda (data) (set-state :data data))
	   :error (bind-lambda (xhr status err)
		    (chain console
			   (error (@ this props url)
				  status
				  (chain err (to-string))))))))
  (:handle-comment-submit
   (lambda (comment)
     (let* ((comments (@ this state data))
	    (new-comments (append comments (array comment))))
       (set-state :data new-comments)
       (ajax :url (@ this props url)
	     :type "POST"
	     :data comment
	     :success (bind-lambda (data) (set-state :data data))
	     :error (bind-lambda (xhr status err)
		      (chain console
			     (error (@ this props url)
				    status
				    (chain err (to-string)))))))))
  (:get-initial-state
   (lambda ()
     (create :data (array))))
  (:component-did-mount
   (lambda ()
     (chain this (load-comments-from-server))
     (set-interval (@ this load-comments-from-server)
		   (@ this props poll-interval))))
  (:render
   (lambda ()
     (psx (:div :class "commentBox"
		(:h1 "Comments")
		(:comment-list :data (@ this state data))
		(:comment-form :on-comment-submit (@ this handle-comment-submit)))))))
  
					
  
							    

  
