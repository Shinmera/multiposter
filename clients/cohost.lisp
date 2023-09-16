(in-package #:org.shirakumo.multiposter)

(define-client cohost (cohost:client)
  ((blog :initarg :blog :initform NIL :accessor blog)
   (page :initarg :page :initform NIL :accessor page)))

(defmethod initargs append ((client cohost))
  (list :token (cohost:token client)
        :page (page client)))

(defclass cohost-result (result)
  ((post-object :initarg :post :initform NIL :accessor post-object)))

(defmethod failed-p ((result cohost-result))
  (null (post-object result)))

(defmethod undo ((result cohost-result))
  (cohost:destroy (post-object result)))

(defmethod post :around ((post post) (client cohost) &rest args)
  (apply #'call-next-method (make-like post :tags (filter-tags (tags post) #'non-comma-p)) client args))

(defmethod post ((post image-post) (client cohost) &key verbose)
  (when verbose (verbose "Posting to cohost page ~a" (page client)))
  (let ((post (cohost:make-post (or (find (page client) (cohost:pages client) :key #'cohost:handle :test #'string-equal)
                                    (error "Unknown cohost page: ~a" (page client)))
                                :title (title post)
                                :tags (tags post) 
                                :content-warnings (enlist (content-warning post))
                                :content (append (loop for file in (files post)
                                                       for desc in (file-descriptions post)
                                                       collect (make-instance 'cohost:attachment :file file :alt-text desc))
                                                 (list (compose-post post :exclude-title T :exclude-tags T))))))
    (make-instance 'cohost-result :post post :url (cohost:url post))))

(defmethod post ((post text-post) (client cohost) &key verbose)
  (when verbose (verbose "Posting to cohost page ~a" (page client)))
  (let ((post (cohost:make-post (or (find (page client) (cohost:pages client) :key #'cohost:handle :test #'string-equal)
                                    (error "Unknown cohost page: ~a" (page client)))
                                :title (title post)
                                :tags (tags post) 
                                :content-warnings (enlist (content-warning post))
                                :content (compose-post post :exclude-title T :exclude-tags T))))
    (make-instance 'cohost-result :post post :url (cohost:url post))))

(defmethod ready-p ((client cohost))
  (not (null (cohost:token client))))

(defmethod setup ((client cohost) &rest args)
  (cond ((and (null args) (not (ready-p client)))
         (loop (handler-case
                   (progn (cohost:login client (query "Enter the email address") (query "Enter the password"))
                          (return))
                 (error ()
                   (format *query-io* "~&Failed to log in. Try again."))))
         (setf (page client) (query "Enter the handle of the page to post on"
                                    :default (cohost:handle (cohost:default-page client)))))
        (T
         (apply #'reinitialize-instance client args))))
