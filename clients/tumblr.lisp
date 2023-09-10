(in-package #:org.shirakumo.multiposter)

(define-client tumblr (file humbler:client)
  ((blog :initarg :blog :initform NIL :accessor blog)))

(defmethod initargs append ((client tumblr))
  (list :key (north:key client)
        :secret (north:secret client)
        :token (north:token client)
        :token-secret (north:token-secret client)
        :blog (blog client)))

(defclass tumblr-result (result)
  ((post-id :initarg :post-id :initform NIL :accessor post-id)))

(defmethod failed-p ((result tumblr-result))
  (null (post-id result)))

(defmethod undo ((result tumblr-result))
  (let ((humbler:*client* (client result))
        (humbler:*user* NIL))
    (humbler:blog/post/delete (blog (client result)) (post-id result))))

(defmethod post :around ((post post) (client tumblr) &key verbose)
  (let ((humbler:*client* client)
        (humbler:*user* NIL))
    (when verbose (verbose "Posting image to Tumblr"))
    (let ((post-id (call-next-method)))
      (make-instance 'tumblr-result :client humbler:*client* :post post :post-id post-id :url
                     (format NIL "https://~a.tumblr.com/post/~a" (blog humbler:*client*) post-id)))))

(defmethod post ((post image-post) (client tumblr) &key verbose)
  (declare (ignore verbose))
  (humbler:blog/post-photo (blog client) (files post) :tags (tags post) :caption (compose-post post :exclude-tags T)))

(defmethod post ((post video-post) (client tumblr) &key verbose)
  (declare (ignore verbose))
  (humbler:blog/post-video (blog client) (file post) :tags (tags post) :caption (compose-post post :exclude-tags T :title T)))

(defmethod post ((post link-post) (client tumblr) &key verbose)
  (declare (ignore verbose))
  (humbler:blog/post-link (blog client) (url post) :title (title post) :tags (tags post) :description (compose-post post :exclude-tags T :exclude-title T)))

(defmethod post ((post text-post) (client tumblr) &key verbose)
  (declare (ignore verbose))
  (humbler:blog/post-text (blog client) (compose-post post :exclude-tags T :exclude-title T) :title (title post) :tags (tags post)))

(defmethod ready-p ((client tumblr))
  (not (null (north:token client))))

(defmethod setup ((client tumblr) &rest args)
  (cond ((null args)
         (let ((humbler:*client* client))
           (setf (blog client) (query "Enter the blog name" :nullable T))
           (setf (north:key client) (query "Enter the oAuth app key"))
           (setf (north:secret client) (query "Enter the oAuth app secret"))
           (format *query-io* "~&> Please visit ~a~%" (humbler:login))
           (unless (blog client) (setf (blog client) (humbler:name (humbler:myself))))))
        (T
         (apply #'reinitialize-instance client args))))
