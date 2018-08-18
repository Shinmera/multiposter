#|
 This file is a part of Multiposter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:multiposter-tumblr
  (:nicknames #:org.shirakumo.multiposter.tumblr)
  (:use #:cl)
  (:export
   #:client))
(in-package #:org.shirakumo.multiposter.tumblr)

(defclass client (multiposter:client humbler:client)
  ((blog :initarg :blog :accessor blog))
  (:default-initargs
   :blog NIL))

(defmethod make-load-form ((client client) &optional env)
  (declare (ignore env))
  `(make-instance 'client
                  :key ,(north:key client)
                  :secret ,(north:secret client)
                  :access-token ,(north:token client)
                  :access-secret ,(north:token-secret client)
                  :blog ,(blog client)))

(defmacro with-client ((client) &body body)
  `(let ((humbler:*client* ,client)
         (humbler:*user* NIL))
     ,@body))

(defmethod multiposter:login ((client client) &key api-key api-secret blog access-token access-secret)
  (let ((api-key
          (or api-key (multiposter:prompt "Please enter the Tumblr API key")))
        (api-secret
          (or api-secret (multiposter:prompt "Please enter the Tumblr API secret")))
        (access-token
          (or access-token (multiposter:prompt "Please enter the Tumblr access token" :default NIL)))
        (access-secret
          (or access-secret (multiposter:prompt "Please enter the Tumblr access secret" :default NIL)))
        (blog
          (or blog (multiposter:prompt "Please enter the blog to post to" :default :detect))))
    (setf (north:key client) api-key)
    (setf (north:secret client) api-secret)
    (setf (north:token client) access-token)
    (setf (north:token-secret client) access-secret)
    (unless (and access-token access-secret)
      (let ((url (humbler:login client)))
        (multiposter:prompt (format NIL "Please visit~%  ~a~%and hit return when complete." url)
                            :converter #'identity)))
    (setf (blog client) (if (eql blog :detect)
                            (with-client (client)
                              (humbler:name (humbler:myself)))
                            blog))
    client))

(defun post-url (client post-id)
  (format NIL "https://~a.tumblr.com/post/~a" (blog client) post-id))

(defmethod multiposter:post-text ((client client) text &key title tags link)
  (with-client (client)
    (post-url client
              (humbler:blog/post-text (blog client) (format NIL "~a~@[~&~%~a~]" text link)
                                      :title title :tags tags :format :markdown :tweet :off))))

(defmethod multiposter:post-link ((client client) url &key title description tags)
  (with-client (client)
    (post-url client
              (humbler:blog/post-link (blog client) url
                                      :description description :title title
                                      :tags tags :format :markdown :tweet :off))))

(defmethod multiposter:post-image ((client client) path &key title description tags link)
  (with-client (client)
    (post-url client
              (humbler:blog/post-photo (blog client) path
                                       :caption (format NIL "~@[~a~%~]~@[~a~]~@[~&~%~a~]" title description link)
                                       :link link :tags tags :format :markdown :tweet :off))))

(defmethod multiposter:post-video ((client client) path &key title description tags link)
  (with-client (client)
    (post-url client
              (humbler:blog/post-video (blog client) path
                                       :caption (format NIL "~@[~a~%~]~@[~a~]~@[~&~%~a~]" title description link)
                                       :tags tags :format :markdown :tweet :off))))
