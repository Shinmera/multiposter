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

(defclass client (multiposter:client)
  ((api-key :initarg :api-key :accessor api-key)
   (api-secret :initarg :api-secret :accessor api-secret)
   (access-token :initarg :access-token :accessor access-token)
   (access-secret :initarg :access-secret :accessor access-secret)
   (blog :initarg :blog :accessor blog))
  (:default-initargs
   :api-key NIL
   :api-secret NIL
   :access-token NIL
   :access-secret NIL
   :blog NIL))

(defmethod make-load-form ((client client) &optional env)
  (declare (ignore env))
  `(make-instance 'client
                  :api-key ,(api-key client)
                  :api-secret ,(api-secret client)
                  :access-token ,(access-token client)
                  :access-secret ,(access-secret client)
                  :blog ,(blog client)))

(defmacro with-client ((client) &body body)
  `(let ((south:*oauth-api-key* (api-key ,client))
         (south:*oauth-api-secret* (api-secret ,client))
         (south:*oauth-access-token* (access-token ,client))
         (south:*oauth-access-secret* (access-secret ,client)))
     ,@body))

(defmethod multiposter:login ((client client) &key api-key api-secret blog access-token access-secret)
  (let ((api-key
          (or api-key (multiposter:prompt "Please enter the Tumblr API key")))
        (api-secret
          (or api-secret (multiposter:prompt "Please enter the Tumblr API secret")))
        (blog
          (or blog (multiposter:prompt "Please enter the blog to post to" :default :detect))))
    (setf (api-key client) api-key)
    (setf (api-secret client) api-secret)
    (unless (eql blog :detect)
      (setf (blog client) blog))
    (unless (and access-token access-secret)
      (let* (south:*oauth-access-token* south:*oauth-access-secret*
             south:*oauth-api-key* south:*oauth-api-secret*
             (url (south:initiate-authentication :api-key api-key :api-secret api-secret)))
        (multiposter:prompt (format NIL "Please visit~%  ~a~%and hit return when complete." url)
                            :converter #'identity)
        (setf (access-token client) south:*oauth-access-token*)
        (setf (access-secret client) south:*oauth-access-secret*)
        (when (eql blog :detect)
          (setf (blog client) (humbler:name (humbler:myself))))))
    client))

(defmethod multiposter:post-text ((client client) text &key tags link)
  (with-client (client)
    (humbler:blog/post-text (blog client) (format NIL "~a~@[~&~%~a~]" text link)
                            :tags tags :format :markdown :tweet :off)))

(defmethod multiposter:post-link ((client client) url &key description tags)
  (with-client (client)
    (humbler:blog/post-link (blog client) url
                            :description description
                            :tags tags :format :markdown :tweet :off)))

(defmethod multiposter:post-image ((client client) path &key description tags link)
  (with-client (client)
    (humbler:blog/post-photo (blog client) path
                             :caption (format NIL "~a~@[~&~%~a~]" text link)
                             :link link :tags tags :format :markdown :tweet :off)))

(defmethod multiposter:post-video ((client client) path &key description tags link)
  (with-client (client)
    (humbler:blog/post-video (blog client) path
                             :caption (format NIL "~a~@[~&~%~a~]" text link)
                             :tags tags :format :markdown :tweet :off)))
