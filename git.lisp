#|
 This file is a part of Multiposter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:multiposter-git
  (:nicknames #:org.shirakumo.multiposter.git)
  (:use #:cl)
  (:export
   #:client))
(in-package #:org.shirakumo.multiposter.git)

(defclass client (multiposter:client)
  ())

(defmethod multiposter:login ((client client) &key)
  client)

(defmethod multiposter:post-text ((client client) text &rest args)
  (declare (ignore args)))

(defmethod multiposter:post-link ((client client) text &rest args)
  (declare (ignore args)))

(defun post-file (path &key description tags link)
  (let ((repository (legit:init path)))
    (legit:pull repository)
    (legit:add repository path)
    (legit:commit repository (format NIL "~a~@[~&~%Tags:~{ ~a~}~]~@[~%URL: ~a~]"
                                     description tags link))
    (legit:push repository)
    (format NIL "file://~a" path)))

(defmethod multiposter:post-image ((client client) path &rest args)
  (apply #'post-file path args))

(defmethod multiposter:post-video ((client client) path &rest args)
  (apply #'post-file path args))

