#|
 This file is a part of Multiposter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:multiposter-mastodon
  (:nicknames #:org.shirakumo.multiposter.mastodon)
  (:use #:cl)
  (:export
   #:client))
(in-package #:org.shirakumo.multiposter.mastodon)

(defvar *text-limit* 500) ;; Hard-coded for now.
(defvar *link-length* 24) 

(defclass client (multiposter:client tooter:client)
  ()
  (:default-initargs
   :base NIL
   :name "Multiposter"
   :website "https://github.com/Shinmera/multiposter"))

(defmethod multiposter:login ((client client) &key base key secret access-token)
  (let ((base (or base (multiposter:prompt "Please enter the Mastodon instance's URL"))))
    (setf (tooter:base client) base)
    (setf (tooter:key client) key)
    (setf (tooter:secret client) secret)
    (setf (tooter:access-token client) access-token)
    (unless access-token
      (multiple-value-bind (maybe-client maybe-url) (tooter:authorize client)
        (or maybe-client
            (let ((prompt (format NIL "Please visit~%  ~a~%and enter the code here" maybe-url)))
              (tooter:authorize client (multiposter:prompt prompt))))))
    client))

(defun shorten-text (text &key (limit *text-limit*) (link-length *link-length*))
  (multiposter:limit-text-with-links text limit link-length))

(defun prep-tag (tag)
  (with-output-to-string (out)
    (loop for char across tag
          do (unless (find char "!$%^&*+.,[](){} ")
               (write-char char out)))))

(defun prep-text (text tags link)
  (let ((text (format NIL "~a~{ #~a~}" text (mapcar #'prep-tag tags))))
    (if link
        (format NIL "~a ~a"
                (shorten-text text :limit (- *text-limit* 1 *link-length*))
                link)
        (shorten-text text))))

(defmethod multiposter:post-text ((client client) text &key title tags link)
  (tooter:url (tooter:make-status client (prep-text (format NIL "~@[~a~%~]~a" title text) tags link))))

(defmethod multiposter:post-link ((client client) url &key title description tags)
  (let ((text (shorten-text (format NIL "~a~@[~a~%~]~@[~%~a~]~{ #~a~}" url title description tags))))
    (tooter:url (tooter:make-status client text))))

(defmethod multiposter:post-image ((client client) path &key title description tags link)
  (tooter:url (tooter:make-status client (prep-text (format NIL "~@[~a~%~]" title description) tags link) :media path)))

(defmethod multiposter:post-video ((client client) path &key title description tags link)
  (tooter:url (tooter:make-status client (prep-text (format NIL "~@[~a~%~]" title description) tags link) :media path)))
