#|
 This file is a part of Multiposter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.multiposter)

(defvar *client* NIL)
(defvar *config-path*
  #+unix #p"~/.config/multiposter/client.lisp"
  #+win32 #p"~/AppData/Local/multiposter/client.lisp")

(defun restore (&optional (path *config-path*))
  (when (probe-file path)
    (let ((*package* #.*package*))
      (load path))
    *client*))

(defun offload (&optional (path *config-path*) (client *client*))
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output
                               :if-exists :supersede)
    (let ((*package* #.*package*))
      (write `(setf *client* ,(make-load-form client))
             :stream stream
             :case :downcase
             :readably T
             :pretty T))
    (fresh-line stream))
  client)

(defun setup (&key (clients (client-types)) (path *config-path*))
  (let* ((clients (loop for client in clients
                        collect (etypecase client
                                  (standard-class (make-instance client))
                                  (symbol (make-instance client))
                                  (client client))))
         (client (make-instance 'multiposter :clients clients :primary (first clients))))
    (login client)
    (offload path client)
    (setf *config-path* path)
    (setf *client* client)))

(defun text (text &key (client *client*) tags link)
  (post-text client text :tags tags :link link))

(defun link (url &key (client *client*) description tags)
  (post-link client url :description description :tags tags))

(defun image (path &key (client *client*) description tags link)
  (post-image client path :description description :tags tags :link link))

(defun video (path &key (client *client*) description tags link)
  (post-video client path :description description :tags tags :link link))

(defmethod post ((client (eql T)) thing &rest args)
  (apply #'post *client* thing args))
