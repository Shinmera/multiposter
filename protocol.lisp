#|
 This file is a part of Multiposter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.multiposter)

(defvar *image-types* '("png" "jpg" "jpeg" "gif" "bmp" "svg"))
(defvar *video-types* '("gifv" "apng" "mp4" "webm" "mov" "mkv"))

(defclass client ()
  ())

(defun client-types ()
  (remove (find-class 'multiposter)
          (sb-mop:class-direct-subclasses
           (find-class 'client))))

(defgeneric login (client &key &allow-other-keys))
(defgeneric post (client thing &key title description tags link))
(defgeneric post-text (client text &key tags link))
(defgeneric post-link (client url &key title description tags))
(defgeneric post-image (client path &key title description tags link))
(defgeneric post-video (client path &key title description tags link))

(defmethod post (client (path pathname) &key title description tags link)
  (cond ((find (pathname-type path) *image-types* :test #'string-equal)
         (post-image client path :link link :tags tags :title title :description description))
        ((find (pathname-type path) *video-types* :test #'string-equal)
         (post-video client path :link link :tags tags :title title :description description))
        (T
         (error "Unknown file type: ~s" (pathname-type path)))))

(defmethod post (client (thing string) &key title description tags link)
  (let ((links (extract-links thing)))
    (cond ((and (null (cdr links))
                (eql :link (caar links)))
           (post-link client thing :tags tags :title title :description description))
          (T
           (post-text client thing :tags tags :link link :title title)))))

(defclass multiposter (client)
  ((clients :initarg :clients :accessor clients)
   (primary :initarg :primary :accessor primary))
  (:default-initargs :clients () :primary NIL))

(defmethod make-load-form ((multiposter multiposter) &optional env)
  `(let ((clients (list ,@(loop for client in (clients multiposter)
                                collect (make-load-form client env)))))
     (make-instance 'multiposter
                    :clients clients
                    :primary ,(when (primary multiposter)
                                `(elt clients ,(position (primary multiposter) (clients multiposter)))))))

(defmethod login ((multiposter multiposter) &rest args)
  (dolist (client (clients multiposter) multiposter)
    (with-simple-restart (continue "Continue with the next client.")
      (apply #'login client args))))

(defun delegate-to (multiposter function primary args)
  (cond ((primary multiposter)
         (let ((link (apply function (primary multiposter) primary args)))
           (list* link
                  (loop for client in (clients multiposter)
                        unless (eq client (primary multiposter))
                        collect (apply function client primary :link link args)))))
        (T
         (loop for client in (clients multiposter)
               collect (apply function client primary args)))))

(defmethod post-text ((multiposter multiposter) text &rest args)
  (delegate-to multiposter #'post-text text args))

(defmethod post-link ((multiposter multiposter) url &rest args)
  (loop for client in (clients multiposter)
        collect (apply #'post-link client url args)))

(defmethod post-image ((multiposter multiposter) path &rest args)
  (delegate-to multiposter #'post-image (if (listp path) (mapcar #'pathname path) path) args))

(defmethod post-video ((multiposter multiposter) path &rest args)
  (delegate-to multiposter #'post-video (pathname path) args))
