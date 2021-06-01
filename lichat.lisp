#|
 This file is a part of Multiposter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:multiposter-lichat
  (:nicknames #:org.shirakumo.multiposter.lichat)
  (:use #:cl)
  (:local-nicknames
   (#:client #:org.shirakumo.lichat.tcp-client)
   (#:lichat #:org.shirakumo.lichat.protocol))
  (:export
   #:client))
(in-package #:org.shirakumo.multiposter.lichat)

(defclass client (multiposter:client client:client)
  ())

(defmethod make-load-form ((client client) &optional env)
  (declare (ignore env))
  `(make-instance 'client
                  :hostname ,(client:hostname client)
                  :port ,(client:port client)
                  :username ,(client:username client)
                  :password ,(client:password client)))

(defmethod multiposter:login ((client client) &key hostname port username password)
  (macrolet ((frob (field prompt &rest args)
               `(let ((,field (or ,field (multiposter:prompt ,prompt ,@args))))
                  (setf (,(find-symbol (string field) '#:client) client) ,field))))
    (frob hostname "Server hostname" :default "chat.tymoon.eu")
    (frob port "Server port" :default 1111 :converter #'parse-integer)
    (frob username "Username")
    (frob password "Password")
    (client:open-connection client)))

(defun ensure-connected (client)
  (unless (client:thread client)
    (client:open-connection client)))

(defmethod multiposter:post-text ((client client) text &key title tags link)
  (declare (ignore tags))
  (ensure-connected client)
  (client:s client 'message :text (format NIL "~@[~a~%~]~a~@[~%~a~]" title text link)))

(defmethod multiposter:post-link ((client client) link &key title tags description)
  (declare (ignore tags))
  (ensure-connected client)
  (client:s client 'message :text (format NIL "~@[~a~%~]~@[~a~%~]~a" title description link)))

(defun post-file (client path &key title description tags link)
  (declare (ignore tags))
  (ensure-connected client)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((payload (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence payload stream)
      (client:s client 'data
                :payload (cl-base64:usb8-array-to-base64-string payload)
                :content-type (trivial-mimes:mime path)
                :filename (file-namestring path))
      (when (or title description link)
        (client:s client 'message :text (format NIL "~@[~a~%~]~@[~a~%~]~@[~a~]" title description link))))))

(defmethod multiposter:post-image ((client client) path &rest args)
  (apply #'post-file client path args))

(defmethod multiposter:post-video ((client client) path &rest args)
  (apply #'post-file client path args))

