#|
 This file is a part of Multiposter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:multiposter-studio
  (:nicknames #:org.shirakumo.multiposter.studio)
  (:use #:cl)
  (:export
   #:client))
(in-package #:org.shirakumo.multiposter.studio)

(defclass client (multiposter:client studio-client:client)
  ()
  (:default-initargs
   :key NIL
   :secret NIL))

(defmethod make-load-form ((client client) &optional env)
  (declare (ignore env))
  `(make-instance 'client
                  :key ,(north:key client)
                  :secret ,(north:secret client)
                  :access-token ,(north:token client)
                  :access-secret ,(north:token-secret client)
                  :api-base ,(studio-client:api-base client)))

(defmethod multiposter:login ((client client) &key base key secret token token-secret)
  (let ((base (or base (multiposter:prompt "Please enter the Studio instance's API URL"
                                           :default (studio-client:api-base client))))
        (key (or key (multiposter:prompt "Please enter the oAuth API key"
                                         :default "608596EE-8554-4F6A-A571-1267BE119E9E")))
        (secret (or secret (multiposter:prompt "Please enter the oAuth API secret"
                                               :default "670F164B-BF5C-4FDE-9EF0-AAC02A92C1ED")))
        (token (or token (multiposter:prompt "Please enter the oAuth access token" :default NIL)))
        (token-secret (or token-secret (multiposter:prompt "Please enter the oAuth access secret" :default NIL))))
    (setf (slot-value client 'studio-client:api-base) base)
    (setf (north:key client) key)
    (setf (north:secret client) secret)
    (setf (north:token client) token)
    (setf (north:token-secret client) token-secret)
    (unless (and token token-secret)
      (let ((prompt (format NIL "Please visit~%  ~a~%and enter the code here"
                            (north:initiate-authentication client))))
        (north:complete-authentication client (multiposter:prompt prompt))))
    client))

(defun prep-tag (tag)
  (with-output-to-string (out)
    (loop for char across tag
          do (unless (find char ",")
               (write-char char out)))))

(defmethod multiposter:post-text ((client client) text &rest args)
  (declare (ignore args)))

(defmethod multiposter:post-link ((client client) url &rest args)
  (declare (ignore args)))

(defun date ()
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    (format NIL "~d.~d.~d" yy mm dd)))

(defmethod multiposter:post-image ((client client) path &key title description tags link)
  (let* ((file (if (listp path) path (list path)))
         (upload (cond (title
                        (studio-client:make-upload client title file :tags tags :description (format NIL "~@[~a~]~@[~%~a~]" description link)))
                       ((and description (<= 64 (length description)))
                        (studio-client:make-upload client description file :tags tags :description link))
                       (T
                        (studio-client:make-upload client (date) file :tags tags :description (format NIL "~@[~a~]~@[~%~a~]" description link))))))
    (studio-client:url upload)))

(defmethod multiposter:post-video ((client client) path &rest args)
  (apply #'multiposter:post-image client path args))
