(defpackage #:multiposter-twitter
  (:nicknames #:org.shirakumo.multiposter.twitter)
  (:use #:cl)
  (:export
   #:client))
(in-package #:org.shirakumo.multiposter.twitter)

(defvar *text-limit* 280)
(defvar *link-length*)

(defclass client (multiposter:client)
  ((api-key :initarg :api-key :accessor api-key)
   (api-secret :initarg :api-secret :accessor api-secret)
   (access-token :initarg :access-token :accessor access-token)
   (access-secret :initarg :access-secret :accessor access-secret))
  (:default-initargs
   :api-key NIL
   :api-secret NIL
   :access-token NIL
   :access-secret NIL))

(defmethod make-load-form ((client client) &optional env)
  (declare (ignore env))
  `(make-instance 'client
                  :api-key ,(api-key client)
                  :api-secret ,(api-secret client)
                  :access-token ,(access-token client)
                  :access-secret ,(access-secret client)))

(defmacro with-client ((client) &body body)
  `(let ((chirp:*oauth-api-key* (api-key ,client))
         (chirp:*oauth-api-secret* (api-secret ,client))
         (chirp:*oauth-access-token* (access-token ,client))
         (chirp:*oauth-access-secret* (access-secret ,client)))
     ,@body))

(defmethod multiposter:login ((client client) &key api-key api-secret access-token access-secret)
  (let ((api-key
          (or api-key (multiposter:prompt "Please enter the Twitter API key"
                                           :default "D1pMCK17gI10bQ6orBPS0w")))
        (api-secret
          (or api-secret (multiposter:prompt "Please enter the Twitter API secret"
                                             :default "BfkvKNRRMoBPkEtDYAAOPW4s2G9U8Z7u3KAf0dBUA")))
        (access-token
          (or access-token (multiposter:prompt "Please enter the Twitter access token" :default NIL)))
        (access-secret
          (or access-secret (multiposter:prompt "Please enter the Twitter access secret" :default NIL))))
    (setf (api-key client) api-key)
    (setf (api-secret client) api-secret)
    (unless (and access-token access-secret)
      (let* (chirp:*oauth-access-token* chirp:*oauth-access-secret*
             chirp:*oauth-api-key* chirp:*oauth-api-secret*
             (url (chirp:initiate-authentication :api-key api-key :api-secret api-secret))
             (pin (multiposter:prompt (format NIL "Please visit~%  ~a~%and enter the code here" url))))
        (multiple-value-bind (access-token access-secret) (chirp:complete-authentication pin)
          (setf (access-token client) access-token)
          (setf (access-secret client) access-secret))))
    client))

(defun shorten-text (text &key (limit *text-limit*) (link-length *link-length*))
  (multiposter:limit-text-with-links text limit link-length))

(defun prep-tag (tag)
  (with-output-to-string (out)
    (loop for char across tag
          do (unless (find char "!$%^&*+.,[](){} ")
               (write-char char out)))))

(defun prep-text (text tags link &key (limit *text-limit*) (link-length *link-length*))
  (let ((text (format NIL "~a~{ #~a~}" text (mapcar #'prep-tag tags))))
    (if link
        (format NIL "~a ~a"
                (shorten-text text :limit (- limit 1 link-length))
                link)
        (shorten-text text :limit limit))))

(defun status-url (status)
  (format NIL "https://twitter.com/~a/status/~a"
          (chirp:screen-name (chirp:user status)) (chirp:id status)))

(defmethod multiposter:post-text ((client client) text &key title tags link)
  (with-client (client)
    (let ((*link-length* (or (chirp:short-url-length-https (chirp:help/configuration)) 20)))
      (status-url (chirp:statuses/update (prep-text (format NIL "~@[~a~%~]~a" title text) tags link))))))

(defmethod multiposter:post-link ((client client) url &key title description tags)
  (with-client (client)
    (let ((*link-length* (or (chirp:short-url-length-https (chirp:help/configuration)) 20))
          (text (shorten-text (format NIL "~a~@[~a~%~]~@[~%~a~]~{ #~a~}" url title description tags))))
      (status-url (chirp:statuses/update text)))))

(defmethod multiposter:post-image ((client client) path &key title description tags link)
  (with-client (client)
    (let ((*link-length* (or (chirp:short-url-length-https (chirp:help/configuration)) 20))
          (limit (- *text-limit* 1)))
      (status-url (chirp:statuses/update (prep-text (format NIL "~@[~a~%~]~@[~a~]" title description) tags link :limit limit) :media path)))))

(defmethod multiposter:post-video ((client client) path &key title description tags link)
  (with-client (client)
    (let ((*link-length* (or (chirp:short-url-length-https (chirp:help/configuration)) 20))
          (limit (- *text-limit* 1)))
      (status-url (chirp:statuses/update (prep-text (format NIL "~@[~a~%~]~@[~a~]" title description) tags link :limit limit) :media path)))))
