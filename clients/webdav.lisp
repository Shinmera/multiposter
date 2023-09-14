(in-package #:org.shirakumo.multiposter)

(define-client webdav (client)
  ((username :initarg :username :initform NIL :accessor username)
   (password :initarg :password :initform NIL :accessor password)
   (authorization :initarg :authorization :initform NIL :accessor authorization)
   (base-url :initarg :base-url :initform NIL :accessor base-url)))

(defmethod initargs append ((client webdav))
  (list :username (username client)
        :password (password client)
        :authorization (authorization client)
        :base-url (base-url client)))

(defmethod webdav-file-url ((client client) (file string))
  (format NIL "~a/~a" (base-url client) file))

(defmethod webdav-file-url ((client client) (file pathname))
  (webdav-file-url client (file-namestring file)))

(defun webdav-request (client method file)
  (multiple-value-bind (stream code)
      (drakma:http-request (webdav-file-url client file) 
                           :method method :want-stream T
                           :basic-authorization (when (username client)
                                                  (list (username client) (password client)))
                           :additional-headers (when (authorization client)
                                                 (list (cons "Authorization" (authorization client)))))
    (unwind-protect
         (let ((content (ignore-errors (alexandria:read-stream-content-into-string stream))))
           (if (<= 400 code)
               (error "WebDAV request failed with code ~d:~%  ~a" code content)
               content))
      (close stream))))

(defclass webdav-result (result)
  ((files :initarg :files :accessor files)))

(defmethod undo ((result webdav-result))
  (dolist (file (files result) result)
    (webdav-request (client result) :delete file)))

(defmethod post ((post post) (client webdav) &key verbose)
  (declare (ignore verbose))
  (error "Can't post ~a to WebDAV." (type-of post)))

(defmethod post ((post image-post) (client webdav) &key verbose)
  (dolist (file (files post))
    (webdav-request client :put file)))

(defmethod post ((post video-post) (client webdav) &key verbose)
  (webdav-request client :put (file post)))

(defmethod ready-p ((client webdav))
  (ignore-errors (webdav-request client :get "/")))

(defmethod setup ((client webdav) &rest args)
  (cond ((and (null args) (null (base-url client)))
         (setf (base-url client) (query "Please enter the base URL"))
         (loop
          (let ((method (query "Choose the authentication method: none, basic, header" :default "basic")))
            (cond ((string-equal "none" method))
                  ((string-equal "basic" method)
                   (setf (username client) (query "Enter the username"))
                   (setf (password client) (query "Enter the password")))
                  ((string-equal "header" method)
                   (setf (authorization client) (query "Enter the full Authorization header"))))
            (if (ready-p client)
                (return client)
                (format *query-io* "~&Login failed. Please try again.~%")))))
        (T
         (apply #'reinitialize-instance client args))))
