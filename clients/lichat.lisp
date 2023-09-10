(in-package #:org.shirakumo.multiposter)

(define-client lichat (file lichat-tcp-client:client)
  ((channel :initarg :channel :accessor channel)))

(defmethod initargs append ((client lichat))
  (list :hostname (lichat-tcp-client:hostname client)
        :port (lichat-tcp-client:port client)
        :username (lichat-tcp-client:username client)
        :password (lichat-tcp-client:password client)
        :channel (channel client)))

(defmethod post ((post post) (client lichat) &key verbose)
  (when verbose (verbose "Posting message to ~a" (channel client)))
  (lichat-tcp-client:s client 'message :channel (channel client) :message
                       (apply #'compose-post-text (merge-paragraphs (title post) (header post))
                              (description post) (footer post))))

(defmethod post :after ((post image-post) (client lichat) &key verbose)
  (dolist (file (files post))
    (when verbose (verbose "Sending ~a" file))
    (lichat-tcp-client:s client 'data :channel (channel client) :payload file :filename (file-namestring file))))

(defmethod post :after ((post video-post) (client lichat) &key verbose)
  (when verbose (verbose "Sending ~a" (file post)))
  (lichat-tcp-client:s client 'data :channel (channel client) :payload (file post) :filename (file-namestring (file post))))

(defmethod post ((post link-post) (client lichat) &rest args)
  (apply #'call-next-method
         (make-like post :title (url post) :header (merge-paragraphs (title post) (header post)))
         client args))

(defmethod ready-p ((client lichat))
  (lichat-tcp-client:connection-open-p client))

(defmethod setup ((client lichat) &rest args &key hostname port username password channel)
  (cond ((null args)
         (setf (hostname client) (query "Enter the lichat server's hostname" :default "chat.tymoon.eu"))
         (setf (port client) (query "Enter the lichat server's port" :default lichat-tcp-client:*default-port* :coerce #'parse-integer))
         (setf (username client) (query "Enter the username"))
         (setf (password client) (query "Enter the password, if any" :nullable T))
         (setf (channel client) (query "Enter the channel to post in")))
        (T
         (when hostname (setf (hostname client) hostname))
         (when port (setf (port client) port))
         (when username (setf (username client) username))
         (when password (setf (password client) password))
         (when channel (setf (channel client) channel))))
  (lichat-tcp-client:open-connection client))
