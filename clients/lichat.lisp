(in-package #:org.shirakumo.multiposter)

(defmacro with-lichat-response ((update client) send &body body)
  (let ((clientg (gensym "CLIENT")))
    `(loop with ,clientg = ,client
           do (handler-case
                  (progn
                    (unless (lichat-tcp-client:connection-open-p ,clientg)
                      (setup ,clientg))
                    (lichat-tcp-client:with-response (,update ,clientg) ,send
                      (etypecase ,update
                        (lichat-protocol:failure
                         (error "Failed: ~a" (lichat-protocol:text ,update)))
                        (lichat-protocol:update
                         (return (progn ,@body))))))
                ((or end-of-file usocket:socket-error #+sbcl sb-int:broken-pipe) ()
                  (setup ,clientg))))))

(define-client lichat (client lichat-tcp-client:client)
  ((channel :initarg :channel :accessor channel))
  (:default-initargs :hostname NIL :thread :local))

(defmethod initargs append ((client lichat))
  (list :hostname (lichat-tcp-client:hostname client)
        :port (lichat-tcp-client:port client)
        :username (lichat-tcp-client:username client)
        :password (lichat-tcp-client:password client)
        :channel (channel client)))

(defclass lichat-result (result)
  ((message-ids :initform () :accessor message-ids)))

(defmethod undo ((result lichat-result))
  (dolist (id (message-ids result))
    (with-lichat-response (message (client result))
      (lichat-tcp-client:s (client result) 'edit :channel (channel (client result)) :id id :text ""))))

(defmethod failed-p ((result lichat-result))
  (null (message-ids result)))

(defmethod post ((post post) (client lichat) &key verbose)
  (let ((result (make-instance 'lichat-result :client client :post post :url (lichat-tcp-client:url client)))
        (message (compose-post post :exclude-tags T)))
    (when verbose (verbose "Posting message to ~a" (channel client)))
    (with-lichat-response (message client)
        (lichat-tcp-client:s client 'message :channel (channel client) :text message)
      (push (lichat-protocol:id message) (message-ids result)))
    result))

(defmethod post ((post image-post) (client lichat) &key verbose)
  (let ((result (call-next-method)))
    (dolist (file (files post) result)
      (when verbose (verbose "Sending ~a" file))
      (with-lichat-response (message client)
          (lichat-tcp-client:s-data client (channel client) file)
        (push (lichat-protocol:id message) (message-ids result))))))

(defmethod post ((post video-post) (client lichat) &key verbose)
  (let ((result (call-next-method))
        (file (file post)))
    (when verbose (verbose "Sending ~a" file))
    (with-lichat-response (message client)
        (lichat-tcp-client:s-data client (channel client) file)
      (push (lichat-protocol:id message) (message-ids result)))))

(defmethod post ((post link-post) (client lichat) &rest args)
  (apply #'call-next-method
         (make-like post :title (url post) :header (merge-paragraphs (title post) (header post)))
         client args))

(defmethod ready-p ((client lichat))
  (lichat-tcp-client:connection-open-p client))

(defmethod setup ((client lichat) &rest args)
  (cond ((and (null args) (null (lichat-tcp-client:hostname client)))
         (format *query-io* "~&Lichat login required.~%")
         (setf (lichat-tcp-client:hostname client) (query "Enter the lichat server's hostname" :default "chat.tymoon.eu"))
         (setf (lichat-tcp-client:port client) (query "Enter the lichat server's port" :default lichat-tcp-client:*default-port* :coerce #'parse-integer))
         (setf (lichat-tcp-client:username client) (query "Enter the username"))
         (setf (lichat-tcp-client:password client) (query "Enter the password, if any" :nullable T))
         (setf (channel client) (query "Enter the channel to post in")))
        (T
         (apply #'reinitialize-instance client args)))
  (lichat-tcp-client:close-connection client)
  (lichat-tcp-client:open-connection client)
  (lichat-tcp-client:s client 'join :channel (channel client)))

(org.shirakumo.verbose:remove-global-controller)
