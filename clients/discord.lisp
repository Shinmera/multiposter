(in-package #:org.shirakumo.multiposter)

(define-client discord (client)
  ((url :initform nil :initarg :url :accessor url)
   (server-id :initform nil :initarg :server-id :accessor server-id)
   (username :accessor username :initarg :username)
   (avatar :accessor avatar :initarg :avatar)
   (colour :initform 0
           :initarg :colour
           :initarg :color
           :accessor colour
           :accessor color))
  (:default-initargs
   :name "discord"
   :username nil
   :avatar nil))

(defmethod initargs append ((d discord))
  (list :url (url d)
        :server-id (server-id d)
        :colour (colour d)
        :username (username d)
        :avatar (avatar d)))

(defmethod setup ((client discord) &rest args)
  (cond ((and (null args)
              (not (slot-boundp client 'url))
              (null (url client))
              (not (slot-boundp client 'server-id))
              (null (server-id client)))
         (format *query-io* "~&Webhook details required.~%")
         (setf
          (url client)
          (query "Enter the webhook URL")
          (server-id client)
          (query "Enter the channel ID the webhook posts to. Optional, but posts cannot be undone if not provided."
                 :nullable t)
          (username client)
          (query "Enter a custom username for this webhook" :nullable t)
          (colour client)
          (query "Enter a custom colour for this webhook as a hexadecimal number. The default is Discord's blue."
                 :check (lambda (x)
                          (<= 0 x #xFFFFFF))
                 :coerce (lambda (x)
                           (parse-integer x :radix 16))
                 :default #x738ADB)))
        (t
         (apply #'reinitialize-instance client args))))

(defmethod ready-p ((client discord))
  (and (server-id client) (url client)))

(defclass discord/result (result)
  ((channel-id :accessor channel-id :initarg :channel-id)
   (message-id :accessor message-id :initarg :message-id)))

(defmethod initialize-instance :after ((inst discord/result)
                                       &key client result-hash-table &allow-other-keys)
  (let ((channel-id (gethash "channel_id" result-hash-table))
        (message-id (gethash "id" result-hash-table)))
    (setf (channel-id inst) channel-id
          (message-id inst) message-id
          (url inst) (format nil "https://discord.com/channels/~a/~a/~a"
                             (server-id client) channel-id message-id))))

(defun post-to-webhook (url json-hash-table files)
  (multiple-value-bind (result https-status)
      (drakma:http-request
       (concatenate 'string url "?wait=true")
       :method :post
       :form-data t
       :external-format-out :utf-8
       :external-format-in :utf-8
       :accept "application/json"
       :parameters (cons
                    (cons "payload_json"
                          (with-output-to-string (s)
                            (yason:encode json-hash-table s)))
                    (loop for i in files
                          collect (cons (gensym "FILE") i))))
    (case https-status
      (200 result)
      (204 nil)
      (400 (error "~a" (flexi-streams:octets-to-string result :external-format :utf-8)))
      (t
       (warn "Unhandled HTTPS status: ~a" https-status)
       nil))))

(defun json-object-hash-table (specs inputs &optional hash-table-args)
  (loop with ht = (apply #'make-hash-table hash-table-args)
        with v = nil
        for (ht-key input-key . spec) in specs
        do (cond ((not (keywordp input-key))
                  (setf (gethash ht-key ht) input-key))
                 ((setf v (getf inputs input-key))
                  (setf (gethash ht-key ht) v)))
        finally (when (plusp (hash-table-count ht))
                  (return ht))))

(defun embed-hash-table (title description link colour tags time image-url)
  (json-object-hash-table
   '(("title" :title) ("description" :description) ("url" :link) ("color" :colour)
     ("fields" :fields) ("footer" :footer) ("timestamp" :time) ("image" :image))
   (list :title title :description description :link link :colour colour :time time
         :fields (list (json-object-hash-table
                        '(("name" "Tags") ("value" :value))
                        (list :value (format nil "~{`~a`~^ ~}" tags))))
         :footer (json-object-hash-table '(("text" "Multiposter")) ())
         :image (json-object-hash-table '(("url" :url)) (list :url image-url)))))

(defun message-hash (username avatar message-content embeds suppress-notifications)
  (json-object-hash-table
   `(("content" :content) ("avatar_url" :avatar) ("username" :username)
     ("flags" :suppress-notifications) ("embeds" :embeds))
   (list :content message-content :avatar avatar :username username
         :suppress-notifications (when suppress-notifications (expt 2 12))
         :embeds embeds)))

(defun parse-json-from-bytes (bytes)
  (yason:parse (flexi-streams:octets-to-string bytes :external-format :utf-8)))

(defun post-without-file (client message embed-json suppress-notifications)
  (let ((response (parse-json-from-bytes
                   (post-to-webhook (url client)
                                    (message-hash (username client) (avatar client) message
                                                  (alexandria:ensure-list embed-json)
                                                  suppress-notifications)
                                    ()))))
    (when (server-id client)
      (make-instance 'discord/result :result-hash-table response :client client))))

(defun post-with-file (client message paths embed-json suppress-notifications)
  (let ((response (parse-json-from-bytes
                   (post-to-webhook (url client)
                                    (message-hash (username client) (avatar client) message
                                                  (alexandria:ensure-list embed-json)
                                                  suppress-notifications)
                                    paths))))
    (when (server-id client)
      (make-instance 'discord/result :result-hash-table response :client client))))

(defmethod post ((post text-post) (client discord)
                 &key suppress-notifications &allow-other-keys)
  (post-without-file client (compose-post post) () suppress-notifications))


(defmethod post ((post image-post) (client discord)
                 &rest rest
                 &key suppress-notifications discord-link
                 &allow-other-keys)
  (remf rest :discord-link)
  (let ((files-list (files post))
        (first-file))
    (cond ((= 0 (length files-list))
           (error "Post must contain at least one image file."))
          ((= 1 (length files-list)))
          (t
           (warn "Too many images in ~a for ~a, will only take first one"
                 post client)))
    (setf first-file (first files-list))
    (post-with-file
     client
     "New image post."
     (list first-file)
     (embed-hash-table (title post)
                       (description post)
                       discord-link
                       (colour client)
                       (tags post)
                       (local-time:format-timestring
                        nil
                        time
                        :timezone local-time:+utc-zone+)
                       (format nil "attachment://~a.~a"
                               (pathname-name first-file)
                               (pathname-type first-file)))
     suppress-notifications)))

(defmethod undo ((result discord/result))
  (drakma:http-request
   (format nil "~a/messages/~a"
           (url (client result))
           (message-id result))
   :method :delete
   :external-format-out :utf-8
   :external-format-in :utf-8
   :accept "application/json")
  result)
