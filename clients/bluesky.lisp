(in-package #:org.shirakumo.multiposter)

(defvar +timestring-format+
    (substitute :gmt-offset :gmt-offset-or-z local-time:+iso-8601-format+))

(defconstant +bluesky-char-limit+ 300)

(define-client bluesky (client)
  ((handle :initarg :handle :accessor handle)
   (password :initarg :password :accessor app-password)
   (did :accessor did :initform nil)
   (access-jwt :accessor access-jwt :initform nil)
   (refresh-jwt :accessor refresh-jwt :initform nil))
  (:default-initargs
   :name "bluesky"))

(defmethod initargs append ((b bluesky))
  (with-accessors ((handle handle) (password app-password)) b
    (list :handle handle :password password)))

(defmethod ready-p ((client bluesky))
  (and (handle client) (app-password client)))

(defclass bluesky/result (result)
  ((rkey :initarg :rkey :accessor rkey)))

(defun bluesky/login (client)
  (let ((response (yason:parse
                   (flexi-streams:octets-to-string
                    (drakma:http-request
                     "https://bsky.social/xrpc/com.atproto.server.createSession"
                     :decode-content t
                     :method :post
                     :accept "application/json"
                     :content-type "application/json"
                     :form-data t
                     :external-format-in :utf-8
                     :external-format-out :utf-8
                     :content (with-output-to-string (s)
                                (yason:encode-alist
                                 `(("identifier" . ,(handle client))
                                   ("password" . ,(app-password client))) s)))
                    :external-format :utf-8))))
    (setf (access-jwt client) (gethash "accessJwt" response)
          (refresh-jwt client) (gethash "refreshJwt" response)
          (did client) (gethash "did" response))
    client))

(defun bluesky/refresh (client)
  (let ((response (yason:parse
                   (flexi-streams:octets-to-string
                    (drakma:http-request
                     "https://bsky.social/xrpc/com.atproto.server.refreshSession"
                     :method :post
                     :accept "application/json"
                     :external-format-in :utf-8 :external-format-out :utf-8
                     :additional-headers `(("Authorization" . ,(format nil "Bearer ~a" (refresh-jwt client)))))
                    :external-format :utf-8))))
    (setf (access-jwt client) (gethash "accessJwt" response)
          (refresh-jwt client) (gethash "refreshJwt" response)
          (did client) (gethash "did" response))
    client))

(defun upload-image-to-bluesky (client image)
  (alexandria:alist-hash-table
   `(("alt" . "")
     ("image" . ,(gethash
                  "blob"
                  (yason:parse
                   (flexi-streams:octets-to-string
                    (drakma:http-request
                     "https://bsky.social/xrpc/com.atproto.repo.uploadBlob"
                     :content-type (format nil "image/~(~a~)" (pathname-type image))
                     :accept "application/json"
                     :method :post
                     :additional-headers `(,(cons "Authorization" (format nil "Bearer ~a" (access-jwt client))))
                     :content image)
                    :external-format :utf-8)))))))

(defun bluesky/post (client text &optional image link)
  (let ((text-length (length (flexi-streams:string-to-octets text :external-format :utf-8)))
        (url-length (if link
                        (length (flexi-streams:string-to-octets link :external-format :utf-8))
                        0))
        (post (alexandria:alist-hash-table
               `(("$type" . "app.bsky.feed.post")
                 ("text" . ,(format nil "~a~@[ ~a~]" text link))
                 ("createdAt" . ,(local-time:format-timestring
                                  nil
                                  (local-time:now)
                                  :timezone local-time:+utc-zone+
                                  :format +timestring-format+)))))
        (token (format nil "Bearer ~a" (access-jwt client))))
    (when image
      (setf (gethash "embed" post)
            (alexandria:alist-hash-table
             `(("$type" . "app.bsky.embed.images")
               ("images" ,(upload-image-to-bluesky client image))))))
    (when link
      (setf (gethash "facets" post)
            (list (alexandria:alist-hash-table
                   `(("index" . ,(alexandria:alist-hash-table
                                  `(("byteStart" . ,(+ 1 text-length))
                                    ("byteEnd" . ,(+ 1 text-length url-length)))))
                     ("features" ,(alexandria:alist-hash-table
                                   `(("$type" . "app.bsky.richtext.facet#link")
                                     ("uri" . ,link)))))))))
    (yason:parse
     (flexi-streams:octets-to-string
      (drakma:http-request
       "https://bsky.social/xrpc/com.atproto.repo.createRecord"
       :method :post
       :content-type "application/json"
       :accept "application/json"
       :form-data t
       :external-format-in :utf-8
       :external-format-out :utf-8
       :additional-headers `(("Authorization" . ,token))
       :content (with-output-to-string (s)
                  (yason:encode
                   (alexandria:alist-hash-table
                    `(("repo" . ,(did client))
                      ("collection" . "app.bsky.feed.post")
                      ("record" . ,post)))
                   s)))
      :external-format :utf-8))))

(defun bluesky/delete (client post-id)
  (multiple-value-bind (response http-status)
      (drakma:http-request
       "https://bsky.social/xrpc/com.atproto.repo.deleteRecord"
       :method :post
       :content-type "application/json"
       :accept "application/json"
       :form-data t
       :external-format-in :utf-8
       :external-format-out :utf-8
       :additional-headers `(("Authorization" . ,(format nil "Bearer ~a" (access-jwt client))))
       :content (with-output-to-string (s)
                  (yason:encode
                   (alexandria:alist-hash-table
                    `(("repo" . ,(did client))
                      ("collection" . "app.bsky.feed.post")
                      ("rkey" . ,post-id)))
                   s)))
    (ecase (print http-status)
      (200 (yason:parse
            (flexi-streams:octets-to-string
             response
             :external-format :utf-8)))
      (400 (error "Error deleting post: ~a"
                  (alexandria:hash-table-alist
                   (yason:parse
                    (flexi-streams:octets-to-string
                     response :external-format :utf-8)))))
      (401 (error "Not authorised.")))))

(defmethod post :before (post (client bluesky) &key &allow-other-keys)
  (bluesky/login client))

(defmethod post :around (post (client bluesky) &key &allow-other-keys)
  (let ((p (call-next-method)))
    (make-instance 'bluesky/result
                   :rkey (gethash "cid" p)
                   :url (gethash "uri" p)
                   :post post
                   :client client)))

(defmethod post ((post text-post) (client bluesky)
                             &key &allow-other-keys)
  (bluesky/post client (compose-post
                        post
                        :exclude-tags t
                        :char-limit +bluesky-char-limit+)))

(defmethod post :before ((post image-post) (client bluesky)
                                     &key &allow-other-keys)
  (case (length (files post))
    (0 (error "No images found in ~s" post))
    (1)
    ((2 3 4) (warn "Only posting the first image is supported for now."))
    (t (cerror "Take only the first image."
               "Too many images in post ~s" post))))

(defmethod post ((post image-post) (client bluesky)
                             &key &allow-other-keys)
  (bluesky/post client (compose-post
                        post
                        :exclude-tags t
                        :char-limit +bluesky-char-limit+)
                (first (files post))))

(defmethod undo ((result bluesky/result))
  (with-accessors ((client client) (rkey rkey)) result
    (bluesky/login client)
    (bluesky/delete client rkey))
  result)
