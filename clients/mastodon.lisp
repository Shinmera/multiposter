(in-package #:org.shirakumo.multiposter)

(define-client mastodon (file tooter:client)
  ())

(defmethod initargs append ((client mastodon))
  (list :base (tooter:base client)
        :key (tooter:key client)
        :secret (tooter:secret client)
        :access-token (tooter:access-token client)
        :name (tooter:name client)))

(defclass mastodon-result (result)
  ((id :initarg :id :initform NIL :accessor id)))

(defmethod undo ((result mastodon-result))
  (tooter:delete-status (client result) (id result)))

(defmethod failed-p ((result mastodon-result))
  (null (id result)))

(defmethod post :around ((post post) (client mastodon) &rest args)
  (apply #'call-next-method (make-like post :tags (filter-tags (tags post))) client args))

(defmethod post ((post post) (client mastodon) &key verbose)
  (when verbose (verbose "Posting message to ~a" (tooter:base client)))
  (let ((status (tooter:make-status client (compose-post post :char-limit 500)
                                    :sensitive (not (null (content-warning post)))
                                    :spoiler-text (content-warning post))))
    (make-instance 'mastodon-result :client client :post post :id (tooter:id status) :url (tooter:url status))))

(defmethod post ((post link-post) (client mastodon) &rest args)
  (apply #'call-next-method
         (make-like post :title (url post) :header (merge-paragraphs (title post) (header post)))
         client args))

(defmethod post ((post image-post) (client mastodon) &key verbose)
  (when verbose (verbose "Posting message to ~a" (tooter:base client)))
  (let* ((media (loop repeat 4
                      for file in (files post)
                      for desc in (file-descriptions post)
                      collect (tooter:make-media client file :description desc)))
         (status (tooter:make-status client (compose-post post :char-limit 500)
                                     :sensitive (not (null (content-warning post)))
                                     :spoiler-text (content-warning post)
                                     :media media)))
    (make-instance 'mastodon-result :client client :post post :id (tooter:id status) :url (tooter:url status))))

(defmethod post ((post video-post) (client mastodon) &key verbose)
  (when verbose (verbose "Posting message to ~a" (tooter:base client)))
  (let ((status (tooter:make-status client (compose-post post :char-limit 500)
                                    :sensitive (not (null (content-warning post)))
                                    :spoiler-text (content-warning post)
                                    :media (file post))))
    (make-instance 'mastodon-result :client client :post post :id (tooter:id status) :url (tooter:url status))))

(defmethod ready-p ((client mastodon))
  (ignore-errors (tooter:verify-credentials client)))

(defmethod setup ((client mastodon) &rest args)
  (cond ((and (null args) (null (tooter:access-token client)))
         (setf (tooter:base client) (query "Enter the Mastodon server name" :default "mastodon.social"))
         (setf (tooter:name client) (query "Enter the client name" :default "Multiposter"))
         (let ((url (nth-value 1 (tooter:authorize client))))
           (tooter:authorize client (query (format NIL "Enter the PIN from ~a" url)))))
        (T
         (apply #'reinitialize-instance client args)))
  (tooter:verify-credentials client))
