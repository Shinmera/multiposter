(in-package #:org.shirakumo.multiposter)

(define-client reader (client north:client)
  ((base-url :initarg :base-url :initform NIL :accessor base-url))
  (:default-initargs
   :request-token-uri NIL
   :authorize-uri NIL
   :access-token-uri NIL))

(defmethod shared-initialize :after ((client client) slots &key (base-url (base-url client)))
  (setf base-url (string-right-trim "/" base-url))
  (setf (slot-value client 'base-url) base-url)
  (unless (north:request-token-uri client)
    (setf (north:request-token-uri client) (format NIL "~a/api/oauth/request-token" base-url)))
  (unless (north:authorize-uri client)
    (setf (north:authorize-uri client) (format NIL "~a/api/oauth/authorize" base-url)))
  (unless (north:access-token-uri client)
    (setf (north:access-token-uri client) (format NIL "~a/api/oauth/access-token" base-url)))
  (unless (north:verify-uri client)
    (setf (north:verify-uri client) (format NIL "~a/api/oauth/verify" base-url))))

(defmethod initargs append ((client reader))
  (list :key (north:key client)
        :secret (north:secret client)
        :token (north:token client)
        :token-secret (north:token-secret client)
        :base-url (base-url client)))

(defclass reader-result (result)
  ((id :initarg :id :initform NIL :accessor id)))

(defmethod failed-p ((result reader-result))
  (null (id result)))

(defmethod undo ((result reader-result))
  (north:make-signed-request (client result) (format NIL "~a/write/~a" (base-url (client result)) (id result))
                             :post :params `(("action" . "delete"))))

(defmethod post ((post post) (client reader) &key verbose)
  (declare (ignore verbose))
  (error "Can't post ~a to Reader." (type-of post)))

(defmethod post ((post text-post) (client reader) &key verbose)
  (when verbose (verbose "Posting text to ~a" (base-url client)))
  (let* ((resp (north:make-signed-request client (format NIL "~a/write" (base-url client))
                                          :post :params `(("action" . "save")
                                                          ("tags" . ,(format NIL "~{~a~^,~}" (filter-tags (tags post) #'non-comma-p)))
                                                          ("text" . ,(if (file post) (alexandria:read-file-into-string (file post)) (description post)))
                                                          ("title" . ,(title post))
                                                          ("format" . ,(ecase (markup post)
                                                                         (:plain "0")
                                                                         (:html "1")
                                                                         (:markdown "2")
                                                                         (:markless "3"))))))
         (url (lquery:$1 (initialize resp) "#message a" (attr :href)))
         (id (car (last (cl-ppcre:split "/" url)))))
    (make-instance 'reader-result :url url :post id)))

(defmethod ready-p ((client reader))
  (not (null (north:token client))))

(defmethod setup ((client reader) &rest args)
  (cond ((null args)
         (setf (slot-value client 'base-url) (query "Enter the Reader base" :default "https://reader.tymoon.eu"))
         (setf (north:key client) (query "Enter the oAuth app key"))
         (setf (north:secret client) (query "Enter the oAuth app secret"))
         (let ((pin (query (format NIL "Enter the PIN from ~a" (north:initiate-authentication client)))))
           (north:complete-authentication client pin)))
        (T
         (apply #'reinitialize-instance client args))))

