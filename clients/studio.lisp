(in-package #:org.shirakumo.multiposter)

(define-client studio (file studio-client:client)
  ())

(defmethod initargs append ((client studio))
  (list :key (north:key client)
        :secret (north:secret client)
        :token (north:token client)
        :token-secret (north:token-secret client)
        :api-base (studio-client:api-base client)))

(defclass studio-result (result)
  ((upload :initarg :upload :initform NIL :accessor upload)))

(defmethod failed-p ((result studio-result))
  (null (upload result)))

(defmethod undo ((result studio-result))
  (studio-client:delete (client result) (upload result)))

(defmethod post ((post post) (client studio) &key verbose)
  (declare (ignore verbose))
  (error "Can't post ~a to Studio." (type-of post)))

(defmethod post ((post image-post) (client studio) &key verbose)
  (when verbose (verbose "Posting image to ~a" (studio-client:api-base client)))
  (let ((upload (studio-client:make-upload client (or (title post) (pathname-name (first (files post)))) (files post)
                                           :tags (tags post) :description (compose-post post :exclude-tags T))))
    (make-instance 'studio-result :client client :post post :upload upload :url (studio-client:url upload))))

(defmethod ready-p ((client studio))
  (not (null (north:token client))))

(defmethod setup ((client studio) &rest args)
  (cond ((null args)
         (setf (slot-value client 'studio-client:api-base) (query "Enter the Studio API base" :default "https://studio.tymoon.eu/api"))
         (setf (north:key client) (query "Enter the oAuth app key"))
         (setf (north:secret client) (query "Enter the oAuth app secret"))
         (let ((pin (query (format NIL "Enter the PIN from ~a" (north:initiate-authentication client)))))
           (north:complete-authentication client pin)))
        (T
         (apply #'reinitialize-instance client args))))
