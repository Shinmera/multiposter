(in-package #:org.shirakumo.multiposter)

(define-client file (client)
  ((path :initarg :path :initform NIL :accessor path)))

(defmethod initargs append ((client file))
  (list :path (path client)))

(defmethod post ((post post) (client file) &key verbose)
  (let* ((name (format NIL "~a~@[ ~a~]" (timestamp) (title post)))
         (path (merge-pathnames (path client) (make-pathname :name name :type "txt"))))
    (when verbose (verbose "Writing post to ~a" path))
    (alexandria:write-string-into-file
     (compose-post post :tag-separator ", " :tag-format "~a")
     path)
    path))

(defmethod post ((post image-post) (client file) &key verbose)
  (let ((path (call-next-method)))
    (loop for i from 0
          for source in (files post)
          for target = (make-pathname :name (format NIL "~a ~d" (pathname-name path) i)
                                      :type (pathname-type source)
                                      :defaults path)
          do (when verbose (verbose "Writing file ~a" target))
             (uiop:copy-file source target))))

(defmethod post ((post video-post) (client file) &key verbose)
  (let* ((path (call-next-method))
         (target (make-pathname :type (pathname-type (file post))
                                :defaults path)))
    (when verbose (verbose "Writing file ~a" target))
    (uiop:copy-file (file post) target)))

(defmethod post ((post link-post) (client file) &rest args)
  (apply #'call-next-method
         (make-like post :title (url post) :header (merge-paragraphs (title post) (header post)))
         client args))

(defmethod ready-p ((client file))
  (path client))

(defmethod setup ((client file) &rest args &key path)
  (cond ((null args)
         (setf (path client) (query "Enter the directory to save files in"
                                    :coerce (lambda (x) (pathname-utils:parse-native-namestring x :as :directory))
                                    :check #'ensure-directories-exist)))
        (T
         (setf (path client) path))))
