(in-package #:org.shirakumo.multiposter)

(define-client file (client)
  ((path :initarg :path :initform NIL :accessor path)))

(defmethod initargs append ((client file))
  (list :path (path client)))

(defclass file-result (result)
  ((files :initarg :files :accessor files)))

(defmethod undo ((result file-result))
  (dolist (file (files result))
    (uiop:delete-file-if-exists file)))

(defmethod post ((post post) (client file) &key verbose)
  (let* ((name (format NIL "~a~@[ ~a~]" (timestamp) (title post)))
         (path (merge-pathnames (path client) (make-pathname :name name :type "txt"))))
    (when verbose (verbose "Writing post to ~a" path))
    (alexandria:write-string-into-file
     (compose-post post :tag-separator ", " :tag-format "~a")
     path)
    (make-instance 'file-result :post post :client client :files (list path)
                                :url (path-url path))))

(defmethod post ((post image-post) (client file) &key verbose)
  (let* ((result (call-next-method))
         (path (first (files result))))
    (loop for i from 0
          for source in (files post)
          for target = (make-pathname :name (format NIL "~a ~d" (pathname-name path) i)
                                      :type (pathname-type source)
                                      :defaults path)
          do (when verbose (verbose "Writing file ~a" target))
             (uiop:copy-file source target)
             (push target (files result)))
    result))

(defmethod post ((post video-post) (client file) &key verbose)
  (let* ((result (call-next-method))
         (path (first (files result)))
         (target (make-pathname :type (pathname-type (file post))
                                :defaults path)))
    (when verbose (verbose "Writing file ~a" target))
    (uiop:copy-file (file post) target)
    (push target (files result))
    result))

(defmethod post ((post link-post) (client file) &rest args)
  (apply #'call-next-method
         (make-like post :title (url post) :header (merge-paragraphs (title post) (header post)))
         client args))

(defmethod ready-p ((client file))
  (uiop:directory-exists-p (path client)))

(defmethod setup ((client file) &rest args &key path)
  (cond ((and (null args) (null (path client)))
         (setf (path client) (query "Enter the directory to save files in"
                                    :coerce (lambda (x) (pathname-utils:parse-native-namestring x :as :directory))
                                    :check #'ensure-directories-exist)))
        (T
         (setf (path client) path))))
