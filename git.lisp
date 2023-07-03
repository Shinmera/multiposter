(defpackage #:multiposter-git
  (:nicknames #:org.shirakumo.multiposter.git)
  (:use #:cl)
  (:export
   #:client))
(in-package #:org.shirakumo.multiposter.git)

(defclass client (multiposter:client)
  ((repository :initarg :repository :accessor repository)))

(defmethod make-load-form ((client client) &optional env)
  (declare (ignore env))
  `(make-instance 'client
                  :repository (legit:init ,(legit:location (repository client)))))

(defmethod multiposter:login ((client client) &key repository)
  (let ((repository (or repository (multiposter:prompt "Please enter the repository path"
                                                       :converter #'legit:init))))
    (setf (repository client) repository)))

(defmethod multiposter:post-text ((client client) text &rest args)
  (declare (ignore args)))

(defmethod multiposter:post-link ((client client) text &rest args)
  (declare (ignore args)))

(defun post-file (client path &key title description tags link)
  (let* ((repository (repository client))
         (paths (mapcar #'uiop:truenamize (if (listp path) path (list path))))
         (repo (uiop:truenamize (legit:location repository)))
         (staged NIL))
    (legit:pull repository)
    (loop for path in paths
          do (when (uiop:subpathp path repo)
               (setf staged T)
               (legit:add repository (uiop:enough-pathname path repo))))
    (when staged
      (legit:commit repository (format NIL "~@[~a~%~%~]~@[~a~]~@[~&~%Tags:~{ ~a~}~]~@[~%URL: ~a~]"
                                       title description tags link))
      (legit:push repository)
      (format NIL "file://~a" path))))

(defmethod multiposter:post-image ((client client) path &rest args)
  (apply #'post-file client path args))

(defmethod multiposter:post-video ((client client) path &rest args)
  (apply #'post-file client path args))

