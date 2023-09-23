(in-package #:org.shirakumo.multiposter)

(define-client git (file)
  ())

(defun %git (verbose client &rest args)
  (let ((cmd (list* "git" "-C" (pathname-utils:native-namestring (path client))
                    (loop for arg in args
                          collect (etypecase arg
                                    (string arg)
                                    (pathname (pathname-utils:native-namestring arg)))))))
    (when verbose (verbose "Running ~{~a~^ ~}" cmd))
    (uiop:run-program cmd :output (if verbose *error-output*) :error-output (if verbose *error-output*))))

(defun add-git-file (source client &optional verbose)
  (let ((file (cond ((pathname-utils:subpath-p source (path client))
                     source)
                    (T
                     (let ((target (make-pathname :name (pathname-name source) 
                                                  :type (pathname-type source)
                                                  :defaults (path client))))
                       (uiop:copy-file source target)
                       target)))))
    (%git verbose client "add" file)
    file))

(defun git-current-commit (client)
  (run* "git" "-C" (pathname-utils:native-namestring (path client)) "rev-parse" "HEAD"))

(defclass git-result (file-result)
  ((commit :initarg :commit :initform NIL :accessor commit)))

(defmethod undo ((result git-result))
  (%git NIL (client result) "reset" (format NIL "~a~~" (commit result))))

(defmethod failed-p ((result git-result))
  (null (commit result)))

(defmethod post :around ((post post) (client git) &key verbose)
  (%git verbose client "pull")
  (let ((result (call-next-method)))
    (let ((status (run* "git" "-C" (pathname-utils:native-namestring (path client)) "status" "--porcelain=v1")))
      (when (cl-ppcre:scan "^A" status)
        (%git verbose client "commit" "-m" (compose-post post :tag-separator ", " :tag-format "~a"))
        (setf (commit result) (git-current-commit client))
        (%git verbose client "push")))
    result))

(defmethod post ((post text-post) (client git) &key verbose)
  (let ((result (change-class (call-next-method) 'git-result)))
    (add-git-file (first (files result)) client verbose)
    result))

(defmethod post ((post image-post) (client git) &key verbose)
  (let ((result (make-instance 'git-result :client client :post post :url (path-url (path client)))))
    (dolist (file (files post) result)
      (add-git-file file client verbose)
      (push file (files result)))))

(defmethod post ((post video-post) (client git) &key verbose)
  (let ((result (make-instance 'git-result :client client :post post :url (path-url (path client)))))
    (add-git-file (file post) client verbose)
    (push (file post) (files result))
    result))

(defmethod post ((post link-post) (client git) &key verbose)
  (let ((result (make-instance 'git-result :client client :post post :url (path-url (path client))))
        (file (make-pathname :name (format NIL "~a~@[ ~a~]" (timestamp) (title post)) :type "url"
                             :defaults (uiop:temporary-directory))))
    (alexandria:write-string-into-file
     (format NIL "[InternetShortcut]~%URL=~a" (url post)) file)
    (add-git-file file client verbose)
    (push file (files result))
    result))
