(in-package #:org.shirakumo.multiposter)

(define-client git (file)
  ())

(defun git (verbose client &rest args)
  (uiop:run-program (list* "git" "-C" (pathname-utils:native-namestring (path client))
                           (loop for arg in args
                                 collect (etypecase arg
                                           (string arg)
                                           (pathname (pathname-utils:native-namestring arg)))))
                    :output (if verbose *error-output*) :error-output (if verbose *error-output*)))

(defun add-git-file (source client &optional verbose)
  (let ((file (cond ((pathname-utils:subpath-p source (path client))
                     source)
                    (T
                     (let ((target (make-pathname :name (pathname-name source) 
                                                  :type (pathname-type source)
                                                  :defaults (path client))))
                       (uiop:copy-file source target)
                       target)))))
    (git verbose client "add" file)
    file))

(defmethod post :around ((post post) (client git) &key verbose)
  (git verbose client "pull")
  (let ((target (call-next-method)))
    (git verbose client "commit" "-m" (compose-post post :tag-separator ", " :tag-format "~a"))
    (git verbose client "push")
    target))

(defmethod post ((post text-post) (client git) &key verbose)
  (add-git-file (call-next-method) client verbose))

(defmethod post ((post image-post) (client git) &key verbose)
  (loop for file in (files post)
        do (add-git-file file client verbose))
  (path client))

(defmethod post ((post video-post) (client git) &key verbose)
  (add-git-file (file post) client verbose))

(defmethod post ((post link-post) (client git) &key verbose)
  (let ((file (make-pathname :name (format NIL "~a~@[ ~a~]" (timestamp) (title post)) :type "url"
                             :defaults (uiop:temporary-directory))))
    (alexandria:write-string-into-file
     (format NIL "[InternetShortcut]~%URL=~a" (url post)) file)
    (add-git-file file client verbose)))
