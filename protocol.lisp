(in-package #:org.shirakumo.multiposter)

(defvar *multiposter* NIL)
(defvar *client-types* (make-hash-table :test 'equalp))

(defgeneric add-tag (tag post))
(defgeneric post (post client &key verbose))
(defgeneric ready-p (client))
(defgeneric setup (client &rest args))
(defgeneric undo (result))
(defgeneric failed-p (result))
(defgeneric add-client (client multiposter))
(defgeneric add-profile (profile multiposter))
(defgeneric find-profile (name multiposter))
(defgeneric find-client (name multiposter))

(defclass post ()
  ((title :initform NIL :accessor title)
   (header :initform NIL :accessor header)
   (footer :initform NIL :accessor footer)
   (description :initform NIL :accessor description)
   (content-warning :initform NIL :accessor content-warning)
   (tags :initform () :accessor tags)))

(defmethod shared-initialize :after ((post post) slots &key (title NIL title-p) (description NIL description-p) (header NIL header-p) (footer NIL footer-p) (content-warning NIL content-warning-p) tags)
  (when title-p (setf (title post) (or* title)))
  (when description-p (setf (description post) (or* description)))
  (when header-p (setf (header post) (or* header)))
  (when footer-p (setf (footer post) (or* footer)))
  (when content-warning-p (setf (content-warning post) (or* content-warning)))
  (dolist (tag tags) (add-tag tag post)))

(defmethod print-object ((post post) stream)
  (print-unreadable-object (post stream :type T :identity T)
    (format stream "~@[~a~]" (title post))))

(defmethod add-tag ((tag string) (post post))
  (let ((trimmed (remove #\Space tag)))
    ;; Tags are considered the same if they match without regard for spaces
    ;; or capitalisation.
    (loop for existing in (tags post)
          do (when (string-equal trimmed (remove #\Space existing))
               (return))
          finally (push tag (tags post))))
  post)

(defmethod compose-post ((post post) &rest args &key exclude-tags exclude-title &allow-other-keys)
  (remf args :exclude-tags)
  (remf args :exclude-title)
  (apply #'compose-post-text (if exclude-title (header post) (merge-paragraphs (title post) (header post)))
         (description post) (footer post) :tags (unless exclude-tags (tags post)) args))

(defclass image-post (post)
  ((files :initform () :accessor files)
   (file-descriptions :initform () :accessor file-descriptions)))

(defmethod shared-initialize :after ((post image-post) slots &key (files () files-p))
  (when files-p
    (let ((new-files ())
          (new-descriptions ()))
      (dolist (file files)
        (destructuring-bind (file &optional description) (enlist file)
          (let ((real-file (probe-file file)))
            (unless real-file
              (error "File does not exist:~%  ~a" file))
            (push real-file new-files)
            (push description new-descriptions))))
      (setf (files post) (nreverse new-files))
      (setf (file-descriptions post) (nreverse new-descriptions)))))

(defmethod print-object ((post image-post) stream)
  (print-unreadable-object (post stream :type T :identity T)
    (format stream "~{~a~^ ~}" (files post))))

(defclass video-post (post)
  ((file :accessor file)))

(defmethod shared-initialize :after ((post video-post) slots &key (file NIL file-p))
  (when file-p
    (let ((real-file (probe-file file)))
      (unless real-file
        (error "File does not exist:~%  ~a" file))
      (setf (file post) real-file))))

(defmethod print-object ((post video-post) stream)
  (print-unreadable-object (post stream :type T :identity T)
    (format stream "~a" (file post))))

(defmethod post ((file pathname) target &rest args)
  (apply #'post (cond ((file-type-p file *image-types*)
                       (make-instance 'image-post :files (list file)))
                      ((file-type-p file *video-types*)
                       (make-instance 'video-post :file file))
                      (T
                       (error "Unknown file type: ~a" (pathname-type file))))
         target args))

(defclass link-post (post)
  ((url :accessor url)))

(defmethod shared-initialize :after ((post link-post) slots &key (url NIL url-p))
  (when url-p
    (if (or* url)
        (setf (url post) url)
        (error "URL cannot be empty!"))))

(defmethod print-object ((post link-post) stream)
  (print-unreadable-object (post stream :type T :identity T)
    (format stream "~a" (url post))))

(defclass text-post (post)
  ())

(defmethod post ((text string) target &rest args)
  (apply #'post (if (cl-ppcre:scan "^https?://" text)
                    (make-instance 'text-post :description text)
                    (make-instance 'link-post :url text))
         target args))

(defclass client ()
  ((name :initarg :name :accessor name)
   (post-tags :initform () :accessor post-tags)))

(defmethod initialize-instance :after ((client client) &key post-tags setup)
  (unless (slot-boundp client 'name)
    (setf (name client) (string-downcase (type-of client))))
  (loop for (post-type . tags) in post-tags
        do (push (list* post-type (loop for tag in tags when (or* tag) collect tag))
                 (post-tags client)))
  (etypecase setup
    (null)
    ((member T :interactive)
     (setup client))
    (cons
     (apply #'setup client setup))))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~a" (name client))))

(defmethod initargs append ((client client))
  (list :name (name client) :post-tags (post-tags client)))

(defmacro define-client (name direct-superclasses direct-slots &rest options)
  `(progn (defclass ,name ,direct-superclasses
            ,direct-slots
            ,@options)
          (setf (gethash (string ',name) *client-types*) ',name)))

(defclass result ()
  ((client :initarg :client :accessor client)
   (post-object :initarg :post :initform NIL :accessor post-object)
   (url :initarg :url :initform NIL :accessor url)))

(defmethod print-object ((result result) stream)
  (print-unreadable-object (result stream :type T :identity T)
    (format stream "~a " (type-of (client result)))
    (if (failed-p result)
        (format stream "FAILED")
        (format stream "~a" (url result)))))

(defmethod undo ((result result))
  (error "Cannot undo this result for ~a.~@[~%Please delete the post at~%  ~a~]"
         (client result) (url result)))

(defmethod failed-p ((result result))
  (null (url result)))

(defmethod post :around ((post post) (client client) &rest args)
  (restart-case (let* ((tags (loop for (type . tags) in (post-tags client)
                                   when (typep post type)
                                   append tags))
                       (result (apply #'call-next-method (make-like post :tags tags) client args)))
                  (etypecase result
                    (result result)
                    (null (make-instance 'result :client client :post post :url ""))
                    (string (make-instance 'result :client client :post post :url result))
                    (pathname (make-instance 'result :client client :post post :url (path-url result)))))
    (continue ()
      :report "Return a failure result"
      (make-instance 'result :client client :post post :url NIL))))

(defclass profile ()
  ((name :initarg :name :accessor name)
   (clients :initform () :accessor clients)
   (tags :initarg :tags :initform () :accessor tags)
   (header :initform NIL :accessor header)
   (footer :initform NIL :accessor footer)))

(defmethod initialize-instance :after ((profile profile) &key header footer clients tags (multiposter *multiposter*))
  (setf (header profile) (or* header))
  (setf (footer profile) (or* footer))
  (setf (tags profile) tags)
  (setf (clients profile) (if clients
                              (loop for client in clients
                                    collect (etypecase client
                                              (client client)
                                              ((or symbol string)
                                               (or (find-client client multiposter)
                                                   (error "Unknown client: ~a" client)))))
                              (clients multiposter))))

(defmethod print-object ((profile profile) stream)
  (print-unreadable-object (profile stream :type T)
    (format stream "~a" (name profile))))

(defmethod initargs append ((profile profile))
  (list :name (name profile)
        :clients (mapcar #'name (clients profile))
        :tags (tags profile)
        :header (header profile)
        :footer (footer profile)))

(defmethod post ((post post) (profile profile) &rest args)
  (let ((post (make-like post :header (merge-paragraphs (header post) (header profile))
                              :footer (merge-paragraphs (footer post) (footer profile))
                              :tags (tags profile))))
    (apply #'post post (or (clients profile) (error "The profile has no clients")) args)))

(defclass multiposter ()
  ((clients :initform (make-hash-table :test 'equalp) :accessor clients)
   (profiles :initform (make-hash-table :test 'equalp) :accessor profiles)
   (default-profile :initform NIL :reader default-profile)))

(defmethod print-object ((multiposter multiposter) stream)
  (print-unreadable-object (multiposter stream :type T)
    (format stream "~{~a~^ ~}" (alexandria:hash-table-keys (clients multiposter)))))

(defmethod shared-initialize :after ((multiposter multiposter) slots &key (default-profile NIL default-profile-p))
  (when default-profile-p (setf (default-profile multiposter) default-profile)))

(defmethod (setf default-profile) (profile (multiposter multiposter))
  (setf (default-profile multiposter) (or (find-profile profile multiposter)
                                          (error "Unknown profile: ~a" profile))))

(defmethod (setf default-profile) ((profile null) (multiposter multiposter))
  (setf (slot-value multiposter 'default-profile) NIL))

(defmethod (setf default-profile) ((profile profile) (multiposter multiposter))
  (setf (slot-value multiposter 'default-profile) profile))

(defmethod post ((post post) (multiposter multiposter) &rest args)
  (apply #'post post (or (default-profile multiposter)
                         (alexandria:hash-table-values (clients multiposter)))
         args))

(defmethod post ((post post) (clients cons) &rest args)
  (let ((results (loop for client in clients
                       collect (apply #'post post client args))))
    (restart-case (dolist (result results results)
                    (when (failed-p result)
                      (error "Failed to post to ~a."
                             (client result))))
      (abort ()
        :report "Undo all posts"
        (loop for result in results
              if (failed-p result)
              do (undo result)
              else
              collect result))
      (continue ()
        :report "Ignore the failure"
        (loop for result in results
              unless (failed-p result)
              collect result)))))

(defmethod post (thing (default (eql T)) &rest args)
  (apply #'post thing *multiposter* args))

(defmethod add-client :before ((client client) (multiposter multiposter))
  (when (gethash (string (name client)) (clients multiposter))
    (cerror "Replace the client" "A client with the name ~s already exists!" (name client)))
  (unless (ready-p client)
    (setup client)
    (unless (ready-p client)
      (cerror "Add the client anyway" "The client ~a is not ready." client))))

(defmethod add-client ((client client) (multiposter multiposter))
  (setf (gethash (string (name client)) (clients multiposter)) client))

(defmethod add-client ((spec cons) (multiposter multiposter))
  (add-client (apply #'make-instance spec) multiposter))

(defmethod add-client (thing (default (eql T)))
  (add-client thing *multiposter*))

(defmethod add-profile :before ((profile profile) (multiposter multiposter))
  (when (gethash (string (name profile)) (profiles multiposter))
    (cerror "Replace the profile" "A profile with the name ~s already exists!" (name profile))))

(defmethod add-profile ((profile profile) (multiposter multiposter))
  (setf (gethash (string (name profile)) (profiles multiposter)) profile))

(defmethod add-profile ((spec cons) (multiposter multiposter))
  (add-profile (apply #'make-instance 'profile :multiposter multiposter spec) multiposter))

(defmethod add-profile (thing (default (eql T)))
  (add-profile thing *multiposter*))

(defmethod find-profile ((name string) (multiposter multiposter))
  (gethash name (profiles multiposter)))

(defmethod find-profile ((name symbol) (multiposter multiposter))
  (find-profile (string name) multiposter))

(defmethod find-profile (thing (default (eql T)))
  (find-profile thing *multiposter*))

(defmethod find-client ((name string) (multiposter multiposter))
  (gethash name (clients multiposter)))

(defmethod find-client ((name symbol) (multiposter multiposter))
  (find-client (string name) multiposter))

(defmethod find-client (thing (default (eql T)))
  (find-client thing *multiposter*))
