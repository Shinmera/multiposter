(in-package #:org.shirakumo.multiposter)

(defvar *image-types* '("png" "jpg" "jpeg" "gif" "bmp" "svg"))
(defvar *video-types* '("gifv" "apng" "mp4" "webm" "mov" "mkv"))

(defclass post ()
  ((title :initform NIL :accessor title)
   (description :initform NIL :accessor description)
   (content-warning :initform NIL :accessor content-warning)
   (tags :initform () :accessor tags)))

(defmethod initialize-instance :after ((post post) &key title description content-warning tags)
  (setf (title post) (or* title))
  (setf (description post) (or* description))
  (setf (content-warning post) (or* content-warning))
  (dolist (tag tags) (add-tag tag post)))

(defmethod print-object ((post post) stream)
  (print-unreadable-object (post stream :type T :identity T)
    (format stream "~@[~a~]" (title post))))

(defgeneric add-tag (tag post))

(defmethod add-tag ((tag string) (post post))
  (let ((trimmed (remove #\Space tag)))
    ;; Tags are considered the same if they match without regard for spaces
    ;; or capitalisation.
    (loop for existing in (tags post)
          do (when (string-equal trimmed (remove #\Space existing))
               (return))
          finally (push tag (tags post))))
  post)

(defclass image-post (post)
  ((files :initform () :accessor files)
   (file-descriptions :initform () :accessor file-descriptions)))

(defmethod initialize-instance :after ((post image-post) &key files)
  (dolist (file files)
    (destructuring-bind (file &optional description) (enlist file)
      (let ((real-file (probe-file file)))
        (unless real-file
          (error "File does not exist:~%  ~a" file))
        (push real-file (files post))
        (push description (file-descriptions post)))))
  (setf (files post) (nreverse (files post)))
  (setf (file-descriptions post) (nreverse (file-descriptions post))))

(defmethod print-object ((post image-post) stream)
  (print-unreadable-object (post stream :type T :identity T)
    (format stream "~{~a~^ ~}" (files post))))

(defclass video-post (post)
  ((file :accessor file)))

(defmethod initialize-instance :after ((post video-post) &key file)
  (let ((real-file (probe-file file)))
    (unless real-file
      (error "File does not exist:~%  ~a" file))
    (setf (file post) real-file)))

(defmethod print-object ((post video-post) stream)
  (print-unreadable-object (post stream :type T :identity T)
    (format stream "~a" (file post))))

(defclass link-post (post)
  ((url :accessor url)))

(defmethod initialize-instance :after ((post link-post) &key url)
  (setf (url post) (or* url))
  (unless (url post)
    (error "URL cannot be empty!")))

(defmethod print-object ((post link-post) stream)
  (print-unreadable-object (post stream :type T :identity T)
    (format stream "~a" (url post))))

(defclass client ()
  ((name :initarg :name :accessor name)
   (post-tags :initform () :accessor post-tags)))

(defmethod initialize-instance :after ((client client) &key post-tags)
  (unless (slot-boundp client 'name)
    (setf (name client) (string-downcase (type-of client))))
  (loop for (post-type . tags) in post-tags
        do (push (list* post-type (loop for tag in tags when (or* tag) collect tag))
                 (post-tags client))))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~a" (name client))))

(defgeneric post (post client))
(defgeneric ready-p (client))
(defgeneric login (client))

(defmethod post :around ((post post) (client client))
  (restart-case (call-next-method)
    (continue ()
      :report "Return a failure result"
      (make-instance 'result :client client :post post :url NIL))))

(defclass result ()
  ((client :initarg :client :accessor client)
   (post :initarg :post :accessor post)
   (url :initarg :url :initform NIL :accessor url)))

(defmethod print-object ((result result) stream)
  (print-unreadable-object (result stream :type T :identity T)
    (format stream "~a " (type-of (client result)))
    (if (failed-p result)
        (format stream "FAILED")
        (format stream "~a" (url result)))))

(defgeneric undo (result))
(defgeneric failed-p (result))

(defmethod failed-p ((result result))
  (null (url result)))

(defclass profile ()
  ((name :initarg :name :accessor name)
   (clients :initform () :accessor clients)
   (tags :initarg :tags :initform () :accessor tags)
   (header :initform NIL :accessor header)
   (footer :initform NIL :accessor footer)))

(defmethod initialize-instance :after ((profile profile) &key header footer clients tags multiposter)
  (setf (header post) (or* header))
  (setf (footer post) (or* footer))
  (setf (tags profile) tags)
  (setf (clients profile) (loop for client in clients
                                collect (etypecase client
                                          (client client)
                                          (symbol (gethash (string client) (clients profile)))
                                          (string (gethash client (clients profile)))))))

(defmethod print-object ((profile profile) stream)
  (print-unreadable-object (profile stream :type T)
    (format stream "~a" (name profile))))

(defmethod post ((post post) (profile profile))
  (setf (description post) (merge-paragraphs (header profile) (description post) (footer profile)))
  (dolist (tag (tags profile))
    (add-tag tag post))
  (post post (clients profile)))

(defclass multiposter ()
  ((clients :initform (make-hash-table :test 'equalp) :accessor clients)
   (profiles :initform (make-hash-table :test 'equalp) :accessor profiles)))

(defmethod print-object ((multiposter multiposter) stream)
  (print-unreadable-object (multiposter stream :type T)
    (format stream "~{~a~^ ~}" (mapcar #'type-of (clients multiposter)))))

(defmethod post ((post post) (multiposter multiposter))
  (post post (clients multiposter)))

(defmethod post ((post post) (clients list))
  (let ((results (loop for client in (clients profile)
                       collect (post post client))))
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

(defgeneric add-client (client multiposter))

(defmethod add-client :before ((client client) (multiposter multiposter))
  (when (gethash (string (name client)) (clients multiposter))
    (cerror "Replace the client" "A client with the name ~s already exists!" (name client)))
  (unless (ready-p client)
    (login client)
    (unless (ready-p client)
      (cerror "Add the client anyway" "The client ~a is not ready." client))))

(defmethod add-client ((client client) (multiposter multiposter))
  (setf (gethash (sring (name client)) (clients multiposter)) multiposter))

(defmethod add-client ((spec cons) (multiposter multiposter))
  (add-client (apply #'make-instance spec) multiposter))
