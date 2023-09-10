(in-package #:org.shirakumo.multiposter)

(defun main/post (thing &key title profile description tag abort-on-failure verbose)
  (flet ((post (type &rest args)
           (handler-bind ((error (lambda (e)
                                   (format *error-output* "~&ERROR: ~a~%" e)
                                   (cond ((uiop:getenv "DEBUG")
                                          (invoke-debugger e))
                                         (abort-on-failure
                                          (abort e))
                                         (T
                                          (continue e))))))
             (let ((results (post (apply #'make-instance type
                                         :title title
                                         :description description
                                         :tags (parse-tags tag)
                                         args)
                                  (if profile
                                      (or (find-profile profile *multiposter*)
                                          (error "Unknown profile: ~a" profile))
                                      *multiposter*)
                                  :verbose verbose)))
               (dolist (result results results)
                 (format *standard-output* "~&~a: ~a~%" (name (client result)) (url result)))))))
    (cond ((listp thing)
           (post 'image-post :files (loop for path in thing
                                          collect (pathname-utils:parse-native-namestring path))))
          ((cl-ppcre:scan "^\\(.*\\)" thing)
           (post 'image-post :files (loop for path in (read-from-string thing)
                                          collect (etypecase path
                                                    (pathname path)
                                                    (string (pathname-utils:parse-native-namestring path))
                                                    (symbol (pathname-utils:parse-native-namestring (string-downcase path)))))))
          ((cl-ppcre:scan "^https?://" thing)
           (post 'link-post :url thing))
          ((file-type-p thing *image-types*)
           (post 'image-post :files (list (pathname-utils:parse-native-namestring thing))))
          ((file-type-p thing *video-types*)
           (post 'video-post :file (pathname-utils:parse-native-namestring thing)))
          (T
           (setf description (merge-paragraphs thing description))
           (post 'text-post)))))

(defun main/add (kind name/type &key client tag header footer verbose ((&rest rest)))
  (cond ((string-equal kind "profile")
         (add-profile (list :name name/type
                            :clients client 
                            :tags (parse-tags tag)
                            :header header
                            :footer footer)
                      *multiposter*)
         (when verbose (verbose "Added profile ~a" name/type)))
        ((string-equal kind "client")
         (add-client (list (or (gethash name/type *client-types*)
                               (error "Unknown client type: ~a" name/type))
                           :name client
                           :post-tags (loop for tag in (parse-tags tag)
                                            collect (cl-ppcre:register-groups-bind (type tag) ("^(:?(.+?):)?(.+)$" tag)
                                                      (list (cond ((null type) 'post)
                                                                  ((string-equal type "post") 'post)
                                                                  ((string-equal type "image") 'image-post)
                                                                  ((string-equal type "video") 'video-post)
                                                                  ((string-equal type "link") 'link-post)
                                                                  ((string-equal type "text") 'text-post))
                                                            tag)))
                           :setup (if rest (loop for (k v) on rest by #'cddr
                                                 collect (intern (string-upcase k) "KEYWORD")
                                                 collect v)))
                     *multiposter*)
         (when verbose (verbose "Added client ~a" (or client name/type))))
        (T
         (error "Unknown thing to add: ~a" kind)))
  (save-config))

(defun main/remove (kind name &key)
  (cond ((string-equal kind "profile")
         (unless (find-profile name *multiposter*)
           (error "Unknown profile: ~a" name))
         (remhash name (profiles *multiposter*)))
        ((string-equal kind "client")
         (unless (find-client name *multiposter*)
           (error "Unknown client: ~a" name))
         (remhash name (clients *multiposter*)))
        (T
         (error "Unknown thing to add: ~a" kind)))
  (save-config))

(defun main/set (property &rest args)
  (cond ((string-equal property "default-profile")
         (setf (default-profile *multiposter*) (first args)))
        (T
         (error "Unknown thing to set: ~a" property)))
  (save-config))

(defun main/list (kind &key verbose)
  (cond ((string-equal kind "profiles")
         (if (not verbose)
             (format *standard-output* "~{~a~^ ~}~%" (alexandria:hash-table-keys (profiles *multiposter*)))
             (loop for profile being the hash-values of (profiles *multiposter*)
                   do (describe-object profile *standard-output*))))
        ((string-equal kind "clients")
         (if (not verbose)
             (format *standard-output* "~{~a~^ ~}~%" (alexandria:hash-table-keys (clients *multiposter*)))
             (loop for client being the hash-values of (clients *multiposter*)
                   do (describe-object client *standard-output*))))
        ((string-equal kind "client-types")
         (if (not verbose)
             (format *standard-output* "~{~a~^ ~}~%" (alexandria:hash-table-keys *client-types*))
             (loop for type being the hash-values of *client-types*
                   do (error "FIXME: todo"))))
        (T
         (error "Unknown thing to list: ~a" kind))))

(defun main/help ()
  (format T "Commands:

post                  Make a new post
  [url | file | files | text]
  -t --title title      The title of the post. If the service has no
                        explicit title field, it is prepended to the
                        description
  -p --profile profile  The profile to use for the post. If no profile
                        is specified, posts to the default profile if
                        any, or all configured clients if not.
  -d --description description
                        The description text to add to the post. This
                        may be truncated if it is too long for a
                        service
  -# --tag [tag | tags] One or more tags to add to the post. May be
                        specified multiple times, or tags may be
                        separated by commas. If a tag contains
                        characters that a service does not support,
                        the characters will be removed from the tag
  -a --abort-on-failure If set and one client fails to post, all posts
                        will be deleted. By default failing clients
                        are simply ignored.
  -v --verbose          Print status updates about what's happening

add profile           Add a new profile
  name                  The name of the profile to add. If the name is
                        already taken, an error is signalled
  -c --client [client | clients]
                        One or more clients this profile will post
                        to. May be specified multiple times, or
                        clients may be separated by commas
  -# --tag [tag | tags] One or more tags to add to each post. May be
                        specified multiple times, or tags may be
                        separated by commas
  -h --header header    A header to prepend to each post description
  -f --footer footer    A footer to append to each post description

add client            Add a new client
  type                  The type of client to add. To list supported
                        client types, use list client-types
  -c --client name      The name of the client. If unspecified,
                        defaults to the client type. If the name is
                        already taken, an error is signalled
  -# --tag [tag | tags] One or more tags to add to each post depending
                        on the post's type. May be specified multiple
                        times, or tags may be separated by
                        commas. Each tag may start with a prefix
                        denoting the post type to apply to, followed
                        by a colon. Valid post types are:
    text                  Posts without any file attachment
    image                 Posts that include one or more images
    video                 Posts that include a video
    link                  Posts that have a leading link
  -v --verbose          Print status updates about what's happening.
  -- args*              Client type specific arguments to handle the
                        login. If unspecified, an interactive setup
                        will be used

remove profile        Remove an existing profile
  name                  The name of the profile to remove

remove client         Remove an existing client
  name                  The name of the client to remove

set default-profile   Set the default profile to use.
  [profile]             The profile to set as default. If none is
                        specified, the default is unset.

list profiles         List known profiles
  -v --verbose          Print detailed information about each profile

list clients          List known clients
  -v --verbose          Print detailed information about each client

list client-types     List available client types
  -v --verbose          Print detailed information about each type's
                        arguments

help                  Shows this help listing

Environment Variables:

DEBUG                 When set, will enter the debugger on error
MULTIPOSTER_CONFIG    The path to the configuration file.
"))

(defun parse-args (args &key flags chars)
  (let ((kargs ())
        (pargs ()))
    (loop for arg = (pop args)
          while arg
          do (labels ((next-arg (arg)
                        (if args
                            (pop args)
                            (error "Missing value for ~a" arg)))
                      (handle-argument (arg)
                        (setf (getf kargs arg) (cond ((find arg flags :test #'string-equal)
                                                      T)
                                                     ((null (getf kargs arg))
                                                      (next-arg arg))
                                                     ((consp (getf kargs arg))
                                                      (list* (next-arg arg) (getf kargs arg)))
                                                     (T
                                                      (list (next-arg arg) (getf kargs arg)))))))
               (cond ((string= "--" arg)
                      (shiftf (getf kargs '&rest) args ()))
                     ((string= "--" arg :end2 2)
                      (handle-argument (find-symbol (string-upcase (subseq arg 2)) "KEYWORD")))
                     ((string= "-" arg :end2 1)
                      (loop for char across (subseq arg 1)
                            for arg = (getf chars char)
                            do (cond (arg
                                      (handle-argument arg))
                                     (T
                                      (error "No such argument ~a" char)))))
                     (T
                      (push arg pargs)))))
    (append (nreverse pargs) kargs)))

(defun main (&optional (args (uiop:command-line-arguments)))
  (let ((args (or args '("help"))))
    (handler-case
        (handler-bind ((error (lambda (e)
                                (when (uiop:getenv "DEBUG")
                                  (invoke-debugger e)))))
          (destructuring-bind (command . args) args
            (let ((cmdfun (find-symbol (format NIL "~a/~:@(~a~)" 'main command) #.*package*)))
              (unless cmdfun
                (error "No command named ~s." command))
              (let ((*multiposter* (load-config NIL)))
                (apply #'funcall cmdfun (parse-args args :flags '(:verbose :abort-on-failure)
                                                         :chars '(#\v :verbose
                                                                  #\# :tag
                                                                  #\c :client
                                                                  #\p :profile
                                                                  #\t :title
                                                                  #\d :description
                                                                  #\f :footer
                                                                  #\h :header
                                                                  #\a :abort-on-failure)))))))
      (error (e)
        (format *error-output* "~&ERROR: ~a~%" e)
        (uiop:quit 2)))
    (uiop:quit)))
