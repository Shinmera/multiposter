(in-package #:org.shirakumo.multiposter)

(defun config-file ()
  (or (when (envvar "MULTIPOSTER_CONFIG")
        (pathname-utils:parse-native-namestring (envvar "MULTIPOSTER_CONFIG")))
      (merge-pathnames #p "multiposter/multiposter.lisp"
                       (or (when (envvar "APPDATA")
                             (pathname-utils:parse-native-namestring (envvar "APPDATA") :as :directory))
                           (when (envvar "XDG_CONFIG_HOME")
                             (pathname-utils:parse-native-namestring (envvar "XDG_CONFIG_HOME") :as :directory))
                           (merge-pathnames (make-pathname :directory '(:relative ".config")) (user-homedir-pathname))))))

(defun load-config (&optional (multiposter *multiposter*) (file (config-file)))
  (let ((*package* #.*package*))
    (ubiquitous:restore file))
  (unless multiposter
    (setf multiposter (make-instance 'multiposter)))
  (loop for profile in (ubiquitous:value :profiles)
        do (add-profile profile multiposter))
  (loop for client in (ubiquitous:value :clients)
        do (add-client client multiposter))
  (setf (default-profile multiposter) (ubiquitous:value :default-profile))
  multiposter)

(defun save-config (&optional (multiposter *multiposter*) (file (config-file)))
  (setf (ubiquitous:value :profiles) (loop for profile being the hash-values of (profiles multiposter)
                                           collect (initargs profile)))
  (setf (ubiquitous:value :clients) (loop for client being the hash-values of (clients multiposter)
                                          collect (list* (type-of client) (initargs client))))
  (setf (ubiquitous:value :default-profile) (name (default-profile multiposter)))
  (let ((*package* #.*package*))
    (ubiquitous:offload file)))