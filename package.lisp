(defpackage #:multiposter
  (:nicknames #:org.shirakumo.multiposter)
  (:use #:cl)
  ;; interface.lisp
  (:export
   #:*client*
   #:*config-path*
   #:restore
   #:offload
   #:setup
   #:text
   #:link
   #:image
   #:video)
  ;; protocol.lisp
  (:export
   #:client
   #:client-types
   #:login
   #:post
   #:post-text
   #:post-link
   #:post-image
   #:post-video
   #:multiposter
   #:clients
   #:primary)
  ;; toolkit.lisp
  (:export
   #:prompt
   #:extract-links
   #:limit-text-with-links))
