#|
 This file is a part of Multiposter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
