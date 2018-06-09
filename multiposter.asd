#|
 This file is a part of Multiposter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem multiposter
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A small application to post to multiple services at once."
  :homepage "https://github.com/Shinmera/multiposter"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "interface")
               (:file "protocol")
               (:file "documentation"))
  :depends-on (:cl-ppcre
               :documentation-utils))
