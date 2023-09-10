(asdf:defsystem multiposter
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An application to post to multiple services at once."
  :homepage "https://shinmera.github.io/multiposter/"
  :bug-tracker "https://github.com/shinmera/multiposter/issues"
  :source-control (:git "https://github.com/shinmera/multiposter.git")
  :build-operation "program-op"
  :build-pathname "multiposter"
  :entry-point "org.shirakumo.multiposter::main"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "config")
               (:file "protocol")
               (:file "main")
               (:module "clients"
                :components ((:file "dummy")
                             (:file "file")
                             (:file "git")
                             (:file "lichat"))))
  :depends-on (:ubiquitous
               :closer-mop
               :cl-ppcre
               :alexandria
               :trivial-arguments
               :pathname-utils

               :lichat-tcp-client))
