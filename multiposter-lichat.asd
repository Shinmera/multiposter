(asdf:defsystem multiposter-lichat
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Lichat client backend for Multiposter"
  :homepage "https://Shinmera.github.io/multiposter/"
  :bug-tracker "https://github.com/Shinmera/multiposter/issues"
  :source-control (:git "https://github.com/Shinmera/multiposter.git")
  :serial T
  :components ((:file "lichat"))
  :depends-on (:multiposter
               :lichat-tcp-client
               :trivial-mimes
               :cl-base64))
