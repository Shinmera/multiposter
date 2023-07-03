(asdf:defsystem multiposter
  :version "1.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A small application to post to multiple services at once."
  :homepage "https://Shinmera.github.io/multiposter/"
  :bug-tracker "https://github.com/Shinmera/multiposter/issues"
  :source-control (:git "https://github.com/Shinmera/multiposter.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "interface")
               (:file "protocol")
               (:file "documentation"))
  :depends-on (:cl-ppcre
               :documentation-utils))
