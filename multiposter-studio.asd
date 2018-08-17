#|
 This file is a part of Multiposter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem multiposter-studio
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Studio client backend for Multiposter"
  :homepage "https://Shinmera.github.io/multiposter/"
  :bug-tracker "https://github.com/Shinmera/multiposter/issues"
  :source-control (:git "https://github.com/Shinmera/multiposter.git")
  :serial T
  :components ((:file "studio"))
  :depends-on (:multiposter
               :studio-client
               :north-dexador))
