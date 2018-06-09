#|
 This file is a part of Multiposter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem multiposter-twitter
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Twitter client backend for Multiposter"
  :homepage "https://github.com/Shinmera/multiposter"
  :serial T
  :components ((:file "twitter"))
  :depends-on (:multiposter
               :chirp))
