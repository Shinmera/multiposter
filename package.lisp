(defpackage #:org.shirakumo.multiposter
  (:use #:cl)
  (:local-nicknames
   (#:cohost #:org.shirakumo.clohost))
  ;; protocol.lisp
  (:export
   #:add-tag
   #:post
   #:ready-p
   #:setup
   #:undo
   #:failed-p
   #:add-client
   #:add-profile
   #:add-schedule
   #:find-profile
   #:find-client
   #:find-schedule
   #:schedule
   #:due-time
   #:post-object
   #:target
   #:due-p
   #:post
   #:title
   #:header
   #:footer
   #:description
   #:content-warning
   #:tags
   #:compose-post
   #:image-post
   #:files
   #:file-descriptions
   #:video-post
   #:file
   #:link-post
   #:url
   #:text-post
   #:markup
   #:convert-markup
   #:client
   #:name
   #:post-tags
   #:define-client
   #:result
   #:client
   #:post-object
   #:url
   #:profile
   #:name
   #:clients
   #:tags
   #:header
   #:footer
   #:multiposter
   #:schedules
   #:default-profile)
  ;; config.lisp
  (:export
   #:config-file
   #:load-config
   #:save-config)
  ;; toolkit.lisp
  (:export
   #:*image-types*
   #:*video-types*
   #:or*
   #:file-type-p
   #:envvar
   #:parse-tags
   #:merge-paragraphs
   #:initargs
   #:make-like
   #:alphanumeric-p
   #:non-space-p
   #:non-comma-p
   #:filter-tags
   #:trim-text
   #:compose-post-text
   #:query
   #:verbose
   #:timestamp
   #:path-url)
  ;; clients
  (:export
   #:cohost
   #:dummy
   #:file
   #:git
   #:lichat
   #:mastodon
   #:reader
   #:studio
   #:tumblr
   #:webdav))
