(dolist (sub '(:multiposter-tumblr
               :multiposter-twitter
               :multiposter-mastodon
               :multiposter-git))
  (asdf:load-system sub))
