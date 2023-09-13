(in-package #:org.shirakumo.multiposter)

(define-client pixiv (client)
  ((cookie-jar :initform (make-instance 'drakma:cookie-jar) :reader cookie-jar)))

(defmethod shared-initialize :after ((client pixiv) slots &key cookies)
  (loop for (k . v) in cookies
        do (push (make-instance 'drakma:cookie :domain "www.pixiv.net" :path "/" :name k :value v)
                 (drakma:cookie-jar-cookies (cookie-jar client)))))

(defmethod initargs append ((client pixiv))
  (list :cookies (loop for cookie in (drakma:cookie-jar-cookies (cookie-jar client))
                       collect (cons (drakma:cookie-name cookie) (drakma:cookie-value cookie)))))

(defun pixiv-request (client method url &optional parameters &rest args)
  (apply #'drakma:http-request
         (format NIL "https://www.pixiv.net~a" url)
         :method method
         :cookie-jar (cookie-jar client)
         :parameters parameters
         :user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/116.0.0.0 Safari/537.36"
         :additional-headers `(("Accept-Language" . "en-GB,en-US;q=0.9,en;q=0.8,de;q=0.7")
                               ("Cache-Control" . "max-age=0")
                               ("Dnt" . "1")
                               ("Set-Ch-Ua" . "\"Chromium\";v=\"86\", \"\"Not\\A;Brand\";v=\"99\", \"Google Chrome\";v=\"86\"")
                               ("Set-Ch-Ua-Mobile" . "?0")
                               ("Set-Ch-Ua-Platform" . "Linux")
                               ("Sec-Fetch-Dest" . "document")
                               ("Sec-Fetch-Mode" . "cors")
                               ("Sec-Fetch-Site" . "same-origin")
                               ,@(getf args :additional-headers))
         args))

(defmethod post ((post image-post) (client pixiv) &key verbose)
  (when verbose (verbose "Posting image to pixiv..."))
  (multiple-value-bind (stream status headers url)
      (pixiv-request client :post "/upload.php"
                     `(("uptype" . "illust")
                       ("x_restrict_sexual" . ,(if (content-warning post) "1" "0"))
                       ("sexual" . "")
                       ("title" . ,(limit (title post) 32))
                       ("tag" . ,(format NIL "~{~a~^ ~}" (limit (tags post) 10)))
                       ("comment" . ,(description post))
                       ("rating" . "1")
                       ("mode" . "upload")
                       ("suggested_tags" . "")
                       ("book_style" . "0")
                       ("restrict" . "0")
                       ("quality[]" . "")
                       ("quality_text" . "")
                       ("qropen" . "")
                       ("ai_type" . "1")
                       ("original" . "on")
                       ,@(loop for file in (files post)
                               collect `("files[]" . ,file))
                       ,@(loop for file in (files post)
                               collect `("file_info[]" . ,(format NIL "{\"type\":~s,\"name\":~s,\"size\":~a}"
                                                                  (trivial-mimes:mime file) (file-namestring file) (file-size file)))))
                     :additional-headers '(("Origin" . "https://www.pixiv.net")
                                           ("Referer" . "https://www.pixiv.net/upload.php")
                                           ("X-Requested-With" . "XMLHttpRequest"))
                     :accept "application/json, text/javascript, */*; q=0.01"
                     :want-stream T)
    (let ((content-type (cdr (assoc "Content-Type" headers :test #'string-equal))))
      (unwind-protect
           (cond ((<= 400 status)
                  (error "Submission failed!"))
                 ((or (search "application/json" content-type)
                      (search "text/javascript" content-type))
                  (let ((error (gethash "error" (yason:parse stream))))
                    (when error (error "Submission failed: ~a" error))
                    url))
                 ((search "text/html" content-type)
                  (error "Ugh."))
                 (T
                  (error "?")))
        (close stream)))))

(defmethod ready-p ((client pixiv))
  (let ((response (ignore-errors (pixiv-request client :get "/en/"))))
    (when (and response (null (search "signup-form" response)))
      client)))

(defmethod setup ((client pixiv) &rest args)
  (cond ((and (null args) (= 0 (length (drakma:cookie-jar-cookies (cookie-jar client)))))
         (let* ((raw (query "Please visit https://www.pixiv.net, log in, and then run the following Javascript code on the page:
  console.log(document.cookie)
This should print the cookie jar into the console. Copy it, and paste it here."))
                (cookies (loop for cookie in (cl-ppcre:split " *; *" raw)
                               collect (cl-ppcre:register-groups-bind (key val) ("(.*?)=(.*)" cookie)
                                         (cons key val)))))
           (reinitialize-instance client :cookies cookies)))
        (T
         (apply #'reinitialize-instance client args))))
