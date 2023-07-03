(in-package #:org.shirakumo.multiposter)

(defun not-empty-converter (input)
  (if (string= input "")
      (error "Empty value.")
      input))

(defun prefix-multiline (prefix string)
  (string-trim '(#\Newline)
               (with-output-to-string (out)
                 (with-input-from-string (in string)
                   (loop for line = (read-line in NIL)
                         while line
                         do (write-string prefix out)
                            (write-line line out))))))

(defun prompt (prompt &key (converter #'not-empty-converter) (default NIL default-p) (stream *query-io*))
  (loop
     (format stream "~&~a~:[~; [~a]~]:~%> " (prefix-multiline "| " prompt) default-p default)
     (let ((input (string-right-trim '(#\Newline) (read-line stream))))
       (cond ((and default-p (string= input ""))
              (return default))
             (T
              (handler-case (return (funcall converter input))
                (error (error)
                  (format stream "~&| Error: ~%~a" (prefix-multiline "|   " error))
                  (format stream "~&| Please try again.~%> "))))))))

(defparameter *url-regex*
  (cl-ppcre:create-scanner
   "(?:http(s)?:\\/\\/)?[\\w.-]+(?:\\.[\\w\\.-]+)+[\\w\\-\\._~:/?#[\\]@!\\$&'\\(\\)\\*\\+,;=.]+"))

(defun extract-links (text)
  (let ((pend 0)
        (parts ()))
    (cl-ppcre:do-matches (start end *url-regex* text)
      (push (subseq text pend start) parts)
      (push (list :link (subseq text start end)) parts)
      (setf pend end))
    (push (subseq text pend) parts)
    (nreverse parts)))

(defun limit-text-with-links (text limit link-length &key (cutoff 20))
  (with-output-to-string (out)
    (let ((parts (extract-links text)))
      (loop with length = 0
            for part in parts
            for increase = (if (stringp part) (length part) link-length)
            do (cond ((< (+ length increase) limit)
                      (incf length increase)
                      (write-string (if (stringp part) part (second part)) out))
                     (T
                      (when (stringp part)
                        (let ((space (or (position #\Space part :from-end T :end (- limit length))
                                         (position #\Linefeed part :from-end T :end (- limit length)))))
                          (if (and space (< (- limit length space) cutoff))
                              (write-string part out :end space)
                              (write-string part out :end (- limit length)))))
                      (return)))))))
