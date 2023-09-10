(in-package #:org.shirakumo.multiposter)

(define-client dummy (client)
  ((char-limit :initarg :char-limit :initform NIL :accessor char-limit)))

(defmethod initargs append ((client dummy))
  (list :char-limit (char-limit client)))

(defmethod post ((post post) (client dummy) &key)
  (format *standard-output* "~a" (compose-post-text post :char-limit (char-limit client)))
  NIL)

(defmethod ready-p ((client dummy)) T)

(defmethod setup ((client dummy) &rest args &key char-limit)
  (cond ((null args)
         (setf (char-limit client) (query "Enter the post length limit" :nullable T :coerce #'parse-integer)))
        (T
         (setf (char-limit client) char-limit))))
