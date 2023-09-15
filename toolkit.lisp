(in-package #:org.shirakumo.multiposter)

(defvar *image-types* '("png" "jpg" "jpeg" "gif" "bmp" "svg"))
(defvar *video-types* '("gifv" "apng" "mp4" "webm" "mov" "mkv"))
(defvar *text-types* '("txt" "org" "md" "mess" "bb" "html" "htm"))

(defmacro or* (&rest vals)
  (let ((arg (gensym "ARG")))
    `(or ,@(loop for val in vals
                 collect `(let ((,arg ,val))
                            (if (stringp ,arg)
                                (unless (string= ,arg "") ,arg)
                                ,arg))))))

(defmacro with-integers-bound (vars (regex string) &body body)
  `(cl-ppcre:register-groups-bind ,vars (,regex ,string)
     (let ,(loop for var in vars collect `(,var (when ,var (parse-integer ,var))))
       ,@body)))

(defun parse-timestring (string)
  (or
   ;; Single integer relative or absolute unix time
   (with-integers-bound (stamp) ("^(\\d+)$" string)
     (if (< stamp (* 60 60 24 7 365 50))
         (+ stamp (get-universal-time))
         (+ stamp (encode-universal-time 0 0 0 1 1 1970 NIL))))
   ;; Relative d/m/h/s specification
   (with-integers-bound (d h m s) ("^in(:? +(:?(\\d+) *(:?d|days?)))?(:?,? +(:?(\\d+) *(:?h|hours?)))?(:?,? +(:?(\\d+) *(:?m|mins?|minutes?)))?(:?,? +(:?(\\d+) *(:?s|sec|seconds?)))?$" string)
     (+ (get-universal-time)
        (* 60 60 24 (or d 0))
        (* 60 60 (or h 0))
        (* 60 (or m 0))
        (* 1 (or s 0))))
   ;; Date/Time spec. Omitted values are computed based on current time
   (with-integers-bound (d o y h m s) ("^(:?(\\d+)[./-](\\d+)(:?[./-](\\d+))?[Tt\\- ]+)?(\\d+):(\\d+)(:?:(\\d+))?$" string)
     (multiple-value-bind (ls lm lh ld lo ly) (decode-universal-time (get-universal-time))
       (let ((time (encode-universal-time (or s ls) (or m lm) (or h lh) (or d ld) (or o lo) (or y ly))))
         (when (and y (< 100 y))
           (incf y 2000))
         ;; If the hour is in the past, assume we want to post on the next day instead.
         (when (and h (< lh h) (= ld (or d ld)) (= lo (or o lo)) (= ly (or y ly)))
           (incf time (* 60 60 24)))
         time)))
   ;; Same as before but reversed order
   (with-integers-bound (h m s d o y) ("^(:?(\\d+):(\\d+)(:?:(\\d+))?[Tt\\- ]+)?(\\d+)[./-](\\d+)(:?[./-](\\d+))?$" string)
     (multiple-value-bind (ls lm lh ld lo ly) (decode-universal-time (get-universal-time))
       (let ((time (encode-universal-time (or s ls) (or m lm) (or h lh) (or d ld) (or o lo) (or y ly))))
         (when (and y (< 100 y))
           (incf y 2000))
         ;; If the hour is in the past, assume we want to post on the next day instead.
         (when (and h (< lh h) (= ld (or d ld)) (= lo (or o lo)) (= ly (or y ly)))
           (incf time (* 60 60 24)))
         time)))
   ;; Welp.
   (error "Don't know how to parse ~a
Valid formats include:

  2                 (meaning in 2 seconds)
  1694799940        (meaning at this unix time)
  in 10 seconds
  in 6d,5m
  10:00             (meaning at 10:00AM today or tomorrow, if already in the past)
  10:00 18.11.2023
  18.11.2023        (meaning at the current time on that date)
  18/11/23 20:00
  18-11-2023T20:00:24
" string)))

(defun file-type-p (thing types)
  (let ((thing (etypecase thing
                 (pathname (file-namestring thing))
                 (string thing))))
    (loop for type in types
          thereis (and (< (1+ (length type)) (length thing))
                       (string= thing type :start1 (- (length thing) (length type)))
                       (char= #\. (char thing (- (length thing) 1 (length type))))))))

(defun envvar (var)
  (let ((val (uiop:getenv var)))
    (when (and val (string/= "" val))
      val)))

(defun parse-tags (tags)
  (loop for tag-ish in (enlist tags)
        when (or* tag-ish)
        append (cl-ppcre:split " *,+ *" tag-ish)))

(defun merge-paragraphs (&rest paragraphs)
  (with-output-to-string (out)
    (let ((filtered (loop for string in paragraphs
                          when (and string (string/= "" string))
                          collect string)))
      (when filtered
        (loop for (string . rest) on filtered
              do (write-string string out)
                 (when (or* (car rest))
                   (format out "~&~%")))))))

(defun enlist (list &rest args)
  (if (consp list)
      list
      (list* list args)))

(defgeneric initargs (thing)
  (:method-combination append :most-specific-first))

(defgeneric make-like (thing &rest initargs))

(defmethod make-like ((thing number) &rest initargs)
  (declare (ignore initargs))
  thing)

(defmethod make-like ((thing standard-object) &rest initargs)
  (let ((copy (allocate-instance (class-of thing))))
    (loop for slot in (c2mop:class-slots (class-of thing))
          do (setf (slot-value copy (c2mop:slot-definition-name slot))
                   (slot-value thing (c2mop:slot-definition-name slot))))
    (apply #'shared-initialize copy () initargs)))

(defun alphanumeric-p (char)
  (or (<= (char-code #\a) (char-code char) (char-code #\z))
      (<= (char-code #\A) (char-code char) (char-code #\Z))
      (<= (char-code #\0) (char-code char) (char-code #\9))))

(defun non-space-p (char)
  (char/= char #\Space))

(defun non-comma-p (char)
  (char/= char #\,))

(defun filter-tags (tags &optional (allowed-char-fun #'alphanumeric-p))
  (loop for tag in tags
        for filtered = (remove-if-not allowed-char-fun tag)
        unless (string= "" filtered) collect filtered))

(defun limit (seq limit)
  (if (<= (length seq) limit)
      seq
      (subseq seq 0 limit)))

(defun trim-text (text char-limit)
  (cond ((<= (length text) char-limit)
         text)
        ((<= char-limit 3)
         NIL)
        ((<= char-limit 6)
         (subseq text 0 char-limit))
        (T
         (format NIL "~a..." (subseq text 0 (- char-limit 3))))))

(defun compose-post-text (header body footer &key tags (tag-format "#~a") (tag-separator " ") char-limit)
  ;; Compose with the following precedence:
  ;; 1. tags (drop whole tags only)
  ;; 2. header
  ;; 3. footer
  ;; 4. body
  (let* ((char-limit (or char-limit most-positive-fixnum))
         (tags (loop for tag in tags collect (format NIL tag-format tag)))
         (tag-length (+ (loop for tag in tags sum (length tag))
                        (* (length tag-separator) (length tags))))
         (total-length (+ (if header (length header) 0)
                          (if footer (length footer) 0)
                          (if body (length body) 0)
                          tag-length
                          (* 2 (max 0 (1- (+ (if header 1 0) (if footer 1 0) (if body 1 0))))))))
    (when (< char-limit total-length)
      ;; First trim the tags to fit
      (when (< char-limit tag-length)
        (setf tags (nreverse tags))
        (loop while (< char-limit tag-length)
              for eliminated = (pop tags)
              do (decf tag-length (+ eliminated 1)))
        (setf tags (nreverse tags)))
      (decf char-limit tag-length)
      ;; Next trim the header
      (when (and header (< char-limit (length header)))
        (setf header (trim-text header char-limit))
        (setf body NIL)
        (setf footer NIL))
      (when header (decf char-limit (length header)))
      ;; Next trim the footer
      (when (and footer (< char-limit (length footer)))
        (setf footer (trim-text footer char-limit))
        (setf body NIL))
      (when footer 
        (decf char-limit (length footer))
        (when header (decf char-limit 2)))
      ;; Finally trim the body
      (when body
        (when (or header footer) 
          (decf char-limit 2))
        (when (< char-limit (length body))
          (setf body (trim-text body char-limit)))))
    (merge-paragraphs header body footer (format NIL (format NIL "~~{~~a~~^~a~~}" tag-separator) tags))))

(defun query (prompt &key nullable default coerce check)
  (format *query-io* "~&> ~a~@[ [~a]~]~%" prompt (or default (when nullable "NIL")))
  (let ((coerce (or coerce #'identity))
        (check (or check (constantly T))))
    (loop for input = (or* (read-line *query-io*))
          do (cond (input
                    (handler-case (let ((value (funcall coerce input)))
                                    (if (funcall check value)
                                        (return value)
                                        (error "")))
                      (error ()
                        (format *query-io* "~&Please enter a valid value.~%"))))
                   (default
                    (return default))
                   (nullable
                    (return NIL))
                   (T
                    (format *query-io* "~&Please enter a value.~%"))))))

(defun verbose (format &rest args)
  (format *error-output* "~&; ~?~%" format args))

(defun timestamp (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time universal-time)
    (format NIL "~4,'0d.~2,'0d.~2,'0d ~2,'0d-~2,'0d-~2,'0d"
            yy mm dd h m s)))

(defun path-url (path)
  (format NIL "file://~a" (pathname-utils:native-namestring path)))

(defun file-size (file)
  (etypecase file
    (stream (file-length file))
    (pathname (with-open-file (stream file :element-type '(unsigned-byte 8))
                (file-length stream)))))

(defun make-random-string (&optional (length 8))
  (with-output-to-string (out)
    (dotimes (i length)
      (write-char (alexandria:random-elt "abcdefghikmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") out))))
