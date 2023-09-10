(in-package #:org.shirakumo.multiposter)

(defmacro or* (&rest vals)
  (let ((arg (gensym "ARG")))
    `(or ,@(loop for val in vals
                 collect `(let ((,arg ,val))
                            (if (stringp ,arg)
                                (unless (string= ,arg "") ,arg)
                                ,arg))))))

(defun merge-paragraphs (&rest paragraphs)
  (with-output-to-string (out)
    (let ((filtered (loop for string in paragraphs
                          when (and string (string/= "" string))
                          collect string)))
      (loop for (string . rest) on filtered
            do (write-string string out)
               (when (or* (car rest))
                 (format out "~&~%"))))))

(defun enlist (list &rest args)
  (if (consp list)
      list
      (list* list args)))
