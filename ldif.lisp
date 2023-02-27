(in-package #:org.shirakumo.ldapper)

(defparameter *ignored-attributes*
  '("cn" "dn" "mail" "userPassword" "givenName" "sn" "gecos" "note"
    "objectClass" "structuralObjectClass"))

(defun parse-ldif-record (input)
  (etypecase input
    (string
     (with-input-from-string (stream input)
       (parse-ldif-record stream)))
    (stream
     (let ((buffer (make-string-output-stream))
           (record ()))
       (labels ((trim (string)
                  (string-trim '(#\Linefeed #\Return #\Space #\Tab) string))
                (commit ()
                  (let* ((line (get-output-stream-string buffer))
                         (col (position #\: line)))
                    (cond ((string= "" line))
                          ((and col (< (1+ col) (length line)))
                           (let* ((key (subseq line 0 col))
                                  (base64-p (char= #\: (char line (1+ col))))
                                  (val (if base64-p
                                           (cl-base64:base64-string-to-string (trim (subseq line (+ 2 col))))
                                           (trim (subseq line (+ 1 col))))))
                             (push (cons key val) record)))
                          (T
                           (warn "Ignoring weird line: ~a" line))))))
         (loop for line = (read-line input NIL NIL)
               while (and line (string/= "" line))
               do (when (char/= #\Space (char line 0))
                    (commit))
                  (write-string line buffer)
               finally (commit))
         record)))))

(defun parse-ldif (input)
  (etypecase input
    (string
     (with-input-from-string (stream input)
       (parse-ldif stream)))
    (pathname
     (with-open-file (stream input)
       (parse-ldif stream)))
    (stream
     (loop for record = (parse-ldif-record input)
           while record collect record))))

(defun ldif-record->account (record &key (ignored-attributes *ignored-attributes*))
  (flet ((field (name)
           (cdr (assoc name record :test #'string-equal))))
    (list :name (field "cn")
          :mail (field "mail")
          :password (field "userPassword")
          :real-name (or (field "gecos") (format NIL "~@[~a ~]~@[~a~]" (field "givenName") (field "sn")))
          :note (field "note")
          :classes (loop for (k . v) in record
                         when (or (string-equal k "objectClass")
                                  (string-equal k "structuralObjectClass"))
                         collect v)
          :attributes (loop for (k . v) in record
                            unless (find k ignored-attributes :test #'string-equal)
                            collect (list k v)))))

(defun import-from-ldif (input)
  (etypecase input
    (string
     (with-input-from-string (stream input)
       (parse-ldif stream)))
    (pathname
     (with-open-file (stream input)
       (parse-ldif stream)))
    (stream
     (connect)
     (postmodern:with-transaction ()
       (loop for record = (parse-ldif-record input)
             while record
             collect (insert-account (ldif-record->account record)))))))
