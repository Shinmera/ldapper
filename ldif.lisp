(in-package #:org.shirakumo.ldapper)

(defparameter *ignored-attributes*
  '("cn" "dn" "mail" "userPassword" "givenName" "sn" "gecos" "note"
    "objectClass" "structuralObjectClass"))
(defparameter *required-attributes*
  '("cn" "mail" "userPassword"))

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

(defun spread-record (record)
  (loop for (k . v) in record
        append (if (consp v)
                   (loop for value in v collect (cons k value))
                   (list (cons k v)))))

(defun ldif-record->account (record &key (ignored-attributes *ignored-attributes*))
  (let ((record (spread-record record)))
    (flet ((field (name)
             (cdr (assoc name record :test #'string-equal))))
      (list :id NIL
            :name (field "cn")
            :mail (field "mail")
            :password (field "userPassword")
            :real-name (or (when (and (field "givenName") (field "sn"))
                             (format NIL "~@[~a ~]~@[~a~]" (field "givenName") (field "sn")))
                           (field "gecos")
                           (field "displayName")
                           (field "sn")
                           (field "givenName"))
            :note (field "note")
            :classes (loop for (k . v) in record
                           when (or (string-equal k "objectClass")
                                    (string-equal k "structuralObjectClass"))
                           collect v)
            :attributes (loop for (k . v) in record
                              unless (find k ignored-attributes :test #'string-equal)
                              collect (list k v))))))

(defun cn-from-dn (dn)
  (cl-ppcre:register-groups-bind (cn full) ("^cn=([^,]+)|(.*)" dn)
    (or cn full)))

(defun account-dn (account &key (base-dn *base-dn*))
  (format NIL "cn=~a~@[,~a~]" (getf account :name) base-dn))

(defun attribute-key (attribute)
  (cond ((or (string-equal "cn" attribute)
             (string-equal "dn" attribute))
         :name)
        ((string-equal "mail" attribute)
         :mail)
        ((string-equal "userPassword" attribute)
         :password)
        ((or (string-equal "gecos" attribute)
             (string-equal "displayName" attribute)
             (string-equal "sn" attribute)
             (string-equal "givenName" attribute))
         :real-name)
        ((string-equal "note" attribute)
         :note)
        ((or (string-equal "objectClass" attribute)
             (string-equal "structuralObjectClass" attribute))
         :classes)
        (T
         :attributes)))

(defun account->ldif-record (account &key (output NIL) (base-dn *base-dn*))
  (etypecase output
    (null
     (with-output-to-string (stream)
       (account->ldif-record account :output stream)))
    (pathname
     (with-open-file (stream output :direction :output)
       (account->ldif-record account :output stream)))
    (stream
     (let ((account (ensure-account account)))
       (format output "dn: cn=~a~@[,~a~]~%" (getf account :name) base-dn)
       (format output "cn: ~a~%" (getf account :name))
       (dolist (class (getf account :classes))
         (format output "objectClass: ~a~%" class))
       (format output "mail: ~a~%" (getf account :mail))
       (format output "userPassword:: ~a~%" (base64:string-to-base64-string (getf account :password)))
       (format output "displayName: ~a~%" (getf account :real-name))
       (format output "note: ~a~%" (getf account :note))
       (loop for (key val) in (getf account :attributes)
             do (format output "~a: ~a~%" key val))
       (format output "~%")))))

(defun import-from-ldif (input &rest args &key (dry-run T) (required-attributes *required-attributes*) (ignored-attributes *ignored-attributes*))
  (etypecase input
    (string
     (with-input-from-string (stream input)
       (apply #'import-from-ldif stream args)))
    (pathname
     (with-open-file (stream input)
       (apply #'import-from-ldif stream args)))
    (stream
     (connect)
     (with-transaction ()
       (loop for record = (parse-ldif-record input)
             while record
             when (loop for attribute in required-attributes
                        always (assoc attribute record :test #'string-equal))
             collect (let ((account (ldif-record->account record :ignored-attributes ignored-attributes)))
                       (if dry-run
                           account
                           (insert-account account))))))))
