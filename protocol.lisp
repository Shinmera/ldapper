(in-package #:org.shirakumo.ldapper)

(defun check-admin (account)
  (unless (and account (account-admin-p account))
    (error 'permission-denied :name (etypecase account
                                      (null "Anonymous")
                                      (string account)
                                      (cons (getf account :name))))))

(defmethod send ((message message))
  (v:trace :ldapper "~a Sending ~a" (client message) message)
  (let ((stream (socket-stream (client message))))
    (write-sequence (encode-message message) stream)
    (finish-output stream)))

(defmethod reply ((command command) &rest args)
  (send (apply #'make-response command args)))

(defun read-command (stream)
  (assert (= 48 (read-byte stream)))
  (let* ((length (read-ber-length stream))
         (buf (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence buf stream)
    (decode-message buf)))

(defgeneric process-command (command client))

(defmethod process-command :around ((command command) (client client))
  (v:trace :ldapper "~a Processing ~a" client command)
  (setf (client command) client)
  (handler-case (handler-bind (((and error (not ldapper-error))
                                 (lambda (e)
                                   (if *debug*
                                       (invoke-debugger e)
                                       (v:warn :ldapper e)))))
                  (call-next-method))
    (ldapper-error (e)
      (reply command :code (code e) :message (princ-to-string e)))
    (cl-postgres-error:unique-violation (e)
      (reply command :code :entry-already-exists :message (princ-to-string e)))
    (error (e)
      (reply command :code :operations-error :message (princ-to-string e))
      (close client))))

(defmethod process-command ((command bind) (client client))
  (cond ((string/= "" (user command))
         (setf (account client) (authenticate (cn-from-dn (user command)) (pass command)))
         (reply command :domain-name (account-dn (account client))))
        (T
         (reply command))))

(defmethod process-command ((command unbind) (client client))
  (setf (account client) NIL))

(defmethod process-command ((command abandon) (client client))
  (v:info :ldapper "~a: Ignoring abandon command" client))

(defmethod process-command ((command add) (client client))
  (check-admin (account client))
  (let* ((record (list* (cons "cn" (cn-from-dn (domain-name command))) (attributes command)))
         (account (insert-account (ldap-record->account record))))
    (reply command :domain-name (account-dn account))))

(defmethod process-command ((command del) (client client))
  (check-admin (account client))
  (let ((account (delete-account (cn-from-dn (domain-name command)))))
    (reply command :domain-name (account-dn account))))

(defmethod process-command ((command moddn) (client client))
  (check-admin (account client))
  (let ((account (ensure-account (cn-from-dn command)))
        (new-name (cn-from-dn (new-domain-name command))))
    (cond ((delete-old-p command)
           (edit-account account :name new-name))
          (T
           (setf account (insert-account account))))
    (reply command :domain-name (account-dn account))))

(defmethod process-command ((command compare) (client client))
  (let* ((account (ensure-account (cn-from-dn (domain-name command))))
         (key (attribute-key (attribute command)))
         (val (getf account key))
         (result (case key
                   (:attributes
                    (loop for (key val) in val
                          thereis (and (string= key (attribute command))
                                       (string= val (value command)))))
                   (:classes
                    (find (value command) val :test #'string=))
                   (T 
                    (string= (value command) val)))))
    (reply command :code (if result :compare-true :compare-false)
                   :domain-name (account-dn account))))


(defmethod process-command ((command modify) (client client))
  (check-admin (account client))
  (let* ((account (ensure-account (cn-from-dn (domain-name command))))
         (args ())
         (attributes (getf account :attributes)))
    (loop for (type attribute . vals) in (modifications command)
          do (multiple-value-bind (new-attrs new-args) (apply #'update-attributes account type attribute vals)
               (setf attributes new-attrs)
               (setf args (append new-args args))))
    (apply #'edit-account account :attributes attributes args)
    (reply command :domain-name (account-dn account))))

(defmethod process-command ((command lookup) (client client))
  (let ((admin-p (and (account client) (account-admin-p (account client))))
        (attrs (unless (equal '("*") (attributes command)) (attributes command))))
    (flet ((send! (domain record)
             (let ((attrs (ldap-record-filter record attrs)))
               (when attrs (send (make-instance 'lookup-entry :client (client command) :id (id command) :domain-name domain :attributes attrs)))))
           (filter! (filter)
             (dolist (account (filter-accounts filter :limit (when (< 0 (size command)) (size command))))
               (send (make-instance 'lookup-entry :client (client command) :id (id command)
                                                  :domain-name (account-dn account)
                                                  :attributes (account->ldap-record account
                                                                                    :skip-dn T 
                                                                                    :trusted admin-p
                                                                                    :attributes attrs))))))
      (cond
        ;; List root DN
        ((string-equal "" (base command))
         (send! "" `(("objectClass" "top")
                     ("supportedLDAPVersion" "3")
                     ("supportedSASLMechanisms")
                     ("supportedExtension" ,@(alexandria:hash-table-keys *extended-oid-map*))
                     ("supportedControl" ,+ldap-control-extension-paging+)
                     ("supportedFeatures")
                     ("namingContexts")
                     ;;("subschemaSubentry" "")
                     ("vendorName" "ldapper")
                     ("vendorVersion" #.(asdf:component-version (asdf:find-system :ldapper)))
                     ("hasSubordinates" "TRUE")))
         (if *base-dn*
             (send! *base-dn*
                    `(("objectClass" "dcObject")
                      ("dc" ,(second (first (parse-dn *base-dn*))))
                      ("hasSubordinates" "TRUE")))
             (filter! (filter command))))
        ;; List all accounts
        ((or (null *base-dn*) (string-equal *base-dn* (base command)))
         (filter! (filter command)))
        ;; Search for particular account
        ((search *base-dn* (base command))
         (filter! `(:and (:= ,@(first (parse-dn (base command)))) ,(filter command))))
        ;; Support for listing dcObjects along the base DN
        (T
         (let ((s-parts (parse-dn (base command)))
               (d-parts (parse-dn *base-dn*)))
           (loop for s = (pop s-parts)
                 for d = (pop d-parts)
                 while (and s d)
                 do (unless (equalp s d)
                      (return))
                 finally (when d-parts
                           (send! (format NIL "~a,~{~a=~a~}" (base command) d-parts)
                                  `(("objectClass" "dcObject")
                                    ("dc" ,(first d-parts))
                                    ("hasSubordinates" "TRUE"))))))))))
  (reply command))

(defmethod process-command ((command extended) (client client))
  (error 'unknown-command :oid (oid command)))

(defmethod process-command ((command password-change) (client client))
  ;; Let users change their own passwords
  (when (or (null (account client))
            (not (string-equal (cn-from-dn (user command)) (getf (account client) :name))))
    (check-admin (account client)))
  (let ((account (authenticate (cn-from-dn (user command)) (pass command)))
        (pass (or (new-pass command) (generate-password))))
    (edit-account account :password pass)
    (reply command :domain-name (account-dn account)
                   :value (unless (new-pass command) pass))))

(defun generate-password (&optional (length 24))
  (let ((str (make-string length))
        (chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (dotimes (i length str)
      (setf (char str i) (aref chars (random (length chars)))))))

(defmethod process-command ((command starttls) (client client))
  (let ((context (loop for listener in *listeners*
                       thereis (context listener))))
    (unless context
      (error "Incapable of SSL"))
    (reply command)
    (change-class client 'ssl-client :context context)))

(defmethod process-command ((command starttls) (client ssl-client))
  (error "Already in SSL"))
