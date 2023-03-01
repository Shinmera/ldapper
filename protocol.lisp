(in-package #:org.shirakumo.ldapper)

(defmethod send ((message message))
  (let ((stream (socket-stream (client message))))
    (write-sequence (encode-message message) stream)))

(defmethod reply ((command command) &rest args)
  (send (apply #'make-response command args)))

(defun read-command (stream)
  (let* ((length (read-ber-length stream))
         (buf (make-array length :element-type '(unsigned-byte 8))))
    (decode-message (read-sequence buf stream))))

(defgeneric process-command (command client))

(defmethod process-command :around ((command command) (client client))
  (handler-case (handler-bind ((error (lambda (e) (v:warn :ldapper e))))
                  (call-next-method))
    (no-such-account (e)
      (reply command :code :no-such-object :message (princ-to-string e)))
    (authentication-failed (e)
      (reply command :code :invalid-credentials :message (princ-to-string e)))
    (permission-denied (e)
      (reply command :code :insufficient-access-rights :message (princ-to-string e)))
    (attribute-required (e)
      (reply command :code :constraint-violation :message (princ-to-string e)))
    (error ()
      (reply command :code :operations-error)
      (close client))))

(defmethod process-command ((command bind) (client client))
  (setf (account client) (authenticate (cn-from-dn (user command)) (pass command)))
  (reply command :domain-name (account-dn (account client))))

(defmethod process-command ((command unbind) (client client))
  (setf (account client) NIL))

(defmethod process-command ((command abandon) (client client))
  (v:info :ldapper "~a: Ignoring abandon command" client))

(defmethod process-command ((command add) (client client))
  (let* ((record (list* (cons "cn" (cn-from-dn (domain-name command))) (attributes command)))
         (account (insert-account (ldif-record->account record))))
    (reply command :domain-name (account-dn account))))

(defmethod process-command ((command del) (client client))
  (let ((account (delete-account (cn-from-dn (domain-name command)))))
    (reply command :domain-name (account-dn account))))

(defmethod process-command ((command moddn) (client client))
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
  (let* ((account (ensure-account (cn-from-dn (domain-name command))))
         (args ())
         (attributes (getf account :attributes)))
    (flet ((filter-attributes (key)
             (setf attributes (loop for entry in attributes
                                    unless (string-equal (first entry) key)
                                    collect entry))))
      (loop for (type attribute . vals) in (modifications command)
            for key = (attribute-key attribute)
            do (ecase key
                 (:name
                  (ecase type
                    (:replace
                     (setf (getf key args) (cn-from-dn (or (first vals) (error 'attribute-required :attribute "cn")))))))
                 (:mail
                  (ecase type
                    (:replace
                     (setf (getf key args) (or (first vals) (error 'attribute-required :attribute "mail"))))))
                 ((:note :real-name)
                  (ecase type
                    (:add
                     (setf (getf key args) (or (first vals) "")))
                    (:replace
                     (setf (getf key args) (or (first vals) "")))
                    (:delete
                     (when (or (null vals) (find (getf key args) vals :test #'string=))
                       (setf (getf key args) "")))))
                 (:classes
                  (ecase type
                    (:add
                     (setf (getf key args) (append (getf key args) vals)))
                    (:replace
                     (setf (getf key args) vals))
                    (:delete
                     (setf (getf key args) (when vals (set-difference (getf key args) vals :test #'string-equal))))))
                 (:attributes
                  (ecase type
                    (:add
                     (dolist (val vals)
                       (push (list attribute val) attributes)))
                    (:replace
                     (filter-attributes attribute)
                     (dolist (val vals)
                       (push (list attribute val) attributes)))
                    (:delete
                     (if vals
                         (setf attributes (loop for entry in attributes
                                                unless (and (string-equal (first entry) key)
                                                            (find (second entry) vals :test #'string=))
                                                collect entry))
                         (filter-attributes attribute))))))))
    (apply #'edit-account account :attributes attributes args)
    (reply command :domain-name (account-dn account))))

(defmethod process-command ((command lookup) (client client))
  (send client))

(defmethod process-command ((command extended) (client client))
  (error "Unknown command"))

(defmethod process-command ((command password-change) (client client))
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
