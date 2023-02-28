(in-package #:org.shirakumo.ldapper)

(defun cn-from-dn (dn)
  (cl-ppcre:register-groups-bind (cn full) ("^cn=([^,]+)|(.*)" dn)
    (or cn full)))

(defun read-command (stream)
  )

(defgeneric process-command (command client))

(defmethod process-command ((command bind) (client client))
  (setf (account client) (authenticate (cn-from-dn (user command)) (pass command))))

(defmethod process-command ((command unbind) (client client))
  (setf (account client) NIL))

(defmethod process-command ((command abandon) (client client))
  )

(defmethod process-command ((command add) (client client))
  (let* ((record (list* (cons "cn" (cn-from-dn (domain-name command))) (attributes command)))
         (account (insert-account (ldif-record->account record))))
    ))

(defmethod process-command ((command del) (client client))
  (let ((account (delete-account (cn-from-dn (domain-name del)))))
    ))

(defmethod process-command ((command moddn) (client client))
  (let ((account (ensure-account (cn-from-dn command)))
        (new-name (cn-from-dn (new-domain-name command))))
    (cond ((delete-old-p command)
           (edit-account account :name new-name))
          (T
           (setf account (insert-account account))))
    ))

(defmethod process-command ((command compare) (client client))
  (let ((account (ensure-account (cn-from-dn (domain-name command)))))
    ))

(defmethod process-command ((command modify) (client client))
  (let ((account (ensure-account (cn-from-dn (domain-name command)))))
    (loop for (type key . vals) in (modifications command)
          do (ecase type
               (:add
                )
               (:delete
                )
               (:replace
                )))))

(defmethod process-command ((command lookup) (client client))
  )
