(in-package #:org.shirakumo.ldapper)

(defvar *postgres-db* "ldap")
(defvar *postgres-user* "ldapper")
(defvar *postgres-pass* NIL)
(defvar *postgres-host* "127.0.0.1")

(defun connect ()
  (unless (and postmodern:*database* (postmodern:connected-p postmodern:*database*))
    (postmodern:connect-toplevel *postgres-db* *postgres-user* *postgres-pass* *postgres-host*)))

(defun disconnect ()
  (when (and postmodern:*database* (postmodern:connected-p postmodern:*database*))
    (postmodern:disconnect-toplevel)))

(defun init-database ()
  (connect)
  (let ((tables (postmodern:list-all-tables)))
    (unless (find "accounts" tables :test #'string= :key #'second)
      (postmodern:query (:create-table 'accounts
                                       ((id :type integer :primary-key T :identity-always T)
                                        (name :type (varchar 64) :unique T)
                                        (mail :type (varchar 64))
                                        (password :type (varchar 256))
                                        (real-name :type (varchar 64))
                                        (note :type text)))))
    (unless (find "classes" tables :test #'string= :key #'second)
      (postmodern:query (:create-table 'classes
                                       ((account :type integer :references ((accounts id)))
                                        (class :type (varchar 64))))))
    (unless (find "attributes" tables :test #'string= :key #'second)
      (postmodern:query (:create-table 'attributes
                                       ((account :type integer :references ((accounts id)))
                                        (key :type (varchar 64))
                                        (value :type text)))))))

(defun list-accounts ()
  (connect)
  (postmodern:query (:order-by (:select '* :from 'accounts) 'name) :plists))

(defun find-account (name)
  (connect)
  (let* ((account (etypecase name
                    (integer (postmodern:query (:select '* :from 'accounts :where (:= 'id name)) :plist))
                    (string (postmodern:query (:select '* :from 'accounts :where (:= 'name name)) :plist))))
         (id (getf account :id)))
    (when id
      (setf (getf account :classes) (postmodern:query (:order-by (:select 'class :from 'accounts :where (:= 'account id)) 'class)))
      (setf (getf account :attributes) (postmodern:query (:order-by (:select 'key 'value :from 'accounts :where (:= 'account id)) 'key))))
    account))

(defun ensure-account (account-ish)
  (etypecase account-ish
    (cons
     account-ish)
    ((or string integer)
     (or (find-account account-ish)
         (error "No account with name ~s found" account-ish)))))

(defun authenticate (account password)
  (connect)
  (let ((account (ensure-account account)))
    (check-password password (getf account :password))))

(defun make-account (name mail password &key real-name note classes attributes already-hashed)
  (connect)
  (postmodern:with-transaction ()
    (let ((account (postmodern:query (:insert-into 'accounts :set
                                                   'name name
                                                   'mail mail
                                                   'password (if already-hashed password (hash password))
                                                   'real-name real-name
                                                   'note note)
                                     :plist)))
      (edit-account account :classes classes :attributes attributes)
      (find-account name))))

(defun insert-account (account)
  (make-account (getf account :name) (getf account :mail) (getf account :password)
                :real-name (getf account :real-name)
                :note (getf account :note)
                :classes (getf account :classes)
                :attributes (getf account :attributes)
                :already-hashed T))

(defun edit-account (account &key mail real-name note password already-hashed (classes NIL classes-p) (attributes NIL attributes-p))
  (connect)
  (postmodern:with-transaction ()
    (let* ((account (ensure-account account))
           (id (getf account :id)))
      (flet ((update (field value)
               (postmodern:query (:update 'accounts :set field value :where (:= 'id id)))
               (setf (getf account field) value)))
        (when mail (update 'mail mail))
        (when real-name (update 'real-name real-name))
        (when note (update 'note note))
        (when password (update 'password (if already-hashed password (hash password)))))
      (when classes-p
        (postmodern:query (:delete-from 'classes :where (:= 'account id)))
        (postmodern:query (:insert-into 'classes :columns 'account 'class
                                        :values (loop for class in classes
                                                      collect (list id class))))
        (setf (getf account :classes) classes))
      (when attributes-p
        (postmodern:query (:delete-from 'attributes :where (:= 'account id)))
        (postmodern:query (:insert-into 'attributes :columns 'account 'key 'value
                           :values (loop for (key val) in attributes
                                         collect (list id key val))))
        (setf (getf account :attributes) attributes))
      account)))

(defun delete-account (account)
  (connect)
  (postmodern:with-transaction ()
    (let* ((account (ensure-account account))
           (id (getf account :id)))
      (postmodern:query (:delete-from 'classes :where (:= 'account id)))
      (postmodern:query (:delete-from 'attributes :where (:= 'account id)))
      (postmodern:query (:delete-from 'accounts :where (:= 'id id)))
      account)))


