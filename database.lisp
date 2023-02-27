(in-package #:org.shirakumo.ldapper)

(defvar *postgres-db* "ldap")
(defvar *postgres-user* NIL)
(defvar *postgres-pass* NIL)
(defvar *postgres-host* "127.0.0.1")
(defvar *transaction* NIL)

(defmacro with-transaction (args &body body)
  `(flet ((thunk () ,@body))
     (if *transaction*
         (thunk)
         (postmodern:with-transaction ,args
           (let ((*transaction* T))
             (thunk))))))

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
      (nconc account
             (list :classes (postmodern:query (:order-by (:select 'class :from 'classes :where (:= 'account id)) 'class) :column)
                   :attributes (postmodern:query (:order-by (:select 'key 'value :from 'attributes :where (:= 'account id)) 'key)))))))

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
    (if (crypto-shortcuts:check-rfc-2307-hash password (getf account :password))
        account
        (error "Bad password."))))

(defun make-account (name mail password &key real-name note classes attributes already-hashed)
  (connect)
  (with-transaction ()
    (let ((id (postmodern:query (:insert-into 'accounts :set
                                              'name name
                                              'mail mail
                                              'password (if already-hashed password (cryptos:rfc-2307-hash password))
                                              'real-name real-name
                                              'note (or note "")
                                              :returning 'id)
                                :single)))
      (edit-account id :classes classes :attributes attributes)
      (find-account id))))

(defun insert-account (account)
  (make-account (getf account :name) (getf account :mail) (getf account :password)
                :real-name (getf account :real-name)
                :note (getf account :note)
                :classes (getf account :classes)
                :attributes (getf account :attributes)
                :already-hashed T))

(defun edit-account (account &key mail real-name note password already-hashed (classes NIL classes-p) (attributes NIL attributes-p))
  (connect)
  (with-transaction ()
    (let* ((account (ensure-account account))
           (id (getf account :id)))
      (flet ((update (field value)
               (postmodern:query (:update 'accounts :set field value :where (:= 'id id)))
               (setf (getf account field) value)))
        (when mail (update 'mail mail))
        (when real-name (update 'real-name real-name))
        (when note (update 'note note))
        (when password (update 'password (if already-hashed password (cryptos:rfc-2307-hash password)))))
      (when classes-p
        (postmodern:query (:delete-from 'classes :where (:= 'account id)))
        (postmodern:query (:insert-rows-into 'classes :columns 'account 'class
                                             :values (loop for class in classes
                                                           collect (list id class))))
        (setf (getf account :classes) classes))
      (when attributes-p
        (postmodern:query (:delete-from 'attributes :where (:= 'account id)))
        (postmodern:query (:insert-rows-into 'attributes :columns 'account 'key 'value
                           :values (loop for (key val) in attributes
                                         collect (list id key val))))
        (setf (getf account :attributes) attributes))
      account)))

(defun delete-account (account)
  (connect)
  (with-transaction ()
    (let* ((account (ensure-account account))
           (id (getf account :id)))
      (postmodern:query (:delete-from 'classes :where (:= 'account id)))
      (postmodern:query (:delete-from 'attributes :where (:= 'account id)))
      (postmodern:query (:delete-from 'accounts :where (:= 'id id)))
      account)))


