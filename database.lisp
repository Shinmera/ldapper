#|
 This file is a part of ldapper
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
    (v:info :ldapper "Connecting to ~a/~a" *postgres-host* *postgres-db*)
    (postmodern:connect-toplevel *postgres-db* *postgres-user* *postgres-pass* *postgres-host*)))

(defun disconnect ()
  (when (and postmodern:*database* (postmodern:connected-p postmodern:*database*))
    (v:info :ldapper "Disconnecting")
    (postmodern:disconnect-toplevel)))

(defun init-database ()
  (connect)
  (let ((tables (postmodern:list-all-tables)))
    (unless (find "accounts" tables :test #'string= :key #'second)
      (v:info :ldapper "Setting up database")
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
                                        (value :type text)))))
    (unless (find "admins" tables :test #'string= :key #'second)
      (postmodern:query (:create-table 'admins
                                       ((account :type integer :references ((accounts id)) :unique T)))))))

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
         (error 'no-such-account :name account-ish)))))

(defun account-admin-p (account)
  (etypecase account
    (integer (postmodern:query (:select '* :from 'admins :where (:= 'account account)) :single))
    (string (account-admin-p (ensure-account account)))
    (cons (postmodern:query (:select '* :from 'admins :where (:= 'account (getf account :id))) :single))))

(defun (setf account-admin-p) (admin-p account)
  (let ((account (ensure-account account)))
    (cond ((and admin-p (not (account-admin-p account)))
           (postmodern:query (:insert-into 'admins :set 'account (getf account :id))))
          ((and (not admin-p) (account-admin-p account))
           (postmodern:query (:delete-from 'admins :where (:= 'account (getf account :id))))))
    admin-p))

(defun search-accounts (attribute value &key full)
  (macrolet ((query (&rest query)
               `(if full
                    (mapcar #'find-account (postmodern:query (:select 'id :from 'accounts ,@query) :column))
                    (postmodern:query (:select 'id 'name 'mail 'password 'real-name 'note :from 'accounts ,@query) :plists))))
    (cond ((string-equal attribute "dn")
           (cl-ppcre:register-groups-bind (cn) ("cn=([^,]+)" value)
             (query :where (:= 'name cn))))
          ((string-equal attribute "cn")
           (query :where (:= 'name value)))
          ((string-equal attribute "mail")
           (query :where (:= 'mail value)))
          ((string-equal attribute "note")
           (query :where (:= 'note value)))
          ((or (string-equal attribute "gecos")
               (string-equal attribute "displayName")
               (string-equal attribute "sn")
               (string-equal attribute "givenName"))
           (query :where (:= 'real-name value)))
          ((or (string-equal attribute "objectClass")
               (string-equal attribute "structuralObjectClass"))
           (query :inner-join 'classes :on (:= 'id 'account) :where (:ilike 'class value) :group-by 'id))
          (T
           (query :inner-join 'attributes :on (:= 'id 'account) :where (:and (:ilike 'key attribute) (:ilike 'value (or value "%"))) :group-by 'id)))))

(defun %filter-to-sql (filter stream)
  (flet ((escape (string)
           (write-char #\' stream)
           (loop for char across string
                 do (case char
                      ((#\' #\\) (write-char #\\ stream)))
                    (write-char char stream))
           (write-char #\' stream)))
    (ecase (first filter)
      (:and
       (format stream "(")
       (loop for (sub . next) on (rest filter)
             do (%filter-to-sql sub stream)
                (when next (format stream " AND ")))
       (format stream ")"))
      (:or
       (format stream "(")
       (loop for (sub . next) on (rest filter)
             do (%filter-to-sql sub stream)
                (when next (format stream " OR ")))
       (format stream ")"))
      (:not
       (format stream "NOT (")
       (%filter-to-sql (second filter) stream)
       (format stream ")"))
      ((:= :>= :<= :~=)
       (case (attribute-key (second filter))
         (:attributes
          (format stream "(LOWER(atrs.key) = LOWER(")
          (escape (second filter))
          (format stream ") AND LOWER(atrs.value)"))
         (:classes
          (format stream "(LOWER(cls.class)"))
         (T
          (format stream "(LOWER(~(~a~))" (attribute-key (second filter)))))
       (ecase (first filter)
         (:= (format stream " = LOWER("))
         (:>= (format stream " >= "))
         (:<= (format stream " <= "))
         (:~= (format stream " ILIKE (")))
       (escape (third filter))
       (format stream "))"))
      (:=*
       (case (attribute-key (second filter))
         (:attributes
          (format stream "LOWER(atrs.key) = LOWER(")
          (escape (second filter))
          (format stream ")"))
         (:classes
          (format stream "(cls.class IS NOT NULL)"))
         (T
          (format stream "~(~a~) != ''" (attribute-key (second filter))))))
      (:substring))))

(defun filter-to-sql (filter &optional limit)
  (with-output-to-string (stream)
    (format stream "SELECT id FROM accounts INNER JOIN classes AS cls ON (id = cls.account) INNER JOIN attributes AS atrs ON (id = atrs.account) WHERE ")
    (%filter-to-sql filter stream)
    (format stream " GROUP BY id")
    (when limit (format stream " LIMIT ~d" limit))))

(defun filter-accounts (filter &key limit)
  (let ((filter (filter-to-sql filter limit)))
    (mapcar #'find-account (postmodern:query filter :column))))

(defun authenticate (account password)
  (connect)
  (let ((account (ensure-account account)))
    (if (crypto-shortcuts:check-rfc-2307-hash password (getf account :password))
        account
        (error 'authentication-failed :name (getf account :name)))))

(defun make-account (name mail &key password real-name note classes attributes already-hashed)
  (connect)
  (with-transaction ()
    (let ((id (postmodern:query (:insert-into 'accounts :set
                                              'name name
                                              'mail mail
                                              'password (if password
                                                            (if already-hashed password (cryptos:rfc-2307-hash password))
                                                            "")
                                              'real-name real-name
                                              'note (or note "")
                                              :returning 'id)
                                :single)))
      (edit-account id :classes classes :attributes attributes))))

(defun insert-account (account)
  (make-account (getf account :name) (getf account :mail)
                :password (getf account :password)
                :real-name (getf account :real-name)
                :note (getf account :note)
                :classes (getf account :classes)
                :attributes (getf account :attributes)
                :already-hashed T))

(defun edit-account (account &key name mail real-name note password already-hashed (classes NIL classes-p) (attributes NIL attributes-p))
  (connect)
  (with-transaction ()
    (let* ((account (ensure-account account))
           (id (getf account :id)))
      (flet ((update (karg field value)
               (postmodern:query (:update 'accounts :set field value :where (:= 'id id)))
               (setf (getf account karg) value)))
        (when name (update :name 'name name))
        (when mail (update :mail 'mail mail))
        (when real-name (update :real-name 'real-name real-name))
        (when note (update 'note note :note))
        (when password (update :password 'password (if already-hashed password (cryptos:rfc-2307-hash password)))))
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
      (postmodern:query (:delete-from 'admins :where (:= 'account id)))
      (postmodern:query (:delete-from 'classes :where (:= 'account id)))
      (postmodern:query (:delete-from 'attributes :where (:= 'account id)))
      (postmodern:query (:delete-from 'accounts :where (:= 'id id)))
      account)))
