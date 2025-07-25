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
    (setf postmodern:*database* (postmodern:connect *postgres-db* *postgres-user* *postgres-pass* *postgres-host*
                                                    :pooled-p T))))

(defun disconnect ()
  (when postmodern:*database*
    (postmodern:disconnect postmodern:*database*)))

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
                                       ((account :type integer :references ((accounts id :on-delete :cascade)))
                                        (class :type (varchar 64))))))
    (unless (find "attributes" tables :test #'string= :key #'second)
      (postmodern:query (:create-table 'attributes
                                       ((account :type integer :references ((accounts id :on-delete :cascade)))
                                        (key :type (varchar 64))
                                        (value :type text)))))
    (unless (find "admins" tables :test #'string= :key #'second)
      (postmodern:query (:create-table 'admins
                                       ((account :type integer :references ((accounts id :on-delete :cascade)) :unique T)))))))

(defun list-accounts ()
  (connect)
  (postmodern:query (:order-by (:select '* (:as (:array (:select 'class :from 'classes :where (:= 'account 'id))) 'classes)
                                        (:as (:array (:select (:array[] 'key 'value) :from 'attributes :where (:= 'account 'id))) 'attributes)
                                        (:as (:select 1 :from 'admins :where (:= 'account 'id)) 'admin-p)
                                        :from 'accounts)
                               'name) :plists))

(defun find-account (name)
  (connect)
  (etypecase name
    (integer (postmodern:query (:select '* (:as (:array (:select 'class :from 'classes :where (:= 'account 'id))) 'classes)
                                        (:as (:array (:select (:array[] 'key 'value) :from 'attributes :where (:= 'account 'id))) 'attributes)
                                        (:as (:select 1 :from 'admins :where (:= 'account 'id)) 'admin-p)
                                        :from 'accounts :where (:= 'id name)) :plist))
    (string (postmodern:query (:select '* (:as (:array (:select 'class :from 'classes :where (:= 'account 'id))) 'classes)
                                       (:as (:array (:select (:array[] 'key 'value) :from 'attributes :where (:= 'account 'id))) 'attributes)
                                       (:as (:select 1 :from 'admins :where (:= 'account 'id)) 'admin-p)
                                       :from 'accounts :where (:= (:lower 'name) (string-downcase name))) :plist))))

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
    (cons (eql 1 (getf account :admin-p)))))

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
                      ((#\') (write-char #\' stream)))
                    (write-char (char-downcase char) stream))
           (write-char #\' stream)))
    (when (second filter)
      (ecase (first filter)
        (:and
         (format stream "(")
         (loop for (sub . next) on (rest filter)
               do (when (and (%filter-to-sql sub stream) next)
                    (format stream " AND ")))
         (format stream ")")
         filter)
        (:or
         (format stream "(")
         (loop for (sub . next) on (rest filter)
               do (when (and (%filter-to-sql sub stream) next)
                    (format stream " OR ")))
         (format stream ")")
         filter)
        (:not
         (format stream "NOT (")
         (%filter-to-sql (second filter) stream)
         (format stream ")")
         filter)
        ((:= :>= :<= :~=)
         (case (attribute-key (second filter))
           (:attributes
            (format stream "(atrs.key = ")
            (escape (second filter))
            (format stream " AND LOWER(atrs.value)"))
           (:classes
            (format stream "(cls.class"))
           (:name
            (format stream "(LOWER(ac.name)"))
           (T
            (format stream "(~(ac.~a~)" (attribute-key (second filter)))))
         (ecase (first filter)
           (:= (format stream " = "))
           (:>= (format stream " >= "))
           (:<= (format stream " <= "))
           (:~= (format stream " ILIKE ")))
         (escape (string-downcase (third filter)))
         (format stream ")")
         filter)
        (:=*
         (case (attribute-key (second filter))
           (:attributes
            (format stream "atrs.key = ")
            (escape (second filter)))
           (:classes
            (format stream "(cls.class IS NOT NULL)"))
           (T
            (format stream "~(ac.~a~) != ''" (attribute-key (second filter)))))
         filter)
        (:substring)))))

(defun filter-to-sql (filter &optional limit)
  (with-output-to-string (stream)
    (format stream "SELECT ac.*,
 ARRAY(SELECT class FROM classes WHERE account=ac.id) AS classes,
 ARRAY(SELECT ARRAY[key,value] FROM attributes WHERE account=ac.id) AS attributes,
 (SELECT 1 FROM admins WHERE account=ac.id) AS \"admin-p\"
 FROM accounts AS ac
   INNER JOIN classes AS cls ON (ac.id = cls.account)
   INNER JOIN attributes AS atrs ON (ac.id = atrs.account)
 WHERE ")
    (%filter-to-sql filter stream)
    (format stream "
 GROUP BY ac.id")
    (when limit (format stream " LIMIT ~d" limit))))

(defun filter-accounts (filter &key limit)
  (connect)
  (postmodern:query (filter-to-sql filter limit) :plists))

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
                                              'name (string-downcase name)
                                              'mail mail
                                              'password (if password
                                                            (if already-hashed password (cryptos:rfc-2307-hash password))
                                                            "")
                                              'real-name (or real-name "")
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
        (when name (update :name 'name (string-downcase name)))
        (when mail (update :mail 'mail mail))
        (when real-name (update :real-name 'real-name real-name))
        (when note (update 'note note :note))
        (when password (update :password 'password (if already-hashed password (cryptos:rfc-2307-hash password)))))
      (when classes-p
        (postmodern:query (:delete-from 'classes :where (:= 'account id)))
        (when classes
          (postmodern:query (:insert-rows-into 'classes :columns 'account 'class
                                               :values (map 'list (lambda (c) (list id (string-downcase c))) classes))))
        (setf (getf account :classes) classes))
      (when attributes-p
        (postmodern:query (:delete-from 'attributes :where (:= 'account id)))
        (when (< 0 (length attributes))
          (postmodern:query (:insert-rows-into 'attributes :columns 'account 'key 'value
                             :values (etypecase attributes
                                       ((array T (* 2))
                                        (loop for y from 0 below (array-dimension attributes 0)
                                              collect (list id (string-downcase (aref attributes y 0)) (aref attributes y 1))))
                                       (sequence (map 'list (lambda (a) (list* id (string-downcase (car a)) (cdr a))) attributes))))))
        (setf (getf account :attributes) attributes))
      account)))

(defun delete-account (account)
  (connect)
  (with-transaction ()
    (let* ((account (ensure-account account))
           (id (getf account :id)))
      (postmodern:query (:delete-from 'accounts :where (:= 'id id)))
      account)))

(defun update-attributes (account type attribute &rest vals)
  (let ((key (attribute-key attribute))
        (attributes (getf account :attributes))
        (args ()))
    (ecase key
      (:name
       (ecase type
         ((:add :replace)
          (setf (getf args key) (cn-from-dn (or (first vals) (error 'attribute-required :attribute "cn")))))
         (:delete)))
      (:mail
       (ecase type
         ((:add :replace)
          (setf (getf args key) (or (first vals) (error 'attribute-required :attribute "mail"))))
         (:delete
          (setf (getf args key) ""))))
      ((:note :real-name)
       (ecase type
         (:add
          (setf (getf args key) (or (first vals) "")))
         (:replace
          (setf (getf args key) (or (first vals) "")))
         (:delete
          (when (or (null vals) (find (getf args key) vals :test #'string=))
            (setf (getf args key) "")))))
      (:password
       (ecase type
         ((:add :replace)
          (setf (getf args key) (base64:base64-string-to-string (or (first vals) "")))
          (setf (getf args :already-hashed) T))
         (:delete
          (setf (getf args key) ""))))
      (:classes
       (ecase type
         (:add
          (setf (getf args key) (union (coerce (getf account key) 'list) vals :test #'string-equal)))
         (:replace
          (setf (getf args key) vals))
         (:delete
          (setf (getf args key) (when vals (set-difference (coerce (getf account key) 'list) vals :test #'string-equal))))))
      (:attributes
       (flet ((filter-attributes (key &optional vals)
                (etypecase attributes
                  (cons
                   (loop for entry in attributes
                         unless (and (string-equal (first entry) key)
                                     (or (null vals) (find (second entry) vals :test #'string=)))
                         collect entry))
                  ((array T (* 2))
                   (setf attributes (loop for i from 0 below (array-dimension attributes 0)
                                          for k = (aref attributes i 0)
                                          for v = (aref attributes i 1)
                                          unless (and (string-equal k key)
                                                      (or (null vals) (find v vals :test #'string=)))
                                          collect (list k v)))))))
         (ecase type
           (:add
            (when (typep attributes '(array T (* 2)))
              (setf attributes (loop for i from 0 below (array-dimension attributes 0)
                                     collect (list (aref attributes i 0) (aref attributes i 1)))))
            (when (= 0 (length attributes))
              (setf attributes ()))
            (dolist (val vals)
              (pushnew (list attribute val) attributes :test
                       (lambda (a b) (and (string-equal (first a) (first b))
                                          (string= (second a) (second b)))))))
           (:replace
            (filter-attributes attribute)
            (dolist (val vals)
              (push (list attribute val) attributes)))
           (:delete
            (filter-attributes attribute vals))))))
    (values attributes args)))
