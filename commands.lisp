(in-package #:org.shirakumo.ldapper)

(defmacro define-simple-print (class &rest slots)
  `(defmethod print-object ((object ,class) stream)
     (print-unreadable-object (object stream :type T)
       (format stream ,(format NIL "~{~a: ~~s~^ ~}" slots)
               ,@(loop for slot in slots collect `(slot-value object ',slot))))))

(defclass message ()
  ((client :initarg :client :accessor client)
   (id :initarg :id :initform 0 :accessor id)))

(defclass command (message)
  ())

(defgeneric tag (command))
(defgeneric response-tag (command))

(defmethod decode-object ((tag symbol) vec start end)
  (decode-object (make-instance tag) vec start end))

(defmethod decode-object :around ((command command) vec start end)
  (values command (call-next-method)))

(defmethod encode-object :around (object (vec vector))
  (call-next-method)
  vec)

(defmethod tag ((command command))
  (type-of command))

(defmethod make-response ((command command) &rest args)
  (apply #'make-instance (response-tag command) :client (client command) :id (id command) args))

(defclass bind (command)
  ((version :initarg :version :initform 3 :accessor version) 
   (user :initarg :user :accessor user)
   (pass :initarg :pass :accessor pass)))

(define-simple-print bind version user)

(defmethod response-tag ((command bind)) 'bind-response)

(defmethod decode-object ((command bind) vec start end)
  (with-decoding (version user start) (vec start end)
    (setf (version command) version)
    (setf (user command) user)
    (setf start (check-ber-tag vec start :context :primitive 0))
    (multiple-value-bind (pass start) (decode-string vec start)
      (setf (pass command) pass)
      start)))

(defmethod encode-object ((command bind) vec)
  (encode (version command) vec)
  (encode (user command) vec)
  (encode-ber-tag :context :primitive 0 vec)
  (let ((octets (babel:string-to-octets (pass command) :encoding :utf-8)))
    (encode-ber-length (length octets) vec)
    (vector-append-extend octets vec)))

(defclass unbind (command)
  ())

(defmethod decode-object ((command unbind) vec start end)
  end)

(defmethod encode-object ((command unbind) vec)
  (encode 'null vec))

(defclass abandon (command)
  ((id :initarg :id :accessor id)))

(defmethod decode-object ((command abandon) vec start end)
  (with-decoding (id) (vec start end)
    (setf (id command) id)
    start))

(defmethod encode-object ((command abandon) vec)
  (encode (id command) vec))

(defclass add (command)
  ((domain-name :initarg :domain-name :accessor domain-name) 
   (attributes :initarg :attributes :initform () :accessor attributes)))

(define-simple-print add domain-name attributes)

(defmethod response-tag ((command add)) 'add-response)

(defmethod decode-object ((command add) vec start end)
  (with-decoding (domain-name attributes) (vec start end)
    (setf (domain-name command) domain-name)
    (with-decoding (&rest attributes) (attributes)
      (setf (attributes command) (loop for attrvec in attributes
                                       for (attribute valvec) = (decode* attrvec)
                                       collect (cons attribute (decode* valvec)))))
    start))

(defmethod encode-object ((command add) vec)
  (encode (domain-name command) vec)
  (encode (encode-alist (attributes command)) vec))

(defclass del (command)
  ((domain-name :initarg :domain-name :accessor domain-name)))

(define-simple-print del domain-name)

(defmethod response-tag ((command del)) 'del-response)

(defmethod decode-object ((command del) vec start end)
  (setf (domain-name command) (babel:octets-to-string vec :start start :end end :encoding :utf-8)))

(defmethod encode-object ((command del) vec)
  (vector-append-extend (babel:string-to-octets (domain-name command) :encoding :utf-8) vec))

(defclass moddn (command)
  ((domain-name :initarg :domain-name :accessor domain-name)
   (new-domain-name :initarg :new-domain-name :accessor new-domain-name)
   (delete-old-p :initarg :delete-old-p :initform T :accessor delete-old-p)
   (new-superior :initarg :new-superior :initform NIL :accessor new-superior)))

(define-simple-print moddn domain-name new-domain-name)

(defmethod response-tag ((command moddn)) 'moddn-response)

(defmethod decode-object ((command moddn) vec start end)
  (with-decoding (domain-name new-domain-name delete-old-p &optional new-superior) (vec start end)
    (setf (domain-name command) domain-name)
    (setf (new-domain-name command) new-domain-name)
    (setf (delete-old-p command) delete-old-p)
    (setf (new-superior command) new-superior)
    start))

(defmethod encode-object ((command moddn) vec)
  (encode (domain-name command) vec)
  (encode (new-domain-name command) vec)
  (encode-boolean (delete-old-p command) vec)
  (when (new-superior command)
    (encode (new-superior command) vec)))

(defclass compare (command)
  ((domain-name :initarg :domain-name :accessor domain-name)
   (attribute :initarg :attribute :accessor attribute)
   (value :initarg :value :accessor value)))

(define-simple-print compare domain-name attribute value)

(defmethod response-tag ((command compare)) 'compare-response)

(defmethod decode-object ((command compare) vec start end)
  (with-decoding (domain-name pair) (vec start end)
    (setf (domain-name command) domain-name)
    (destructuring-bind (attribute value) (decode* pair)
      (setf (attribute command) attribute)
      (setf (value command) value)
      start)))

(defmethod encode-object ((command compare) vec)
  (encode (domain-name command) vec)
  (encode (encode* (attribute command) (value command)) vec))

(defclass modify (command)
  ((domain-name :initarg :domain-name :accessor domain-name) 
   (modifications :initarg :modifications :initform () :accessor modifications)))

(define-simple-print modify domain-name modifications)

(defmethod response-tag ((command modify)) 'modify-response)

(defmethod decode-object ((command modify) vec start end)
  (with-decoding (domain-name modifications) (vec start end)
    (setf (domain-name command) domain-name)
    (with-decoding (&rest modifications) (modifications)
      (setf (modifications command) (loop for modvec in modifications
                                          for (type attrvec) = (decode* modvec)
                                          for (attribute valvec) = (decode* attrvec)
                                          collect (list* (id->ldap-modify-type type) attribute (decode* valvec)))))
    start))

(defmethod encode-object ((command modify) vec)
  (encode (domain-name command) vec)
  (let ((modifications (vec)))
    (loop for (type attribute . values) in (modifications command)
          for mod-seq = (vec)
          do (encode type mod-seq)
             (encode (encode-kv attribute values) mod-seq)
             (encode mod-seq modifications))
    (encode modifications vec)))

(defclass lookup (command)
  ((filter :initarg :filter :accessor filter)
   (base :initarg :base :initform "" :accessor base)
   (scope :initarg :scope :initform :base :accessor scope)
   (deref :initarg :deref :initform :never :accessor deref)
   (size :initarg :size :initform 0 :accessor size)
   (timestamp :initarg :timestamp :initform 0 :accessor timestamp)
   (types-p :initarg :types-p :initform NIL :accessor types-p)
   (attributes :initarg :attributes :initform () :accessor attributes)
   (paging-size :initarg :paging-size :initform NIL :accessor paging-size)
   (paging-cookie :initarg :paging-cookie :initform NIL :accessor paging-cookie)))

(define-simple-print lookup base filter attributes size)

(defmethod response-tag ((command lookup)) 'lookup-done)

(defmethod decode-object ((command lookup) vec start end)
  (with-decoding (base scope deref size timestamp types-p) (vec start end :count 6)
    (setf (base command) base)
    (setf (scope command) (id->ldap-scope scope))
    (setf (deref command) (id->ldap-deref deref))
    (setf (size command) size)
    (setf (timestamp command) timestamp)
    (setf (types-p command) types-p)
    (multiple-value-bind (filter next) (decode-filter vec start)
      (setf (filter command) filter)
      (setf start next))
    (multiple-value-bind (attrs next) (decode vec start)
      (setf (attributes command) (decode* attrs))
      (setf start next))
    start))

(defmethod encode-object ((command lookup) vec)
  (encode (base command) vec)
  (encode (scope command) vec)
  (encode (deref command) vec)
  (encode (size command) vec)
  (encode (timestamp command) vec)
  (encode-boolean (types-p command) vec)
  (encode-filter (filter command) vec)
  (encode (apply #'encode* (attributes command)) vec)
  (when (and (paging-size command) (= 0 (size command)))
    (encode-ber-tag :context :constructed 0 vec)
    (encode (encode* +ldap-control-extension-paging+ T (encode* (paging-size command) (paging-cookie command))) vec)))

(defun encode-filter (filter &optional (vec (vec)))
  (encode-ber-tag :context :constructed (ldap-filter->id (car filter)) vec)
  (ecase (first filter)
    ((:and :or)
     (let ((sub (vec)))
       (dolist (expr (rest filter))
         (encode-filter expr sub))
       (encode-ber-length (length sub) vec)
       (vector-append-extend sub vec)))
    (:not
     (destructuring-bind (expr) (rest filter)
       (let ((sub (encode-filter expr)))
         (encode-ber-length (length sub) vec)
         (vector-append-extend sub vec))))
    (:=*
     (vector-pop vec)
     (encode-ber-tag :context :primitive (ldap-filter->id (car filter)) vec)
     (destructuring-bind (key) (rest filter)
       (let ((octets (babel:string-to-octets key :encoding :utf-8)))
         (encode-ber-length (length octets) vec)
         (vector-append-extend octets vec))))
    ((:= :>= :<= :~=)
     (destructuring-bind (key val) (rest filter)
       (let ((sub (encode* key val)))
         (encode-ber-length (length sub) vec)
         (vector-append-extend sub vec))))
    (:substring
     (destructuring-bind (key val) (rest filter)
       (let ((sub (vec)))
         (encode key sub)
         ;; TODO: this
         (error "implement")
         (encode-ber-length (length sub) vec)
         (vector-append-extend sub vec)))))
  vec)

(defun decode-filter (vec &optional (start 0))
  (multiple-value-bind (class p/c id start) (decode-ber-tag vec start)
    (declare (ignore p/c))
    (assert (eql :context class))
    (ecase (id->ldap-filter id)
      ((:and :or)
       (multiple-value-bind (len start) (decode-ber-length vec start)
         (values (list* (id->ldap-filter id)
                        (loop with end = (+ start len)
                              until (<= end start)
                              collect (multiple-value-bind (filter next) (decode-filter vec start)
                                        (setf start next)
                                        filter)))
                 start)))
      (:not
       (multiple-value-bind (len start) (decode-ber-length vec start)
         (values (list :not (decode-filter vec start))
                 (+ start len))))
      (:=*
       (multiple-value-bind (len start) (decode-ber-length vec start)
         (values (list :=* (babel:octets-to-string vec :start start :end (+ start len) :encoding :utf-8))
                 (+ start len))))
      ((:= :>= :<= :~=)
       (multiple-value-bind (len start) (decode-ber-length vec start)
         (with-decoding (key val) (vec start (+ start len))
           (values (list (id->ldap-filter id) key val)
                   start))))
      (:substring
       (error "implement")))))

(defvar *extended-oid-map* (make-hash-table :test 'equal))

(defun oid-type (oid)
  (gethash oid *extended-oid-map* 'extended))

(defun (setf oid-type) (type oid)
  (setf (gethash oid *extended-oid-map*) type))

(defclass extended (command)
  ((oid :initarg :oid :accessor oid)
   (value :initarg :value :initform NIL :accessor value)))

(define-simple-print extended oid)

(defmethod response-tag ((command extended)) 'extended-response)

(defmethod decode-object ((command extended) vec start end)
  (setf start (check-ber-tag vec start :context :primitive 0))
  (multiple-value-bind (oid start) (decode-string vec start)
    (setf start (check-ber-tag vec start :context :primitive 1))
    (multiple-value-bind (length next) (decode-ber-length vec start)
      (when (< 0 length)
        (setf (value command) (subseq vec next (+ next length))))
      (setf start next))
    (change-class command (oid-type oid)))
  start)

(defmethod encode-object ((command extended) vec)
  (encode (oid command) vec)
  (when (value command)
    (encode (value command) vec)))

(defclass password-change (extended)
  ((user :initarg :user :accessor user)
   (pass :initarg :pass :accessor pass)
   (new-pass :initarg :new-pass :accessor new-pass)))

(setf (oid-type "1.3.6.1.4.1.4203.1.11.1") 'password-change)

(define-simple-print password-change user)

(defmethod update-instance-for-different-class :after ((previous extended) (command password-change) &key)
  (let ((start 0) (vec (value command)))
    (with-decoding (vec) (vec)
      (setf start (check-ber-tag vec start :context :primitive 0))
      (multiple-value-bind (user next) (decode-string vec start)
        (setf (user command) user)
        (setf start next))
      (setf start (check-ber-tag vec start :context :primitive 1))
      (multiple-value-bind (pass next) (decode-string vec start)
        (setf (pass command) pass)
        (setf start next))
      (setf start (check-ber-tag vec start :context :primitive 2))
      (multiple-value-bind (new-pass next) (decode-string vec start)
        (setf (new-pass command) new-pass)
        (setf start next)))))

(defclass starttls (extended)
  ())

(setf (oid-type "1.3.6.1.4.1.1466.20037") 'starttls)
