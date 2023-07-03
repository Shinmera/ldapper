(in-package #:org.shirakumo.ldapper)

(define-condition ldapper-condition () ())

(define-condition ldapper-error (ldapper-condition error) ())

(define-condition no-such-account (ldapper-error)
  ((name :initarg :name :reader name))
  (:report (lambda (c s) (format s "No account with name ~s found."
                                 (name c)))))

(defmethod code ((condition no-such-account)) :no-such-object)

(define-condition authentication-failed (ldapper-error)
  ((name :initarg :name :reader name))
  (:report (lambda (c s) (format s "Failed to authenticate against ~s."
                                 (name c)))))

(defmethod code ((condition authentication-failed)) :invalid-credentials)

(define-condition permission-denied (ldapper-error)
  ((name :initarg :name :reader name))
  (:report (lambda (c s) (format s "~s is not permitted to perform this action."
                                 (name c)))))

(defmethod code ((condition permission-denied)) :insufficient-access-rights)

(define-condition attribute-required (ldapper-error)
  ((attribute :initarg :attribute :reader attribute))
  (:report (lambda (c s) (format s "The attribute ~s is required and cannot be removed."
                                 (attribute c)))))

(defmethod code ((condition attribute-required)) :constraint-violation)

(define-condition unknown-command (ldapper-error)
  ((oid :initarg :oid :reader oid))
  (:report (lambda (c s) (format s "The command with OID ~s is unknown."
                                 (oid c)))))

(defmethod code ((condition unknown-command)) :operations-error)
