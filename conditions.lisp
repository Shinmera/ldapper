(in-package #:org.shirakumo.ldapper)

(define-condition ldapper-condition () ())

(define-condition ldapper-error (ldapper-condition error) ())

(define-condition no-such-account (ldapper-error)
  ((name :initarg :name :reader name))
  (:report (lambda (c s) (format s "No account with name ~s found."
                                 (name c)))))

(define-condition authentication-failed (ldapper-error)
  ((name :initarg :name :reader name))
  (:report (lambda (c s) (format s "Failed to authenticate against ~s."
                                 (name c)))))

(define-condition permission-denied (ldapper-error)
  ((account :initarg :account :reader account))
  (:report (lambda (c s) (format s "~a is not permitted to perform this action."
                                 (getf (account c) :name "Anonymous")))))

(define-condition attribute-required (ldapper-error)
  ((attribute :initarg :attribute :reader attribute))
  (:report (lambda (c s) (format s "The attribute ~s is required and cannot be removed."
                                 (attribute c)))))
