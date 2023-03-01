#|
 This file is a part of ldapper
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.ldapper)

(defclass result (message)
  ((code :initarg :code :initform :success :accessor code)
   (domain-name :initarg :domain-name :initform "" :accessor domain-name)
   (message :initarg :message :initform "Ok." :accessor message)
   (referrals :initarg :referrals :initform NIL :accessor referrals)))

(defmethod tag ((result result))
  (type-of result))

(defmethod encode-object ((result result) vec)
  (encode-enum (ldap-result-code->id (code result)) vec)
  (encode (domain-name result) vec)
  (encode (message result) vec)
  (when (referrals result)
    (vector-push-extend #x03 vec)
    (encode (apply #'encode* (referrals result)) vec)))

(defclass bind-response (result)
  ((sasl-response :initarg :sasl-response :initform NIL :accessor sasl-response)))

(defmethod encode-object ((result bind-response) vec)
  (call-next-method)
  (when (sasl-response result)
    (encode (sasl-response result) vec)))

(defclass lookup-entry (message)
  ((domain-name :initarg :domain-name :accessor domain-name)
   (attributes :initarg :attributes :initform () :accessor attributes)))

(defmethod encode-object ((result lookup-entry) vec)
  (encode (domain-name result) vec)
  (encode (encode-alist (attributes result)) vec))

(defmethod tag ((entry lookup-entry)) 'lookup-entry)

(defclass lookup-done (result)
  ())

(defclass modify-response (result)
  ())

(defclass add-response (result)
  ())

(defclass del-response (result)
  ())

(defclass moddn-response (result)
  ())

(defclass comp-response (result)
  ())

(defclass extended-response (result)
  ((oid :initarg :oid :initform NIL :accessor oid)
   (value :initarg :value :initform NIL :accessor value)))

(defmethod encode-object ((result extended-response) vec)
  (call-next-method)
  (when (oid result)
    (vector-push-extend #x10 vec)
    (encode (oid result) vec))
  (when (value result)
    (vector-push-extend #x11 vec)
    (encode (value result) vec)))
