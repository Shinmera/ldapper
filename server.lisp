#|
 This file is a part of ldapper
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.ldapper)

(defvar *base-dn* NIL)
(defvar *ldap-servers* '(("0.0.0.0" 389)
                         ("0.0.0.0" 636 :ssl-certificate "ldapper-chain.pem" :ssl-certificate-key "ldapper-key.pem")))
(defvar *connection-timeout* (* 5 60))
(defvar *user-id* NIL)
(defvar *group-id* NIL)
(defvar *listeners* (make-hash-table :test 'eq))
(defvar *thread* NIL)

(defun lower-privileges (user group)
  #+sbcl
  (etypecase group
    (null)
    (integer (sb-posix:setgid group))
    (string (sb-posix:setgid (sb-posix:group-gid (sb-posix:getgrnam group)))))
  #+sbcl
  (etypecase user
    (null)
    (integer (sb-posix:setuid user))
    (string (sb-posix:setuid (sb-posix:passwd-uid (sb-posix:getpwnam user))))))

(defclass listener ()
  ((socket :initform NIL :accessor socket)
   (context :initform NIL :accessor context)))

(defun start-listener (host port &key ssl-certificate ssl-certificate-key ssl-certificate-password)
  (let ((listener (make-instance 'listener)))
    (cond ((and ssl-certificate ssl-certificate-key)
           (setf (context listener) (cl+ssl:make-context :certificate-chain-file (uiop:native-namestring ssl-certificate)
                                                         :private-key-file (uiop:native-namestring ssl-certificate-key)
                                                         :private-key-password ssl-certificate-password)))
          ((or ssl-certificate ssl-certificate-key)
           (error "Need both ssl-certificate and ssl-certificate-key")))
    (v:info :ldapper "Listening on ~a:~a~@[ SSL~*~]" host port ssl-certificate)
    (setf (socket listener) (usocket:socket-listen host port :reuse-address T :element-type '(unsigned-byte 8)))
    listener))

(defmethod accept ((listener listener))
  (let* ((socket (usocket:socket-accept (socket listener) :element-type '(unsigned-byte 8)))
         (client (if (context listener)
                     (make-instance 'ssl-client :socket socket :context (context listener))
                     (make-instance 'client :socket socket))))
    (setf (gethash socket *listeners*) client)))

(defmethod close ((listener listener) &key abort)
  (declare (ignore abort))
  (when (socket listener)
    (usocket:socket-close (socket listener))
    (remhash (socket listener) *listeners*)
    (setf (socket listener) NIL))
  (when (context listener)
    (cl+ssl:ssl-ctx-free (context listener))
    (setf (context listener) NIL)))

(defclass client ()
  ((id :accessor id)
   (socket :initarg :socket :initform NIL :accessor socket)
   (socket-stream :initform NIL :accessor socket-stream)
   (account :initform NIL :accessor account)))

(defmethod initialize-instance :after ((client client) &key socket)
  (setf (socket-stream client) (usocket:socket-stream socket))
  (setf (id client) (format NIL "~a:~a"
                            (usocket:vector-quad-to-dotted-quad (usocket:get-peer-address socket))
                            (usocket:get-peer-port socket)))
  (v:debug :ldapper "~a Accepting new connection" client))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~a~@[ CLOSED~]" (id client) (open-stream-p (socket-stream client)))))

(defmethod accept ((client client))
  (restart-case
      (handler-bind (((or error sb-ext:timeout) (lambda (e) (v:severe :ldapper e) (abort))))
        (handler-case
            (sb-ext:with-timeout 1.0
              (process-command (read-command (socket-stream client)) client))
          (end-of-file ()
            (close client))))
    (abort (&optional e)
      :report "Disconnect the client."
      (v:info :ldapper "~a Aborting client~@[~%  ~a~]" client e)
      (close client))))

(defmethod close ((client client) &key abort)
  (when (socket-stream client)
    (v:debug :ldapper "~a Closing connection" client)
    (close (socket-stream client) :abort abort)
    (setf (socket-stream client) NIL))
  (when (socket client)
    (usocket:socket-close (socket client))
    (remhash (socket client) *listeners*)
    (setf (socket client) NIL)))

(defclass ssl-client (client)
  ((context :initarg :context :accessor context)))

(defmethod shared-initialize :after ((client ssl-client) slots &key context)
  (when context
    (cl+ssl:with-global-context (context)
      (setf (socket-stream client) (cl+ssl:make-ssl-server-stream (usocket:socket-stream (socket client)))))))

(defun start (&key (servers NIL servers-p))
  (unwind-protect
       (progn
         (read-config)
         (connect)
         (init-database)
         (v:info :ldapper "Starting server")
         (dolist (server (if servers-p servers *ldap-servers*))
           (let ((listener (apply #'start-listener server)))
             (setf (gethash (socket listener) *listeners*) listener)))
         (when (= 0 (sb-posix:getuid))
           (lower-privileges *user-id* *group-id*))
         (acceptor-loop))
    (stop)))

(defun stop ()
  (v:info :ldapper "Stopping server")
  (loop for object being the hash-values of *listeners*
        do (close object))
  (disconnect))

(defun acceptor-loop ()
  (restart-case
      (loop (dolist (socket (usocket:wait-for-input (alexandria:hash-table-keys *listeners*) :ready-only T))
              (accept (gethash socket *listeners*))))
    (abort ()
      :report "Exit the acceptor loop")))
