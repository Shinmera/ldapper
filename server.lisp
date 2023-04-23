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
(defvar *workers* 20)
(defvar *listeners* ())
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

(defmethod close ((listener listener) &key abort)
  (declare (ignore abort))
  (when (socket listener)
    (usocket:socket-close (socket listener))
    (setf (socket listener) NIL))
  (when (context listener)
    (cl+ssl:ssl-ctx-free (context listener))
    (setf (context listener) NIL)))

(defclass client ()
  ((id :accessor id)
   (socket :initarg :socket :initform NIL :accessor socket)
   (socket-stream :initform NIL :accessor socket-stream)
   (channel :initform (lparallel:make-channel) :accessor channel)
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

(defmethod accept ((listener listener))
  (let ((socket (usocket:socket-accept (socket listener) :element-type '(unsigned-byte 8))))
    (if (context listener)
        (make-instance 'ssl-client :socket socket :context (context listener))
        (make-instance 'client :socket socket))))

(defmethod close ((client client) &key abort)
  (when (socket-stream client)
    (v:debug :ldapper "~a Closing connection" client)
    (close (socket-stream client) :abort abort)
    (setf (socket-stream client) NIL))
  (when (socket client)
    (usocket:socket-close (socket client))
    (setf (socket client) NIL)))

(defclass ssl-client (client)
  ((context :initarg :context :accessor context)))

(defmethod shared-initialize :after ((client ssl-client) slots &key context)
  (when context
    (cl+ssl:with-global-context (context)
      (setf (socket-stream client) (cl+ssl:make-ssl-server-stream (usocket:socket-stream (socket client)))))))

(defun start (&key (servers NIL servers-p) (workers *workers*))
  (unwind-protect
       (progn
         (read-config)
         (connect)
         (init-database)
         (v:info :ldapper "Starting server")
         (unless lparallel:*kernel*
           (setf lparallel:*kernel* (lparallel:make-kernel workers :name "ldapper-clients")))
         (dolist (server (if servers-p servers *ldap-servers*))
           (push (apply #'start-listener server) *listeners*))
         (when (= 0 (sb-posix:getuid))
           (lower-privileges *user-id* *group-id*))
         (acceptor-loop))
    (stop)))

(defun stop ()
  (v:info :ldapper "Stopping server")
  (when lparallel:*kernel*
    (lparallel:end-kernel))
  (loop for socket = (pop *listeners*)
        while socket do (close socket))
  (disconnect))

(defun acceptor-loop ()
  (restart-case
      (loop for ready = (usocket:wait-for-input (mapcar #'socket *listeners*) :ready-only T)
            do (loop for socket in ready
                     for listener = (find socket *listeners* :key #'socket)
                     for client = (accept listener)
                     do (lparallel:submit-task (channel client) #'serve client)))
    (abort ()
      :report "Exit the acceptor loop")))

(defmethod serve ((client client))
  (let ((postmodern:*database* NIL))
    (unwind-protect
         (restart-case
             (handler-bind (((or stream-error usocket:socket-error) #'abort)
                            (error (lambda (e) (v:severe :ldapper e) (abort))))
               (loop while (and (socket-stream client) (open-stream-p (socket-stream client)))
                     for timeout = (nth-value 1 (usocket:wait-for-input (socket client) :timeout *connection-timeout*))
                     do (when (or (null timeout) (<= timeout 0))
                          (v:info :ldapper "~a Timing client out" client)
                          (return (close client)))
                        (process-command (read-command (socket-stream client)) client)))
           (abort ()
             :report "Disconnect the client."
             (v:info :ldapper "~a Aborting client" client)
             (close client)))
      (disconnect))))

(defun start-threaded ()
  (when (and *thread* (bt:thread-alive-p *thread*))
    (error "Already running!"))
  (setf *thread* (bt:make-thread #'start :name "ldapper")))

(defun stop-threaded ()
  (when (and *thread* (bt:thread-alive-p *thread*))
    (bt:interrupt-thread *thread* #'abort)
    (loop while (bt:thread-alive-p *thread*)
          do (sleep 0.01))
    (setf *thread* NIL)))
