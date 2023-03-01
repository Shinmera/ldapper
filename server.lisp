(in-package #:org.shirakumo.ldapper)

(defvar *base-dn* NIL)
(defvar *ldap-servers* '(("0.0.0.0" 389)
                         ("0.0.0.0" 636 :ssl-certificate "ldapper-chain.pem" :ssl-certificate-key "ldapper-key.pem")))
(defvar *workers* 20)
(defvar *listeners* ())

(defclass listener ()
  ((socket :initform NIL :accessor socket)
   (context :initform NIL :accessor context)))

(defun start-listener (host port &key ssl-certificate ssl-certificate-key ssl-certificate-password)
  (let ((listener (make-instance 'listener)))
    (cond ((and ssl-certificate ssl-certificate-key)
           (setf (context listener) (cl+ssl:make-context :certificate-chain-file ssl-certificate :private-key-file ssl-certificate-key :private-key-password ssl-certificate-password)))
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
  ((socket :initarg :socket :initform NIL :accessor socket)
   (socket-stream :initform NIL :accessor socket-stream)
   (channel :initform (lparallel:make-channel) :accessor channel)
   (account :initform NIL :accessor account)))

(defmethod initialize-instance :after ((client client) &key socket)
  (setf (socket-stream client) (usocket:socket-stream socket)))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type T)
    (let ((socket (socket client)))
      (if socket
          (handler-case (format stream "~a" (usocket:get-peer-name socket))
            (error () (format stream "CLOSING")))
          (format stream "CLOSED")))))

(defmethod accept ((listener listener))
  (let ((socket (usocket:socket-accept (socket listener) :element-type '(unsigned-byte 8))))
    (v:debug :ldapper "Accepting new connection on ~a" (usocket:get-peer-name socket))
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
  (loop for ready = (usocket:wait-for-input (mapcar #'socket *listeners*) :ready-only T)
        do (loop for socket in ready
                 for listener = (find socket *listeners* :key #'socket)
                 for client = (accept listener)
                 do (lparallel:submit-task (channel client) #'serve client))))

(defmethod serve ((client client))
  (restart-case
      (handler-bind ((end-of-file #'abort))
        (loop while (and (socket-stream client) (open-stream-p (socket-stream client)))
              for command = (read-command (socket-stream client))
              do (process-command command client)))
    (abort ()
      :report "Disconnect the client."
      (close client))))
