(in-package #:org.shirakumo.ldapper)

(defvar *ldap-servers* '(("0.0.0.0" 389)
                         ("0.0.0.0" 636 :ssl-certificate "ldapper-chain.pem" :ssl-certificate-key "ldapper-key.pem")))
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
    (setf (socket listener) (usocket:socket-listen host port :reuse-address T :element-type '(unsigned-byte 8)))))

(defmethod close ((listener listener) &key abort)
  (when (socket listener)
    (close (socket listener) :abort abort)
    (setf (socket listener) NIL))
  (when (context listener)
    (cl+ssl:ssl-ctx-free (context listener))
    (setf (context listener) NIL)))

(defclass client ()
  ((socket :initarg :socket :initform NIL :accessor socket)
   (socket-stream :initform NIL :accessor socket-stream)
   (channel :initform (lparallel:make-channel) :accessor channel)))

(defmethod initialize-instance :after ((client client) &key socket)
  (setf (socket-stream client) (usocket:socket-stream socket)))

(defmethod accept ((listener listener))
  (let ((socket (usocket:socket-accept (socket listener) :element-type '(unsigned-byte 8))))
    (if (context listener)
        (make-instance 'ssl-client :socket socket :context (context listener))
        (make-instance 'client :socket socket))))

(defmethod close ((client client) &key abort)
  (when (socket-stream client)
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

(defun start (&key (servers *ldap-servers*) (workers 10))
  (unwind-protect
       (progn
         (connect)
         (unless lparallel:*kernel*
           (setf lparallel:*kernel* (lparallel:make-kernel workers :name 'ldapper-clients)))
         (dolist (server servers)
           (push (apply #'start-listener server) *listeners*))
         (acceptor-loop))
    (stop)))

(defun stop ()
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
  (loop while (open-stream-p (socket-stream client))
        do (process-command (read-command (socket-stream client)) (socket-stream client))))
