(in-package #:org.shirakumo.ldapper)

(defvar *debug* NIL)
(defvar *pidfile* NIL)
(defvar *base-dn* NIL)
(defvar *ldap-servers* '(("0.0.0.0" 389)
                         ("0.0.0.0" 636 :ssl-certificate "ldapper-chain.pem" :ssl-certificate-key "ldapper-key.pem")))
(defvar *connection-timeout* (* 5 60))
(defvar *user-id* NIL)
(defvar *group-id* NIL)
(defvar *listeners* (make-hash-table :test 'eq))
(defvar *thread* NIL)
(defvar *pending-reload* NIL)

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

(defclass socket-object ()
  ((socket :initarg :socket :initform NIL :accessor socket)))

(defmethod initialize-instance :after ((object socket-object) &key)
  (when (socket object)
    (setf (gethash (socket object) *listeners*) object)))

(defmethod close ((object socket-object) &key abort)
  (declare (ignore abort))
  (when (socket object)
    (usocket:socket-close (socket object))
    (remhash (socket object) *listeners*)
    (setf (socket object) NIL)))

(defclass listener (socket-object)
  ((context :initarg :context :initform NIL :accessor context)
   (id :initarg :id :accessor id)))

(defun start-listener (host port &rest args &key ssl-certificate ssl-certificate-key ssl-certificate-password)
  (let (context)
    (cond ((and ssl-certificate ssl-certificate-key)
           (setf context (cl+ssl:make-context :certificate-chain-file (uiop:native-namestring ssl-certificate)
                                              :private-key-file (uiop:native-namestring ssl-certificate-key)
                                              :private-key-password ssl-certificate-password)))
          ((or ssl-certificate ssl-certificate-key)
           (error "Need both ssl-certificate and ssl-certificate-key")))
    (v:info :ldapper "Listening on ~a:~a~@[ SSL~*~]" host port ssl-certificate)
    (make-instance 'listener :socket (usocket:socket-listen host port :reuse-address T :element-type '(unsigned-byte 8))
                             :context context :id (list* host port args))))

(defmethod accept ((listener listener))
  (let ((socket (usocket:socket-accept (socket listener) :element-type '(unsigned-byte 8))))
    (handler-case
        (if (context listener)
            (make-instance 'ssl-client :socket socket :context (context listener))
            (make-instance 'client :socket socket))
      (error (e)
        (v:error :ldapper "Error during accept: ~a" e)
        (v:debug :ldapper e)
        (ignore-errors (usocket:socket-close socket))
        (remhash socket *listeners*)))))

(defmethod close :after ((listener listener) &key abort)
  (declare (ignore abort))
  (when (context listener)
    (cl+ssl:ssl-ctx-free (context listener))
    (setf (context listener) NIL)))

(defclass client (socket-object)
  ((id :accessor id)
   (socket-stream :initform NIL :accessor socket-stream)
   (account :initform NIL :accessor account)))

(defmethod initialize-instance :after ((client client) &key socket)
  (setf (usocket:socket-option socket :tcp-keepalive) T)
  (setf (usocket:socket-option socket :receive-timeout) *connection-timeout*)
  (setf (usocket:socket-option socket :send-timeout) *connection-timeout*)
  (setf (socket-stream client) (usocket:socket-stream socket))
  (setf (id client) (format NIL "~a:~a"
                            (usocket:vector-quad-to-dotted-quad (usocket:get-peer-address socket))
                            (usocket:get-peer-port socket)))
  (v:debug :ldapper "~a Accepting new connection" client))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~a~@[ CLOSED~]" (id client) (not (open-stream-p (socket-stream client))))))

(defmethod accept ((client client))
  (restart-case
      (handler-bind (((and error (not stream-error))
                       (lambda (e)
                         (cond (*debug*
                                (invoke-debugger e))
                               (T
                                (v:severe :ldapper e)
                                (abort e))))))
        (handler-case
            (if *debug*
                (process-command (read-command (socket-stream client)) client)
                (sb-ext:with-timeout 1.0
                  (process-command (read-command (socket-stream client)) client)))
          (stream-error (e)
            (v:debug :ldapper "~a stream error, closing~@[~%  ~a~]" client e)
            (close client))
          (sb-ext:timeout ()
            (v:warn :ldapper "~a timed out, closing" client)
            (close client))))
    (abort (&optional e)
      :report "Disconnect the client."
      (v:info :ldapper "~a Aborting client~@[~%  ~a~]" client e)
      (close client))))

(defmethod close :before ((client client) &key abort)
  (when (socket-stream client)
    (v:debug :ldapper "~a Closing connection" client)
    (unless abort
      (ignore-errors (finish-output (socket-stream client))))
    (ignore-errors (close (socket-stream client) :abort abort))
    (setf (socket-stream client) NIL)))

(defclass ssl-client (client)
  ((context :initarg :context :accessor context)))

(defmethod shared-initialize :after ((client ssl-client) slots &key context)
  (when context
    (cl+ssl:with-global-context (context)
      (setf (socket-stream client) (cl+ssl:make-ssl-server-stream (usocket:socket-stream (socket client)))))))

(defun start (&key (servers NIL servers-p))
  (when *pidfile*
    #+sbcl
    (alexandria:write-string-into-file (princ-to-string (sb-posix:getpid)) *pidfile*
                                       :if-exists :supersede))
  (unwind-protect
       (handler-bind ((error (lambda (e)
                               (cond (*debug*
                                      (invoke-debugger e))
                                     (T
                                      (v:error :ldapper "Unhandled error in server: ~a" e)
                                      (v:trace :ldapper e))))))
         (read-config)
         (connect)
         (init-database)
         (v:info :ldapper "Starting server")
         (dolist (server (if servers-p servers *ldap-servers*))
           (apply #'start-listener server))
         (when (= 0 (sb-posix:getuid))
           (lower-privileges *user-id* *group-id*))
         (acceptor-loop)
         (v:info :ldapper "Exiting gracefully"))
    (stop)))

(defun adapt-servers ()
  (loop for listener being the hash-values of *listeners*
        do (when (and (typep listener 'listener)
                      (not (find (id listener) *ldap-servers* :test #'equal)))
             (close listener)))
  (loop for server in *ldap-servers*
        do (when (not (find server *listeners* :key #'id :test #'equal))
             (apply #'start-listener server))))

(defun reload ()
  (v:info :ldapper "Reloading server")
  (read-config)
  (disconnect)
  (init-database)
  (adapt-servers))

(defun stop ()
  (v:info :ldapper "Stopping server")
  (when *pidfile*
    (ignore-errors (uiop:delete-file-if-exists *pidfile*)))
  (loop for object being the hash-values of *listeners*
        do (close object))
  (disconnect))

(defun acceptor-loop ()
  (restart-case
      (loop (dolist (socket (handler-case (usocket:wait-for-input (alexandria:hash-table-keys *listeners*) :ready-only T)
                              (usocket:socket-error (e)
                                (v:error :ldapper "Socket failed while waiting: ~a" e)
                                (v:debug :ldapper e)
                                (ignore-errors
                                 (remhash (usocket:socket e) *listeners*))
                                ())
                              (stream-error (e)
                                (v:error :ldapper "Socket failed while waiting: ~a" e)
                                (v:debug :ldapper e)
                                (remhash (find (stream-error-stream e) (alexandria:hash-table-keys *listeners*)
                                               :key #'usocket:socket-stream)
                                         *listeners*)
                                ())))
              (accept (gethash socket *listeners*)))
            (when *pending-reload*
              (reload)
              (setf *pending-reload* NIl)))
    (abort ()
      :report "Exit the acceptor loop"
      (v:info :ldapper "Aborting acceptor loop"))))
