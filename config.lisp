#|
 This file is a part of ldapper
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.ldapper)

(defun envvar (name)
  (let ((var (uiop:getenv name)))
    (when (and var (string/= "" var))
      var)))

(defun parse-listen-config (val &optional (file *default-pathname-defaults*))
  (destructuring-bind (host port . args) (cl-ppcre:split " +" val)
    (let (ssl-cert ssl-key ssl-pass)
      (loop for arg in args
            for (k v) = (cl-ppcre:split "=" arg :limit 2)
            do (cond ((string-equal k "ssl-cert") (setf ssl-cert v))
                     ((string-equal k "ssl-key") (setf ssl-key v))
                     ((string-equal k "ssl-pass") (setf ssl-pass v))
                     (T (error "Bad LISTEN argument: ~s" k))))
      (list host (parse-integer port) :ssl-certificate (when ssl-cert (merge-pathnames ssl-cert file))
                                      :ssl-certificate-key (when ssl-key (merge-pathnames ssl-key file))
                                      :ssl-certificate-password ssl-pass))))

(defun read-config-file (file)
  (let ((listen ()))
    (flet ((process-var (var val)
             (cond ((string-equal var "LDAPPER_POSTGRES_HOST") (setf *postgres-host* val))
                   ((string-equal var "LDAPPER_POSTGRES_USER") (setf *postgres-user* val))
                   ((string-equal var "LDAPPER_POSTGRES_PASS") (setf *postgres-pass* val))
                   ((string-equal var "LDAPPER_POSTGRES_DB") (setf *postgres-db* val))
                   ((string-equal var "LDAPPER_BASE_DN") (setf *base-dn* val))
                   ((string-equal var "LDAPPER_CONNECTION_TIMEOUT") (setf *connection-timeout* (parse-integer val)))
                   ((string-equal var "LDAPPER_LISTEN") (push (parse-listen-config val file) listen))
                   ((string-equal var "LDAPPER_USER") (setf *user-id* val))
                   ((string-equal var "LDAPPER_GROUP") (setf *group-id* val))
                   ((string-equal var "LDAPPER_LOG_LEVEL")
                    (setf (v:repl-level) (cond ((string-equal "trace" val) :trace)
                                               ((string-equal "debug" val) :debug)
                                               ((string-equal "info" val) :info)
                                               ((string-equal "warn" val) :warn)
                                               ((string-equal "error" val) :error)
                                               ((string-equal "severe" val) :severe)
                                               (T (v:repl-level))))))))
      (with-open-file (stream file :if-does-not-exist NIL)
        (when stream
          (loop for line = (read-line stream NIL NIL)
                while line
                do (let ((pos (position #\= line)))
                     (when pos
                       (process-var (subseq line 0 pos) (subseq line (1+ pos))))))))
      (when listen
        (setf *ldap-servers* listen)))))

(defun read-envvars ()
  (macrolet ((maybe-set (var envvar &optional (transform 'identity))
               `(let ((var (envvar ,envvar)))
                  (when var (setf ,var (,transform var))))))
    (maybe-set *postgres-host* "LDAPPER_POSTGRES_HOST")
    (maybe-set *postgres-user* "LDAPPER_POSTGRES_USER")
    (maybe-set *postgres-pass* "LDAPPER_POSTGRES_PASS")
    (maybe-set *postgres-db* "LDAPPER_POSTGRES_DB")
    (maybe-set *base-dn* "LDAPPER_BASE_DN")
    (maybe-set *user-id* "LDAPPER_USER")
    (maybe-set *group-id* "LDAPPER_GROUP")
    (maybe-set *connection-timeout* "LDAPPER_CONNECTION_TIMEOUT" parse-integer)
    (let ((val (envvar "LDAPPER_LOG_LEVEL")))
      (when val
        (setf (v:repl-level) (cond ((string-equal "trace" val) :trace)
                                   ((string-equal "debug" val) :debug)
                                   ((string-equal "info" val) :info)
                                   ((string-equal "warn" val) :warn)
                                   ((string-equal "error" val) :error)
                                   ((string-equal "severe" val) :severe)
                                   (T (v:repl-level))))))
    (let ((val (envvar "LDAPPER_LISTEN")))
      (when val
        (setf *ldap-servers* (list (parse-listen-config val)))))))

(defun read-config ()
  (read-config-file "/etc/default/ldapper")
  (read-config-file "~/.config/ldapper/config")
  (read-envvars))

(defun print-config (&optional (stream *standard-output*))
  (when *postgres-host* (format stream "~&LDAPPER_POSTGRES_HOST=~a~%" *postgres-host*))
  (when *postgres-user* (format stream "~&LDAPPER_POSTGRES_USER=~a~%" *postgres-user*))
  (when *postgres-pass* (format stream "~&LDAPPER_POSTGRES_PASS=~a~%" *postgres-pass*))
  (when *postgres-db* (format stream "~&LDAPPER_POSTGRES_DB=~a~%" *postgres-db*))
  (when *base-dn* (format stream "~&LDAPPER_BASE_DN=~a~%" *base-dn*))
  (dolist (server *ldap-servers*)
    (destructuring-bind (host port &key ssl-certificate ssl-certificate-key ssl-certificate-password) server
      (format stream "~&LDAPPER_LISTEN=~a ~a~@[ ssl-cert=~a~]~@[ ssl-key=~a~]~@[ ssl-pass=~a~]~%"
              host port ssl-certificate ssl-certificate-key ssl-certificate-password))))
