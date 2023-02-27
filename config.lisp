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
      (list host (parse-integer port) :ssl-certificate (merge-pathnames ssl-cert file)
                                      :ssl-certificate-key (merge-pathnames ssl-key file)
                                      :ssl-certificate-password ssl-pass))))

(defun read-config-file (file)
  (let ((listen ()))
    (flet ((process-var (var val)
             (cond ((string-equal var "LDAPPER_POSTGRES_HOST") (setf *postgres-host* val))
                   ((string-equal var "LDAPPER_POSTGRES_USER") (setf *postgres-user* val))
                   ((string-equal var "LDAPPER_POSTGRES_PASS") (setf *postgres-pass* val))
                   ((string-equal var "LDAPPER_POSTGRES_DB") (setf *postgres-db* val))
                   ((string-equal var "LDAPPER_LISTEN") (push (parse-listen-config val file) listen)))))
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
  (macrolet ((maybe-set (var envvar)
               `(let ((var (envvar ,envvar)))
                  (when var (setf ,var var)))))
    (maybe-set *postgres-host* "WG_POSTGRES_HOST")
    (maybe-set *postgres-user* "WG_POSTGRES_USER")
    (maybe-set *postgres-pass* "WG_POSTGRES_PASS")
    (maybe-set *postgres-db* "WG_POSTGRES_DB")
    (let ((listen (envvar "LDAPPER_LISTEN")))
      (when listen
        (setf *ldap-servers* (list (parse-listen-config listen)))))))

(defun read-config ()
  (read-config-file "/etc/default/ldapper")
  (read-config-file "~/.config/ldapper/config")
  (read-envvars))
