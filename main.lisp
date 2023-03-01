(in-package #:org.shirakumo.ldapper)

(defun main ()
  (read-config)
  (handler-case
      (destructuring-bind (self &optional (command "help") &rest args) sb-ext:*posix-argv*
        (cond ((string-equal command "start")
               (start))
              ((string-equal command "stop")
               (error "Not implemented lol"))
              ((string-equal command "list")
               (dolist (account (list-accounts)) (account->ldif-text account :output *standard-output* :trusted T)))
              ((string-equal command "import")
               (let ((add-args ()) (file (pop args)))
                 (loop for (key val) on args by #'cddr
                       do (cond ((string-equal key "--dry-run") (setf (getf add-args :dry-run) (string-equal val "true")))
                                ((string-equal key "--require") (push val (getf add-args :required-attributes)))
                                ((string-equal key "--ignore") (push val (getf add-args :ignored-attributes)))
                                (T (error "Unknown key argument ~a" key))))
                 (let ((accounts (apply #'import-from-ldif file add-args)))
                   (dolist (account accounts) (account->ldif-text account :output *standard-output* :trusted T)))))
              ((string-equal command "add")
               (let ((add-args ()) (name (pop args)) (mail (pop args)))
                 (unless name (error "NAME required"))
                 (unless mail (error "MAIL required"))
                 (loop for (key val) on args by #'cddr
                       do (cond ((string-equal key "--note") (setf (getf add-args :note) val))
                                ((string-equal key "--real-name") (setf (getf add-args :real-name) val))
                                ((string-equal key "--password") (setf (getf add-args :password) val))
                                ((string-equal key "--class") (push val (getf add-args :classes)))
                                ((string-equal key "--attribute") (push (cl-ppcre:split "=" val :limit 2) (getf add-args :attributes)))
                                (T (error "Unknown key argument ~a" key))))
                 (let ((peer (apply #'make-account name mail add-args)))
                   (format *standard-output* "~{~a: ~a~%~}" peer))))
              ((string-equal command "remove")
               (delete-account (or (first args) (error "NAME required"))))
              ((string-equal command "install")
               (let ((unit "ldapper") (start T) (enable T))
                 (loop for (key val) on args by #'cddr
                       do (cond ((string-equal key "--unit") (setf unit val))
                                ((string-equal key "--start") (setf start (string-equal val "true")))
                                ((string-equal key "--enable") (setf enable (string-equal val "true")))
                                (T (error "Unknown key argument ~a" key))))
                 (v:info :ldapper "Installing ~a" unit)
                 (with-open-file (stream (format NIL "/etc/systemd/system/~a.service" unit) :direction :output)
                   (format stream "[Unit]
Description=LDAP Server
Requires=network.target
After=network.target

[Service]
ExecStart=~a start
Restart=on-failure
RestartSec=5s

[Install]
WantedBy=multi-user.target
" (truename self)))
                 (v:info :ldapper "Creating config at ~a" "/etc/default/ldapper")
                 (with-open-file (stream "/etc/default/ldapper" :direction :output :if-exists NIL)
                   (when stream (print-config stream)))
                 (when start (uiop:run-program (list "systemctl" "start" unit)))
                 (when enable (uiop:run-program (list "systemctl" "enable" unit)))))
              ((string-equal command "config")
               (print-config *standard-output*))
              ((string-equal command "help")
               (format *error-output* "Usage: ~a [command] ...

Command can be:
  start  --- Start the ldap server
  stop   --- Stop the ldap server
  list   --- List known accounts in LDIF format
  import --- Import accounts from an LDIF file
    FILE                 --- The path of the LDIF file to import.
    --dry-run BOOLEAN    --- Whether to print the results only. [true]
    --require ATTRIBUTE  --- Specify a necessary attribute for an
                             account. May be specified multiple times.
                             [cn mail userPassword]
    --ignore ATTRIBUTE   --- Ignore an attribute and omit it from the
                             saved attribute list. May be specified
                             multiple times.
                             [cn dn mail userPassword givenName sn
                              gecos note objectClass 
                              structuralObjectClass]
  add    --- Add a new account. Prints the account info on completion
    NAME                 --- The name of the account
    MAIL                 --- The email address of the account
    --password PASS      --- The password for the account's login
    --real-name NAME     --- The \"real name\" of the account holder
    --note NOTE          --- An optional note about the account
    --class CLASS        --- Add an object class, can be specified
                             multiple times
    --attribute KEY=VAL  --- Add an object attribute, can be specified
                             multiple times
  remove --- Remove an account
    NAME                 --- The name of the account to remove
  passwd --- Change the password of an account
    NAME                 --- Will prompt for the password on STDIN
  install --- Install a basic server setup with systemd
    --unit UNIT          --- The service unit name to use [ldapper]
    --start BOOLEAN      --- Whether to start the service [true]
    --enable BOOLEAN     --- Whether to enable the service [true]
  config  --- Print the current configuration
  help    --- Show this help

The following configuration variables exist:

  LDAPPER_POSTGRES_HOST       --- The hostname of the postgres server
                                  [127.0.0.1]
  LDAPPER_POSTGRES_USER       --- The user to connect to postgres with
                                  [ldap]
  LDAPPER_POSTGRES_PASS       --- The password of the postgres user
  LDAPPER_POSTGRES_DB         --- The postgres database to use [ldap]
  LDAPPER_BASE_DN             --- The base domain name to be used.
  LDAPPER_LISTEN              --- Can be specified multiple times to
                                  specify servers must be in the
                                  following format, where FILE may be
                                  relative to the configuration file.
    HOST PORT [ssl-cert=FILE] [ssl-key=FILE] [ssl-pass=PASS]

The variables are first read from a file at /etc/default/ldapper
Then from $HOME/.config/ldapper/config
Then from environment variables
" self))
              (T (error "Unknown command ~s" command))))
    (sb-sys:interactive-interrupt ()
      (v:info :ldapper "Exiting from interrupt")
      (sb-ext:exit :code 0))
    (error (e)
      (v:error :ldapper "Error: ~a" e)
      (sb-ext:exit :code 1))))
