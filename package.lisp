(defpackage #:org.shirakumo.ldapper
  (:use #:cl)
  (:local-nicknames (#:v #:org.shirakumo.verbose))
  ;; conditions.lisp
  (:export
   #:ldapper-condition
   #:ldapper-error
   #:no-such-account
   #:name
   #:authentication-failed
   #:permission-denied
   #:attribute-required
   #:unknown-command)
  ;; database.lisp
  (:export
   #:*postgres-db*
   #:*postgres-user*
   #:*postgres-pass*
   #:*postgres-host*
   #:connect
   #:disconnect
   #:list-accounts
   #:find-account
   #:ensure-account
   #:account-admin-p
   #:search-accounts
   #:filter-accounts
   #:authenticate
   #:make-account
   #:insert-account
   #:edit-account
   #:delete-account)
  ;; ldif.lisp
  (:export
   #:parse-ldif
   #:ldap-record->account
   #:account->ldap-record
   #:account->ldif-text
   #:import-from-ldif)
  ;; ldap.lisp
  (:export)
  ;; commands.lisp
  (:export)
  ;; results.lisp
  (:export)
  ;; server.lisp
  (:export
   #:*base-dn*
   #:*ldap-servers*
   #:*workers*
   #:start
   #:stop)
  ;; protocol.lisp
  (:export)
  ;; config.lisp
  (:export
   #:read-config-file
   #:read-envvars
   #:read-config
   #:print-config))
