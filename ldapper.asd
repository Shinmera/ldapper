(asdf:defsystem ldapper
  :components ((:file "package")
               (:file "database")
               (:file "ldif")
               (:file "protocol")
               (:file "server")
               (:file "config"))
  :depends-on (:usocket
               :postmodern
               :trivial-ldap
               :cl+ssl
               :verbose
               :crypto-shortcuts
               :lparallel
               :cl-ppcre))
