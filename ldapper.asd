(asdf:defsystem ldapper
  :components ((:file "package")
               (:file "database")
               (:file "ldif")
               (:file "protocol")
               (:file "server"))
  :depends-on (:usocket
               :postmodern
               :trivial-ldap
               :cl+ssl
               :verbose
               :lparallel))
