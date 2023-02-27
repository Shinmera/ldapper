(asdf:defsystem ldapper
  :build-operation "program-op"
  :build-pathname "ldapper"
  :entry-point "org.shirakumo.ldapper::main"
  :components ((:file "package")
               (:file "database")
               (:file "ldif")
               (:file "protocol")
               (:file "server")
               (:file "config")
               (:file "main"))
  :depends-on (:usocket
               :postmodern
               :trivial-ldap
               :cl+ssl
               :verbose
               :crypto-shortcuts
               :lparallel
               :cl-ppcre))
