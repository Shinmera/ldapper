(asdf:defsystem ldapper
  :version "1.0.3"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A simple LDAP server for user accounts."
  :homepage "https://shinmera.github.io/ldapper"
  :bug-tracker "https://github.com/shinmera/ldapper/issues"
  :source-control (:git "https://github.com/shinmera/ldapper.git")
  :build-operation "program-op"
  :build-pathname "ldapper"
  :entry-point "org.shirakumo.ldapper::main"
  :serial T
  :components ((:file "package")
               (:file "conditions")
               (:file "database")
               (:file "ldif")
               (:file "ldap")
               (:file "commands")
               (:file "results")
               (:file "server")
               (:file "protocol")
               (:file "config")
               (:file "main"))
  :depends-on (:usocket
               :postmodern
               :cl+ssl
               :verbose
               :crypto-shortcuts
               :lparallel
               :cl-ppcre
               :babel
               :alexandria
               :trivial-signal))
