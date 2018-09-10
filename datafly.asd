(defsystem "datafly"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ("iterate"
               "optima"
               "trivial-types"
               "closer-mop"
               "cl-syntax-annot"
               "sxql"
               "dbi"
               "alexandria"
               "babel"
               "local-time"
               "function-cache"
               "jonathan"
               "kebab"
               "log4cl")
  :components ((:module "src"
                :components
                ((:file "datafly" :depends-on ("model" "db" "cache" "logger" "inflate" "json"))
                 (:file "model" :depends-on ("db" "cache" "util"))
                 (:file "db" :depends-on ("logger"))
                 (:file "cache")
                 (:file "logger")
                 (:file "inflate" :depends-on ("db"))
                 (:file "json")
                 (:file "util"))))
  :description "Lightweight database library."
  :long-description
  #.(read-file-string (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "datafly-test"))))
