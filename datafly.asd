(defsystem "datafly"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ("iterate"
               "optima"
               "trivial-types"
               "closer-mop"
               "named-readtables"
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
                ((:file "datafly" :depends-on ("model" "db" "cache" "logger" "inflate" "json" "syntax"))
                 (:file "model" :depends-on ("db" "cache" "syntax" "util"))
                 (:file "db" :depends-on ("logger" "syntax"))
                 (:file "cache" :depends-on ("syntax"))
                 (:file "logger" :depends-on ("syntax"))
                 (:file "inflate" :depends-on ("db" "syntax"))
                 (:file "json" :depends-on ("syntax"))
                 (:file "syntax")
                 (:file "util"))))
  :description "Lightweight database library."
  :long-description
  #.(read-file-string (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "datafly-test"))))
