(defsystem "datafly-test"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :defsystem-depends-on ("prove-asdf")
  :depends-on ("datafly"
               "prove"
               "sxql")
  :components ((:module "t"
                :components
                ((:test-file "datafly")
                 (:test-file "json"))))
  :perform (test-op (op c)
             (symbol-call :prove :run-test-system c)))
