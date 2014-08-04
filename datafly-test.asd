#|
  This file is a part of datafly project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage datafly-test-asd
  (:use :cl :asdf))
(in-package :datafly-test-asd)

(defsystem datafly-test
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:datafly
               :cl-test-more
               :sxql)
  :components ((:module "t"
                :components
                ((:test-file "datafly")
                 (:test-file "json"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
