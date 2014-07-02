#|
  This file is a part of datafly project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

#|
  Lightweight database library.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage datafly-asd
  (:use :cl :asdf))
(in-package :datafly-asd)

(defsystem datafly
  :version "0.1"
  :author "Eitarow Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:iterate
               :optima
               :trivial-types
               :closer-mop
               :cl-syntax-annot
               :sxql
               :dbi
               :alexandria
               :babel
               :local-time
               :function-cache)
  :components ((:module "src"
                :components
                ((:file "datafly" :depends-on ("model" "db" "cache" "inflate"))
                 (:file "model" :depends-on ("db" "cache" "util"))
                 (:file "db")
                 (:file "cache")
                 (:file "inflate" :depends-on ("db"))
                 (:file "util"))))
  :description "Lightweight database library."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op datafly-test))))
