(defpackage datafly.syntax
  (:use :cl)
  (:import-from :cl-annot)
  (:import-from :named-readtables
                :defreadtable
                :in-readtable)
  (:export :datafly-syntax
           :enable-syntax))
(in-package :datafly.syntax)

(defreadtable datafly-syntax
  (:merge :standard)
  (:macro-char #\@ #'cl-annot.syntax:annotation-syntax-reader))

(defmacro enable-syntax ()
  '(in-readtable datafly-syntax))
