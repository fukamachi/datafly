(in-package :cl-user)
(defpackage datafly.cache
  (:use :cl
        :function-cache)
  (:import-from :alexandria
                :compose))
(in-package :datafly.cache)

(syntax:use-syntax :annot)

(defvar *model-cache* (make-hash-table :test 'eq))

@export
(defmacro defcached-accessor (name args cache-args &body body)
  (let ((cache (intern (format nil "*~A-~A*" name :cache))))
    `(progn
       (defcached ,name (,@args)
         ,@body)
       (defun (setf ,name) (new ,@args)
         (setf (get-cached-value ,cache (list ,@cache-args)) (list new))))))

@export
(defun clear-model-caches (model &optional accessor)
  (check-type model symbol)
  (check-type accessor symbol)
  (flet ((cache-val (accessor)
           (symbol-value (intern (format nil "*~A-~A*" accessor :cache)))))
    (mapc (compose #'clear-cache #'cache-val)
          (if accessor
              (list accessor)
              (gethash model *model-cache*)))))
