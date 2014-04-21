(in-package :cl-user)
(defpackage datafly.cache
  (:use :cl
        :iterate
        :function-cache)
  (:import-from :alexandria
                :compose))
(in-package :datafly.cache)

(syntax:use-syntax :annot)

(defvar *model-accessors* (make-hash-table :test 'eq))

@export
(defmacro defcached-accessor (name args cache-args &body body)
  (let ((cache (intern (format nil "*~A-~A*" name :cache))))
    `(progn
       (defcached ,name (,@args)
         ,@body)
       (defun (setf ,name) (new ,@args)
         (setf (get-cached-value ,cache (list ,@cache-args)) (list new))))))

(defun accessor-cache (accessor)
  (let ((cache-symbol (intern (format nil "*~A-~A*" accessor :cache))))
    (when (boundp cache-symbol)
      (symbol-value cache-symbol))))

@export
(defun clear-model-caches (model &optional accessor)
  (check-type model symbol)
  (check-type accessor symbol)
  (mapc (compose #'clear-cache #'accessor-cache)
        (if accessor
            (list accessor)
            (gethash model *model-accessors*))))

@export
(defun clear-object-caches (object &optional accessor)
  (let ((caches (mapcar #'accessor-cache
                        (if accessor
                            (list accessor)
                            (gethash (class-name (class-of object)) *model-accessors*)))))
    (iter (for cache in caches)
      (when cache
        (iter (for (key) in-hashtable (cached-results cache))
          (when (eq (car key) object)
            (clear-cache cache key)))))))
