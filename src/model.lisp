(in-package :cl-user)
(defpackage datafly.model
  (:use :cl
        :optima
        :iterate
        :sxql)
  (:shadowing-import-from :iterate
                          :for)
  (:import-from :datafly.db
                :retrieve-one
                :retrieve-all)
  (:import-from :datafly.cache
                :defcached-accessor
                :*model-accessors*)
  (:import-from :datafly.util
                :partition)
  (:import-from :alexandria
                :ensure-list
                :ensure-car
                :make-keyword))
(in-package :datafly.model)

(syntax:use-syntax :annot)

@export
(defmacro defmodel (name-and-options &body slot-descriptions)
  (let* ((doc (if (stringp (car slot-descriptions))
                  (car slot-descriptions)
                  nil))
         (slot-descriptions (if doc
                                (cdr slot-descriptions)
                                slot-descriptions))
         (slot-names (iter (for slot in slot-descriptions)
                       (collect (ensure-car slot))))
         (inflate (make-hash-table :test 'eq))
         (accessors '()))
    (destructuring-bind (name &rest options) (ensure-list name-and-options)
      (multiple-value-bind (inflate-options rest-options)
          (partition :inflate options :key #'car :test #'eq)
        (iter (for (key columns fn) in inflate-options)
          (iter (for column in (ensure-list columns))
            (setf (gethash column inflate) fn)))
        (setf options rest-options))
      (let ((conc-name (or (cadr (find :conc-name options :key #'car :test #'eq))
                           (intern (format nil "~A-" name)))))
        `(progn
           ,@(multiple-value-bind (has-a-options rest-options)
                 (partition :has-a options :key #'car :test #'eq)
               (setf options rest-options)
               (iter (for (has-a accessor-definition sxql) in has-a-options)
                 (ematch (ensure-list accessor-definition)
                   ((or (list slot-name)
                        (list slot-name model-class))
                    (let ((accessor (intern (format nil "~A~A" conc-name slot-name))))
                      (push accessor accessors)
                      (collect
                          `(defcached-accessor ,accessor (,name) (,name)
                             (apply #'retrieve-one
                                    (with-slots (,@slot-names) ,name
                                      (declare (ignorable ,@slot-names))
                                      (select :*
                                        (from ,(intern (substitute #\_ #\-
                                                                   (symbol-name (or model-class slot-name)))
                                                       :keyword))
                                        ,sxql
                                        (limit 1)))
                                    (and (find-class ',(or model-class slot-name) nil)
                                         '(:as ,(or model-class slot-name)))))))))))
           ,@(multiple-value-bind (has-many-options rest-options)
                 (partition :has-many options :key #'car :test #'eq)
               (setf options rest-options)
               (iter (for (has-many accessor-definition sxql) in has-many-options)
                 (ematch (ensure-list accessor-definition)
                   ((or (list slot-name)
                        (list slot-name model-class))
                    (let ((accessor (intern (format nil "~A~A" conc-name slot-name)))
                          (sxql-query (gensym "SXQL")))
                      (push accessor accessors)
                      (collect
                          `(defcached-accessor ,accessor (,name &key limit offset) (,name :limit limit :offset offset)
                             (apply #'retrieve-all
                                    (let ((,sxql-query (with-slots (,@slot-names) ,name
                                                         (declare (ignorable ,@slot-names))
                                                         ,sxql)))
                                      (setf (sxql.statement:select-statement-limit-clause ,sxql-query)
                                            (if limit
                                                (list (limit limit))
                                                nil))
                                      (setf (sxql.statement:select-statement-offset-clause ,sxql-query)
                                            (if offset
                                                (list (offset offset))
                                                nil))
                                      ,sxql-query)
                                    (and (find-class ',(or model-class slot-name) nil)
                                         '(:as ,(or model-class slot-name)))))))))))
           ,@(progn (setf (gethash name *model-accessors*) accessors) nil)
           (defstruct (,name
                       (:constructor ,(intern (format nil "~A~A"
                                                      (string :make-) name))
                           (&key ,@slot-names
                            &allow-other-keys
                              ,@(and (not (zerop (hash-table-count inflate)))
                                 `(&aux
                                     ,@(iter (for (column fn) in-hashtable inflate)
                                         (collect `(,column (funcall ,fn ,column))))))))
                       ,@options)
             ,@(when doc (list doc))
             ,@slot-descriptions))))))

@export
(defmacro model (defstruct-form)
  (ematch defstruct-form
    ((list* 'defstruct body)
     `(defmodel ,@body))))

@export
(defun object-to-plist (object)
  (let ((slots (c2mop:class-direct-slots (class-of object))))
    (iter (for slot in slots)
      (let ((slot-name (c2mop:slot-definition-name slot)))
        (collect (make-keyword slot-name))
        (collect (slot-value object slot-name))))))

@export
(defun copy-model (object)
  (let* ((class-name (class-name (class-of object)))
         (constructor-name (intern (format nil "~A~A"
                                           (string :make-) class-name)
                                   (symbol-package class-name))))
    (apply (symbol-function constructor-name)
           (object-to-plist object))))
