(in-package :cl-user)
(defpackage datafly.model
  (:use :cl
        :optima
        :iterate
        :sxql)
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
               (iter (for (has-a table where) in has-a-options)
                 (ematch (ensure-list table)
                   ((or (list table)
                        (list slot-name table))
                    (let ((accessor (intern (format nil "~A~A" conc-name (or slot-name table)))))
                      (push accessor accessors)
                      (collect
                          `(defcached-accessor ,accessor (,name) (,name)
                             (apply #'retrieve-one
                                    (select :*
                                      (from ,(make-keyword (substitute #\_ #\- (string-upcase table))))
                                      (with-slots (,@slot-names) ,name
                                        (declare (ignorable ,@slot-names))
                                        ,where)
                                      (limit 1))
                                    (and (find-class ',table nil)
                                         '(:as ,table))))))))))
           ,@(multiple-value-bind (has-many-options rest-options)
                 (partition :has-many options :key #'car :test #'eq)
               (setf options rest-options)
               (iter (for (has-many table where order-by) in has-many-options)
                 (ematch (ensure-list table)
                   ((or (list table)
                        (list slot-name table))
                    (let ((accessor (intern (format nil "~A~A" conc-name (or slot-name table)))))
                      (push accessor accessors)
                      (collect
                          `(defcached-accessor ,accessor (,name &key limit offset) (,name :limit limit :offset offset)
                             (apply #'retrieve-all
                                    (select :*
                                      (from ,(make-keyword (substitute #\_ #\- (string-upcase table))))
                                      (with-slots (,@slot-names) ,name
                                        (declare (ignorable ,@slot-names))
                                        ,where)
                                      ,@(and order-by
                                             `((with-slots (,@slot-names) ,name
                                                 (declare (ignorable ,@slot-names))
                                                 ,order-by)))
                                      (and limit
                                           (limit limit))
                                      (and offset
                                           (offset offset)))
                                    (and (find-class ',table nil)
                                         '(:as ,table))))))))))
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
