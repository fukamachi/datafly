(in-package :cl-user)
(defpackage :datafly.db
  (:use :cl
        :iterate
        :sxql)
  (:shadowing-import-from :iterate
                          :for)
  (:import-from :datafly.logger
                :*sql-logger*
                :*trace-sql*)
  (:import-from :dbi
                :prepare
                #+nil :execute
                :fetch-all
                :fetch
                :ping
                :connect
                :disconnect
                :connection-driver-type)
  (:import-from :trivial-types
                :property-list
                :association-list))
(in-package :datafly.db)

(syntax:use-syntax :annot)

@export
(defvar *connection* nil)

(defvar *connections* (make-hash-table :test 'equal))

@export
(defvar *default-row-type* 'property-list)

@export
(defun database-type ()
  (connection-driver-type *connection*))

@export
(defun connect-cached (&rest connect-args)
  (let ((conn (gethash connect-args *connections*)))
    (cond
      ((null conn)
       (setf (gethash connect-args *connections*)
             (apply #'dbi:connect
                    connect-args)))
      ((not (dbi:ping conn))
       (dbi:disconnect conn)
       (remhash connect-args *connections*)
       (apply #'connect-cached connect-args))
      (T conn))))

@export
(defun connect-toplevel (&rest connect-args)
  (when *connection*
    (error "Connection is already established in toplevel."))
  (setf *connection*
        (apply #'connect-cached connect-args)))

@export
(defun disconnect-toplevel ()
  (when *connection*
    (dbi:disconnect *connection*)
    (setf *connection* nil)))

(defun connection-quote-character (conn)
  (case (connection-driver-type conn)
    (:mysql #\`)
    (:postgres #\")
    (:sqlite3 #\")
    (T nil)))

(defun convert-column-name (name)
  (intern (substitute #\- #\_
                      (string-upcase (symbol-name name)))
          :keyword))

(defun convert-column-value (value)
  (if (eq value :null)
      nil
      value))

(defun convert-row (row &key (as *default-row-type*) (prettify t))
  (flet ((prettify-column-name (name)
           (if prettify
               (convert-column-name name)
               name)))
    (when row
      (case as
        ((null property-list)
         (iter (for (column value) on row by #'cddr)
           (collect (prettify-column-name column))
           (collect (convert-column-value value))))
        (association-list
         (iter (for (column value) on row by #'cddr)
           (collect (cons (prettify-column-name column)
                          (convert-column-value value)))))
        (hash-table
         (let ((hash (make-hash-table :test 'eq)))
           (iter (for (column value) on row by #'cddr)
             (setf (gethash (prettify-column-name column) hash)
                   (convert-column-value value)))
           hash))
        (T
         (let* ((class (find-class as))
                (class-name (class-name class))
                (class-package (symbol-package class-name))
                (constructor (intern (format nil "~A~A" #.(string :make-) class-name)
                                     class-package))
                (row (iter (for (column value) on row by #'cddr)
                       (collect (prettify-column-name column))
                       (collect (convert-column-value value)))))
           (apply (symbol-function constructor) row)))))))

(defun get-prev-stack ()
  #+sbcl
  (let (prev-stack)
    (sb-debug::map-backtrace
     (lambda (frame)
       (let ((fun (sb-di::debug-fun-name (sb-di::frame-debug-fun frame))))
         (when (and (null prev-stack)
                    (symbolp fun)
                    (not (find (package-name (symbol-package fun))
                               (list :common-lisp :datafly.db :function-cache)
                               :test #'string=)))
           (setf prev-stack fun)))))
    prev-stack))

(defun execute-with-connection (conn statement)
  (check-type conn dbi.driver:<dbi-connection>)
  (multiple-value-bind (sql params)
      (let ((sxql:*quote-character* (or sxql:*quote-character*
                                        (connection-quote-character *connection*))))
        (typecase statement
          (string (values statement nil))
          (otherwise (sxql:yield statement))))
    (let* ((prepared (dbi:prepare conn sql))
           (results (dbi:fetch-all (apply #'dbi:execute prepared params))))
      (when *trace-sql*
        (let ((stack (get-prev-stack)))
          (log:trace :logger *sql-logger*
                     "~A (~{~S~^, ~}) [~D row~:P]~:[~;~:* | ~S~]" sql params (length results) stack)))
      results)))

@export
(defun retrieve-one (statement &key by = (as *default-row-type*) (prettify t))
  (assert (eq (null by) (null =)))
  (when (keywordp statement)
    (setf statement
          (select :*
            (from statement)
            (if (and by =)
                (where (:= by =))
                nil)
            (limit 1))))
  (convert-row
   (first (execute-with-connection *connection* statement))
   :as as
   :prettify prettify))

@export
(defun retrieve-one-value (statement &optional key (prettify t))
  (declare (ignorable prettify))
  (if key
      (getf (retrieve-one statement :prettify prettify) key)
      (second (retrieve-one statement))))

@export
(defun retrieve-all (statement &key (as *default-row-type*) (prettify t))
  (mapcar (lambda (row)
            (convert-row row :as as :prettify prettify))
          (execute-with-connection *connection* statement)))

@export
(defun retrieve-all-values (statement &optional key (prettify t))
  (mapcar (if key
              (lambda (row) (getf row key))
              #'second)
          (retrieve-all statement :prettify prettify)))

@export
(defun execute (statement)
  (execute-with-connection *connection* statement)
  (values))
