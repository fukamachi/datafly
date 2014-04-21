(in-package :cl-user)
(defpackage :datafly.db
  (:use :cl
        :iterate
        :sxql)
  (:import-from :dbi
                :prepare
                #+nil :execute
                :fetch-all
                :fetch
                :ping
                :connect
                :disconnect
                :connection-driver-type)
  (:import-from :alexandria
                :when-let))
(in-package :datafly.db)

(syntax:use-syntax :annot)

@export
(defvar *connection* nil)

(defvar *connections* (make-hash-table :test 'equal))

@export
(defvar *trace-sql* nil)

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

(defun convert-row (row &key as)
  (when-let ((row
              (iter (for (column value) on row by #'cddr)
                (collect (convert-column-name column))
                (collect (convert-column-value value)))))
    (if as
        (let* ((class (find-class as))
               (class-name (class-name class))
               (class-package (symbol-package class-name))
               (constructor (intern (format nil "~A~A" #.(string :make-) class-name)
                                    class-package)))
          (apply (symbol-function constructor) row))
        row)))

(defun execute-with-connection (conn statement)
  (check-type conn dbi.driver:<dbi-connection>)
  (multiple-value-bind (sql params)
      (let ((sxql:*quote-character* (or sxql:*quote-character*
                                        (connection-quote-character *connection*))))
        (sxql:yield statement))
    (when *trace-sql*
      (format t "~&~A (~{~S~^, ~})~%" sql params))
    (let ((prepared (dbi:prepare conn sql)))
      (apply #'dbi:execute prepared params))))

@export
(defun retrieve-one (statement &key by = as)
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
   (dbi:fetch (execute-with-connection *connection* statement))
   :as as))

@export
(defun retrieve-one-value (statement &optional key)
  (if key
      (getf (retrieve-one statement) key)
      (second (retrieve-one statement))))

@export
(defun retrieve-all (statement &key as)
  (mapcar (lambda (row)
            (convert-row row :as as))
          (dbi:fetch-all (execute-with-connection *connection* statement))))

@export
(defun retrieve-all-values (statement &optional key)
  (mapcar (if key
              (lambda (row) (getf row key))
              #'second)
          (retrieve-all statement)))

@export
(defun execute (statement)
  (execute-with-connection *connection* statement))
