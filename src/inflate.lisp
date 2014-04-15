(in-package :cl-user)
(defpackage datafly.inflate
  (:use :cl)
  (:import-from :datafly.db
                :database-type)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :local-time
                :timestamp
                :universal-to-timestamp
                :timestamp-subtimezone
                :parse-timestring
                :*default-timezone*)
  (:import-from :babel
                :octets-to-string))
(in-package :datafly.inflate)

(syntax:use-syntax :annot)

@export
(defun tinyint-to-boolean (val)
  (ecase val
    ('nil nil)
    (0 nil)
    (1 t)))

@export
(defun datetime-to-timestamp (val)
  (etypecase val
    (null nil)
    (local-time:timestamp val)
    (integer
     (let ((ts (universal-to-timestamp val)))
       ;; KLUDGE: CL-MYSQL converts DATETIME strings with UTC timezone.
       (if (eq (database-type) :mysql)
           (universal-to-timestamp
            (- val (timestamp-subtimezone ts *default-timezone*)))
           ts)))
    (string (parse-timestring val))))

@export
(defun unixtime-to-timestamp (val)
  (etypecase val
    (null nil)
    (local-time:timestamp val)
    (integer
     (local-time:unix-to-timestamp val))))

@export
(defun string-to-keyword (val)
  (etypecase val
    (null nil)
    (keyword val)
    ((or symbol string) (make-keyword (string-upcase val)))))

@export
(defun octet-vector-to-string (val)
  (etypecase val
    (null nil)
    ((vector (unsigned-byte 8)) (babel:octets-to-string val))))
