(in-package :cl-user)
(defpackage datafly.json
  (:use :cl
        :iterate)
  (:import-from :cl-json
                #+nil :encode-json
                :encode-json-to-string)
  (:import-from :local-time
                :timestamp
                :timestamp-to-unix)
  (:import-from :trivial-types
                :property-list-p
                #+nil :association-list-p)
  (:import-from #:alexandria
                #:copy-hash-table))
(in-package :datafly.json)

(syntax:use-syntax :annot)

(defun association-list-p (object)
  (and (trivial-types:association-list-p object)
       (not (eq (caar object) (caadr object)))))

(defun object-to-plist (object)
  (let ((slots (c2mop:class-direct-slots (class-of object))))
    (iter (for slot in slots)
      (let ((slot-name (c2mop:slot-definition-name slot)))
        (collect slot-name)
        (collect (slot-value object slot-name))))))

@export
(defgeneric convert-object (object)
  (:method ((object structure-object))
    (object-to-plist object))
  (:method ((object standard-object))
    (object-to-plist object)))

(defun convert-for-json (object)
  (typecase object
    (hash-table
     (let ((hash (copy-hash-table object)))
       (iter (for (key val) in-hashtable hash)
         (setf (gethash key hash) (convert-for-json val)))
       hash))
    (local-time:timestamp
     (local-time:timestamp-to-unix object))
    ((or structure-object
         standard-object)
     (let ((hash (make-hash-table :test 'eq)))
       (iter (for slot in (c2mop:class-direct-slots (class-of object)))
         (let ((slot-name (c2mop:slot-definition-name slot)))
           (setf (gethash slot-name hash)
                 (convert-for-json (slot-value object slot-name)))))
       hash))
    (null nil)
    ((satisfies property-list-p)
     (let ((hash (make-hash-table :test 'eq)))
       (iter (for (key val) on object by #'cddr)
         (setf (gethash key hash) (convert-for-json val)))
       hash))
    ((satisfies association-list-p)
     (let ((hash (make-hash-table :test 'equal)))
       (iter (for (key . val) in object)
         (setf (gethash key hash) (convert-for-json val)))
       hash))
    ((or list simple-vector)
     (map 'simple-vector
          #'convert-for-json
          object))
    (T object)))

@export
(defgeneric encode-json (object &optional stream)
  (:method ((object t) &optional stream)
    (if stream
        (cl-json:encode-json (convert-for-json object) stream)
        (cl-json:encode-json-to-string (convert-for-json object)))))
