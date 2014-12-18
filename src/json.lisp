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
                #+nil :association-list-p))
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
     (iter (for (key val) in-hashtable object)
       (collect (cons key
                      (convert-for-json val)))))
    (local-time:timestamp
     (local-time:timestamp-to-unix object))
    ((or structure-object
         standard-object)
     (iter (for (k v) on (convert-object object) by #'cddr)
       (collect (cons k (convert-for-json v)))))
    ((satisfies property-list-p)
     (iter (for (key val) on object by #'cddr)
       (collect (cons key
                      (convert-for-json val)))))
    ((satisfies association-list-p)
     (iter (for (key . val) in object)
       (collect (cons key
                      (convert-for-json val)))))
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
