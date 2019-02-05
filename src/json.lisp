(in-package :cl-user)
(defpackage datafly.json
  (:use :cl
        :iterate)
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
  (and (listp object)
       (listp (car object))
       (destructuring-bind (a . b) (car object)
	 (not (listp b)))))

(defun property-list-p (object)
  (and (listp object)
       (not (null object))
       (listp (cdr object))
       (not (null (cdr object)))
       (destructuring-bind (a b &rest r) object
	 (keywordp a))))

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
  (flet ((convert-key (key)
           (kebab:to-camel-case (princ-to-string key))))
    (typecase object
      (hash-table
       (jojo:with-object
         (iter (for (key val) in-hashtable object)
           (jojo:write-key (convert-key key))
           (jojo:%write-char #\:)
           (convert-for-json val))))
      (local-time:timestamp
       (jojo:%to-json (local-time:timestamp-to-unix object)))
      ((or structure-object
           standard-object)
       (jojo:with-object
         (iter (for slot in (c2mop:class-direct-slots (class-of object)))
           (let ((slot-name (c2mop:slot-definition-name slot)))
             (jojo:write-key (convert-key slot-name))
             (jojo:%write-char #\:)
             (convert-for-json (slot-value object slot-name))))))
      (null (jojo:%to-json :null))
      ((satisfies property-list-p)
       (jojo:with-object
         (iter (for (key val) on object by #'cddr)
           (jojo:write-key (convert-key key))
           (jojo:%write-char #\:)
           (convert-for-json val))))
      ((satisfies association-list-p)
       (jojo:with-object
         (iter (for (key . val) in object)
           (jojo:write-key (convert-key key))
           (jojo:%write-char #\:)
           (convert-for-json val))))
      (list
       (jojo:%write-char #\[)
       (loop with first = t
             for o in object
             do (if first
                    (setf first nil)
                    (jojo:%write-char #\,))
                (convert-for-json o))
       (jojo:%write-char #\]))
      ((and vector (not string))
       (jojo:%write-char #\[)
       (loop with first = t
             for o across object
             do (if first
                    (setf first nil)
                    (jojo:%write-char #\,))
                (convert-for-json o))
       (jojo:%write-char #\]))
      (keyword (let ((*print-case* :downcase))
                 (jojo:%to-json (princ-to-string object))))
      (t (jojo:%to-json object)))))

@export
(defgeneric encode-json (object &optional stream)
  (:method ((object t) &optional stream)
    (if stream
        (jojo:with-output (stream)
          (convert-for-json object))
        (jojo:with-output-to-string* (convert-for-json object)))))
