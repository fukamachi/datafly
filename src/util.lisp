(in-package :cl-user)
(defpackage datafly.util
  (:use :cl))
(in-package :datafly.util)

(syntax:use-syntax :annot)

@export
(defun partition-if (pred sequence &key from-end (start 0) end (key #'identity))
  (let ((yes nil)
        (no nil)
        (sequence (if (or (not (zerop start))
                          end)
                      (subseq sequence start end)
                      sequence)))
    (map nil
         #'(lambda (x)
             (if (funcall pred (funcall key x))
                 (push x yes)
                 (push x no)))
         (if from-end
             (nreverse sequence)
             sequence))
    (values yes no)))

@export
(defun partition (item sequence &key from-end (start 0) end (key #'identity) (test #'eql))
  (partition-if
   (lambda (x)
     (funcall test x item))
   sequence
   :from-end from-end
   :start start
   :end end
   :key key))
